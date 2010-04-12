;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/

(ns download
  (:use :reload aux agent)
  (:require [clojure.contrib.logging :as log]
            [clojure.contrib.duck-streams :as duck]

            fn
            service.cod.data.account
            progress)
  (:import (java.io File
                    FileOutputStream
                    InputStream
                    InterruptedIOException)

           (java.net ConnectException)

           (org.apache.commons.httpclient URI
                                          HttpClient
                                          HttpStatus
                                          ConnectTimeoutException 
                                          NoHttpResponseException)

           (org.apache.commons.httpclient.methods GetMethod
                                                  HeadMethod)

           (org.apache.commons.httpclient.params HttpMethodParams)

           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'download)

(defn match-service [line services]
  (some (fn [[service {address-pattern :address}]]
          (when-let [address (re-find address-pattern line)]
            {:service service :address address}))
        services))

(def *services*
     {::data.cod.ru
      {:address #"http://[\w\.\-]*.data.cod.ru/\d+"
       ;; :link (URI. (re-find #"http://files[-\d\w\.]*data.cod.ru/[^\"]+" page) true)
       ;; :name (second (re-find #"<b title=\".*\">(.*)</b>" page))
       :hosts #{#"files3?.dsv.*.data.cod.ru" #"files2.dsv.*.data.cod.ru"}}

      ::77.35.112.8*
      {:address #"http://77.35.112.8[1234]/.+"}

      ::dsvload.net.ftpupload
      {:address #"http://dsvload.net/ftpupload/.+"}})

(derive ::agent :agent/agent)

(defprotocol Downloading
  (status [d])
  (status! [d new-status])
  (status? [d current-status])

  (dead? [d])
  (alive? [d])
  (run? [d])
  (does-nothing? [d])
  (ok? [d])
  (idle? [d])
  (fail? [d])

  (pass [d])
  (idle! [d])
  (fail! [d]) 
  (die! [d])

  (services [d])
  
  (out-of-space-on-work-path? [d])
  (out-of-space-on-done-path? [d])
  (fully-loaded? [d])
  (already-on-done-path? [d])

  (execute [d action]))
  
(deftype Download [status-atom services- service
                   address working-path done-path 
                   host link file length]

  clojure.lang.IPersistentMap
  
  Downloading
  (status [this] @status-atom)

  (status!
   [this new-status]
   {:pre [(({:idle    #{:running}
             :fail    #{:running}
             :dead    #{}
             :running #{:stoping :idle :fail :dead}
             :stoping #{:idle :fail :dead}}
            (status this)) new-status)]}
   (with-return this
     (reset! status-atom new-status)))

  (status?
   [this current-status]
   (cond (keyword? current-status) (= (status this) current-status)
         (set? current-status) (keyword? (current-status (status this)))))

  (dead? [this] (status? this :dead))
  
  (alive? [this] (not (dead? this)))

  (run? [this] (status? this #{:running :stoping}))
  
  (does-nothing? [this])
  
  (ok? [this] (status? this #{:idle :fail}))
  
  (idle? [this])
  
  (fail? [this])

  (pass [this] this)

  (idle! [this] (status! this :idle))

  (fail! [this] (status! this :fail))

  (die! [this] (status! this :dead))

  (services [this] @services-)

  (out-of-space-on-work-path?
   [this]
   (when (and working-path length file)
     (< (.getUsableSpace working-path)
        (- length (file-length file)))))

  (out-of-space-on-done-path?
   [this]
   (when (and done-path length)
     (< (.getUsableSpace done-path) length)))

  (fully-loaded?
   [this]
   (when (and length file)
     (<= length (file-length file))))

  (already-on-done-path?
   [this]
   (when (and done-path file)
     (.exists (File. done-path (.getName file)))))

  (execute
   [this action]
   (try (status! this :running)
        (let [fag (future (action this))]
          (try (with-deref [fag]
                 (cond (run? fag) (idle! fag)
                       :else fag))
               (catch Exception _ (fail! this))))
        (catch AssertionError _ this))))

(defn make-download-agent
  [address-line & {:as opts :keys [services working-path done-path]
                   :or {services *services*}}]

  (let [{:keys [service address]}
        (when address-line (match-service address-line services))]

    (when (and service address)
      (make-agent {:status (atom :idle)
                   :service service
                   :services (delay services)
                   :address (URI. address)
                   :working-path working-path
                   :done-path done-path
                   :host nil
                   :link nil
                   :file nil
                   :length nil}
                  :tag ::agent))))

(defmulti run  type)
(defmulti stop type)

(defmulti sleep    :service)
(defmulti get-link :service)
(defmulti get-name :service)
(defmulti get-tag  :service)
(defmulti get-file :service)
(defmulti get-length :service)
(defmulti move-to-done-path :service)
(defmulti download :service)

(def buffer-size 4096)
(def timeout-after-fail   3000)
(def connection-timeout   30000)
(def head-request-timeout 30000)
(def get-request-timeout  30000)

(defmethod sleep :default [ag millis]
  (with-return ag
    (Thread/sleep millis)))

(defmethod get-link :default [{:as ag :keys [address]}]
  (idle (assoc ag :link address)))

(defmethod get-link ::data.cod.ru [{:as ag :keys [address]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str address))]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout))
    (.. get getParams (setSoTimeout get-request-timeout))
    (try (let [status (.executeMethod client get)]
           (if (= status HttpStatus/SC_OK)
             (let [parsed (service.cod.data.account/parse-page
                           (duck/slurp* (.getResponseBodyAsStream get)))]
               (if (and (parsed :name) (parsed :link))
                 (idle (assoc ag :name (parsed :name) :link (parsed :link)))
                 (do (log/info (str "Невозможно получить имя файла и ссылку с адреса " address))
                     (die ag))))
             (die ag)))
         (catch ConnectException e
           (do (log/info (str "Время ожидания соединения с сервером истекло для " address))
               (fail ag)))
         (catch ConnectTimeoutException e
           (do (log/info (str "Время ожидания соединения с сервером истекло для " address))
               (fail ag)))
         (catch InterruptedIOException e
           (do (log/info (str "Время ожидания ответа сервера истекло для " address))
               (fail ag)))
         (catch NoHttpResponseException e
           (do (log/info (str "Сервер не отвечает на запрос для " address))
               (fail ag)))
         (catch Exception e 
           (do (log/info (str "Ошибка во время получения имени файла и ссылки с адреса " address))
               (fail ag)))
         (finally (.releaseConnection get)))))
 
(defmethod get-name :default [{:as ag :keys [link]}]
  (idle (assoc ag :name (second (re-find #"/([^/]+)$" (.getPath link))))))

(defmethod get-name ::data.cod.ru [ag]
  (get-link ag))

(defmethod move-to-done-path :default [{:as ag :keys [#^File file, #^File done-path]}]
  (if-let [moved (move-file file done-path)]
    (idle (assoc ag :file moved))
    (die ag)))

(defmethod get-tag :default [{:as ag :keys [link service services]}]
  (let [tags (seq (-> services force service :tags))
        tag (or (when tags
                  (some (fn [tag] (when (re-find tag (str link))
                                    (str tag)))
                        tags))
                (.getHost link))]
    (if tag
      (idle (assoc ag :tag tag))
      (die ag))))

(defmethod get-length :default [{:as ag :keys [name link]}]
  (let [#^HttpClient client (new HttpClient)
        #^HeadMethod head (HeadMethod. (str link))]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout))
    (.. head getParams (setSoTimeout head-request-timeout))
    (try (let [status (.executeMethod client head)]
           (.releaseConnection head)
           (if (= status HttpStatus/SC_OK)
             (if-let [length (.. head (getResponseHeader "Content-Length") (getValue))]
               (idle (assoc ag :length (Integer/parseInt length)))
               (do (log/info (str "Невозможно узнать размер файла " name))
                   (die ag)))
             (fail ag)))
         (catch ConnectException e
           (do (log/info (str "Время ожидания соединения с сервером истекло для " name))
               (fail ag)))
         (catch ConnectTimeoutException e
           (do (log/info (str "Время ожидания соединения с сервером истекло для " name))
               (fail ag)))
         (catch InterruptedIOException e
           (do (log/info (str "Время ожидания ответа сервера истекло для " name))
               (fail ag)))
         (catch NoHttpResponseException e
           (do (log/info (str "Сервер не отвечает на запрос для " name))
               (fail ag)))
         (catch Exception e 
           (do (log/info (str "Ошибка во время получения длины файла " name))
               (fail ag)))
         (finally (.releaseConnection head)))))

(defmethod get-file :default [{:as ag :keys [name working-path]}]
  (idle (assoc ag :file (join-paths working-path name))))

(defmethod download :default [{:as ag :keys [name tag length link file]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str link))]
    (when (and link file)
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout connection-timeout))
      (.. get getParams (setSoTimeout get-request-timeout))
		(.setRequestHeader get "Range" (str "bytes=" (file-length file) \-))
      (try (.executeMethod client get)
           (let [content-length (.getResponseContentLength get)]
             (cond (not content-length)
                   (do (log/info (str "Невозможно проверить файл перед загрузкой " name))
                       (fail ag))

                   (not= content-length (- length (file-length file)))
                   (do (log/info (str "Размер получаемого файла не совпадает с ожидаемым " name))
                       (fail ag))

                   :else
                   (with-open [#^InputStream input (.getResponseBodyAsStream get)
                               #^FileOutputStream output (FileOutputStream. file true)]
                     (log/info (str "Начата загрузка " name))
                     (let [buffer (make-array Byte/TYPE buffer-size)]
                       (loop [progress (file-length file)]
                         (let [size (.read input buffer)]
                           (when (pos? size)
                             (do (.write output buffer 0 size)                                 
                                 (recur (+ progress size))))))
                       (.flush output)
                       (log/info (str "Закончена загрузка " name))
                       (assoc ag :fail false)))))
           (catch ConnectException e
             (do (log/info (str "Время ожидания соединения с сервером истекло для " name))
                 (fail ag)))
           (catch ConnectTimeoutException e
             (do (log/info (str "Время ожидания соединения с сервером истекло для " name))
                 (fail ag)))
           (catch InterruptedIOException e
             (do (log/info (str "Время ожидания ответа сервера истекло для " name))
                 (fail ag)))
           (catch NoHttpResponseException e
             (do (log/info (str "Сервер не отвечает на запрос для " name))
                 (fail ag)))
           (catch Exception e 
             (do (log/info (str "Ошибка во время загрузки " name))
                 (fail ag)))
           (finally (.releaseConnection get))))))

(defn next-alive-untagged-after [ag]
  (next-after-when (fn/and alive? (fn/not tag) (partial same aim ag))
                   ag (agents ag)))

(defn alive-unfailed-with-same-tag-as [ag]
  (some (fn/and alive? (fn/not fail) (partial same tag ag))
        (agents ag)))

(defn next-alive-with-same-tag-after [ag]
  (next-after-when (fn/and alive? (partial same tag ag))
                   ag (agents ag)))

;; (defn tag-locked-in-env? 
;;   [ag] (or (tag-locked? ag)
;;            (some (fn-and (partial same tag ag) tag-locked? (constantly true))
;;                  (env ag))))

(defmacro action [name body]
  `(with-meta #(~body %) {:action ~name}))

(defn- reflex [{:as ag :keys [address link tag name file length done-path]}]
  (cond (dead? ag)        (action :pass     pass)
        (not address)     (action :die      die)
        (not link)        (action :get-link get-link)
        (not tag)         (action :get-tag  get-tag)
        (not name)        (action :get-name get-name)
        (not file)        (action :get-file get-file)
        (already-on-done-path? ag)          die
        (not length)      (action :get-length get-length)
        (out-of-space-on-work-path? ag)     die
        
        (fully-loaded? ag) (cond (out-of-space-on-done-path? ag) die
                                 done-path move-to-done-path
                                 :else die)

        :else (action :download download)))

(defn- done [ag]
  (with-return ag
    (or (run (or (alive-unfailed-with-same-tag-as *agent*)
                 (next-alive-with-same-tag-after *agent*)))
        ;; (when (termination? ag) (terminate ag)))))
        )))

(defmethod run clojure.lang.Agent [ag & opts]
  (send-off ag run opts))

(defmethod run clojure.lang.PersistentHashMap [ag & opts]
  (cond (dead? ag) ag

        :else (execute ag (reflex ag))))

        ;; ((no tag) ag)
        ;; (let [new-state (action/percept-and-execute ag)]
        ;;   (cond (dead? new-state) (done *agent*)
        ;;         (fail? new-state) (do (sleep *agent* timeout-after-fail)
        ;;                               (when-not (debug? ag) (run *agent*)))
        ;;         (tag new-state) (when-not (debug? ag)
        ;;                           (run (next-alive-untagged-after ag))
        ;;                           (run *agent*))
        ;;         :else (when-not (debug? ag) (run *agent*)))
        ;;   new-state)

        ;; (tag-locked-in-env? ag) ag

        ;; :else (let [new-state 
        ;;             (with-locked-tag ag (action/percept-and-execute ag))]
        ;;         (cond (dead? new-state) (done *agent*)
        ;;               (fail? new-state) (do (sleep *agent* timeout-after-fail)
        ;;                                     (done *agent*))
        ;;               :else (when-not (debug? ag) (run *agent*)))
        ;;         new-state)))))


(comment
(def d (make-download-agent :line "http://dsv.data.cod.ru/720957"
                            :working-path (File. "/home/haru/inbox/")))
(:action (meta (reflex @d)))
(execute @d (reflex @d))
(def gl (run (run (run @d))))
gl

(do
  (reset! (gl :status) :idle)
  (execute gl get-length))

(:action (meta (reflex gl)))
(run gl)
d
(agent-error d)
(send-off d get-link)
(send-off d get-tag)
(send-off d get-file)
(send-off d get-length)
(send-off d download)
)
