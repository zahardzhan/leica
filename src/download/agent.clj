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

(ns download.agent
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

(in-ns 'download.agent)

(defn match-service [line services]
  (some (fn [[service {address-pattern :address}]]
          (when-let [address (re-find address-pattern line)]
            {:service service :address address}))
        services))

(def *services*
     {::cod.data
      {:address #"http://[\w\.\-]*.data.cod.ru/\d+"
       :tags #{#"files3?.dsv.*.data.cod.ru" #"files2.dsv.*.data.cod.ru"}}

      ::77.35.112.8*
      {:address #"http://77.35.112.8[1234]/.+"}

      ::dsvload.net.ftpupload
      {:address #"http://dsvload.net/ftpupload/.+"}})

(derive ::download :agent/dummy)

(defn download-agent
  [& state]
  (let [{:keys [line services working-path done-path]
         :or {services *services*}}
        (apply hash-map state)

        {:keys [service address]}
        (when line (match-service line services))]

    (when (and service address)
      (make-agent :aim ::download
                  :service service
                  :services (delay services)
                  :address (URI. address)
                  :working-path working-path
                  :done-path done-path
                  :alive true
                  :fail false
                  :link nil
                  :file nil
                  :length nil))))

(defmulti run  type)
(defmulti stop type)

(defmulti ok       :service)
(defmulti fail     :service)
(defmulti die      :service)
(defmulti pass     :service)
(defmulti sleep    :service)
(defmulti get-link :service)
(defmulti get-name :service)
(defmulti get-tag  :service)
(defmulti get-file :service)
(defmulti get-length :service)
(defmulti move-to-done-path :service)
(defmulti download :service)

(def timeout-after-fail   3000)
(def connection-timeout   30000)
(def head-request-timeout 30000)
(def get-request-timeout  30000)

(defmethod pass :default [ag] ag)

(defmethod ok :default [ag]
  (assoc ag :fail false))
 
(defmethod fail :default [ag]
  (assoc ag :fail true))
 
(defmethod die :default [ag]
  (assoc ag :alive false))

(defmethod sleep :default [ag millis]
  (with-return ag
    (Thread/sleep millis)))

(defmethod get-link :default [{:as ag :keys [address]}]
  (ok (assoc ag :link address)))

(defmethod get-link ::cod.data [{:as ag :keys [address]}]
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
                 (assoc ag :name (parsed :name) :link (parsed :link) :fail false)
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
  (ok (assoc ag :name (second (re-find #"/([^/]+)$" (.getPath link))))))

(defmethod get-name ::cod.data [ag]
  (get-link ag))

(defmethod move-to-done-path :default [{:as ag :keys [#^File file, #^File done-path]}]
  (if-let [moved (move-file file done-path)]
    (ok (assoc ag :file moved))
    (die ag)))

(defmethod get-tag :default [{:as ag :keys [link service services]}]
  (let [tags (seq (-> services force service :tags))
        tag (or (when tags
                  (some (fn [tag] (when (re-find tag (str link))
                                    (str tag)))
                        tags))
                (.getHost link))]
    (if tag
      (ok (assoc ag :tag tag))
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
               (assoc ag :length (Integer/parseInt length) :fail false)
               (do (log/info (str "Невозможно узнать размер файла " name))
                   (die ag)))
             (die ag)))
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
  (ok (assoc ag :file (join-paths working-path name))))

(defmethod download :default [{:as ag :keys [name tag length link file
                                             progress-agent]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str link))
        buffer-size 4096]
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
                                 (when progress-agent
                                   (send progress-agent progress/show-progress
                                         {:tag tag :name name :progress progress
                                          :total length :time nil}))
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
           (finally (when progress-agent
                      (send progress-agent progress/hide-progress tag))
                    (.releaseConnection get))))))

(defn out-of-space-on-work-path? [{:keys [working-path length file]}]
  (when (and working-path length file)
    (< (.getUsableSpace working-path)
       (- length (file-length file)))))

(defn out-of-space-on-done-path? [{:keys [done-path length]}]
  (when (and done-path length)
    (< (.getUsableSpace done-path) length)))

(defn fully-loaded? [{:keys [length file]}]
  (when (and length file)
    (<= length (file-length file))))

(defn already-on-done-path? [{:keys [done-path file]}]
  (when (and done-path file)
    (.exists (File. done-path (.getName file)))))

(defn next-alive-untagged-after [ag]
  (next-after-when #(and (derefed % :alive)
                         (derefed % :tag not)
                         (same aim ag %))
                   ag (agents ag)))

(defn alive-unfailed-with-same-tag-as [ag]
  (some #(and (derefed % :alive)
              (derefed % :fail not)
              (same :tag (derefed ag) (derefed %))
              %)
        (agents ag)))

(defn next-alive-with-same-tag-after [ag]
  (next-after-when #(and (derefed % :alive)
                         (same :tag (derefed ag) (derefed %)))
                   ag (agents ag)))

;; (defn lock-tag
;;   [ag] (reset! (tag-lock ag) true))

;; (defn unlock-tag
;;   [ag] (reset! (tag-lock ag) false))

;; (defn tag-locked-in-env? 
;;   [ag] (or (tag-locked? ag)
;;            (some (fn-and (partial same tag ag) tag-locked? (constantly true))
;;                  (env ag))))

;; (defmacro with-locked-tag
;;   "Замыкает (блокирует) таг агента на время выполнения им действия, блокирующего
;;   действия других агентов в окружении с таким же тагом."
;;   [ag & body]
;;   `(when-not (tag-locked-in-env? ~ag)
;;      (lock-tag ~ag)
;;      (let [result# (do ~@body)]
;;        (unlock-tag ~ag)
;;        result#)))

(defn reflex [{:as ag :keys [alive address link tag name file length done-path]}]
  (cond (not alive)       #(pass ag)
        (not address)     #(die ag)
        (not link)        #(get-link ag)
        (not tag)         #(get-tag ag)
        (not name)        #(get-name ag)
        (not file)        #(get-file ag)
        (already-on-done-path? ag)          #(die ag)
        (not length)      #(get-length ag)
        (out-of-space-on-work-path? ag)     #(die ag)
        
        (fully-loaded? ag) (cond (out-of-space-on-done-path? ag) #(die ag)
                                 done-path #(move-to-done-path ag)
                                 :else #(die ag))

        :else #(download ag)))

(defmethod run clojure.lang.Agent [ag]
  (send-off ag run))

;; (defmethod run clojure.lang.PersistentArrayMap [ag]
;;   (let [done (fn [ag]
;;                (or (run (or (alive-unfailed-with-same-tag-as *agent*)
;;                             (next-alive-with-same-tag-after *agent*)))
;;                    (when (termination? ag) (terminate ag)))
;;   ag)

;;     (cond (dead? ag) ag

;;           ((no tag) ag)
;;           (let [new-state (action/percept-and-execute ag)]
;;             (cond (dead? new-state) (done *agent*)
;;                   (fail? new-state) (do (sleep *agent* timeout-after-fail)
;;                                         (when-not (debug? ag) (run *agent*)))
;;                   (tag new-state) (when-not (debug? ag)
;;                                     (run (next-alive-untagged-after ag))
;;                                     (run *agent*))
;;                   :else (when-not (debug? ag) (run *agent*)))
;;             new-state)

;;           (tag-locked-in-env? ag) ag

;;           :else (let [new-state 
;;                       (with-locked-tag ag (action/percept-and-execute ag))]
;;                   (cond (dead? new-state) (done *agent*)
;;                         (fail? new-state) (do (sleep *agent* timeout-after-fail)
;;                                               (done *agent*))
;;                         :else (when-not (debug? ag) (run *agent*)))
;;                   new-state))))

;; http://dsv.data.cod.ru/694636
(comment
(def d (download-agent :line "http://dsv.data.cod.ru/694636"))
(send d get-link)
(get-tag @d)
              #(get-name ag)
            #(get-file ag)


)