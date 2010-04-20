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

(defprotocol Download-agent-protocol
  (status [ag])
  (status! [ag new-status])
  (status? [ag current-status])

  (dead? [ag])
  (alive? [ag])
  (run? [ag])
  (does-nothing? [ag])
  (ok? [ag])
  (idle? [ag])
  (fail? [ag])

  (services [ag])
  
  (out-of-space-on-work-path? [ag])
  (out-of-space-on-done-path? [ag])
  (fully-loaded? [ag])
  (already-on-done-path? [ag])

  (execute [ag action])
  (run [ag] [ag & opts]))

(def *rec*         true)
(def *transfer*    true)
  
(deftype Download-agent [status-atom services- service strategy
                         address working-path done-path 
                         host link file length]

  clojure.lang.IPersistentMap
  
  Download-agent-protocol
  (status [ag] @status-atom)

  (status!
   [ag new-status]
   {:pre [(({:idle    #{:running}
             :fail    #{:running}
             :dead    #{}
             :running #{:stoping :idle :fail :dead}
             :stoping #{:idle :fail :dead}}
            (status ag)) new-status)]}
   (with-return ag
     (reset! status-atom new-status)))

  (status?
   [ag current-status]
   (cond (keyword? current-status) (= (status ag) current-status)
         (set? current-status) (keyword? (current-status (status ag)))))

  (dead? [ag] (status? ag :dead))
  
  (alive? [ag] (not (dead? ag)))

  (run? [ag] (status? ag #{:running :stoping}))
  
  (does-nothing? [ag] (status? ag #{:idle :fail}))
  
  (ok? [ag] (status? ag #{:idle :running :stoping}))
  
  (idle? [ag] (status? ag :idle))
  
  (fail? [ag] (status? ag :fail))

  (services [ag] @services-)

  (out-of-space-on-work-path?
   [ag]
   (when (and working-path length file)
     (< (.getUsableSpace working-path)
        (- length (file-length file)))))

  (out-of-space-on-done-path?
   [ag]
   (when (and done-path length)
     (< (.getUsableSpace done-path) length)))

  (fully-loaded?
   [ag]
   (when (and length file)
     (<= length (file-length file))))

  (already-on-done-path?
   [ag]
   (when (and done-path file)
     (.exists (File. done-path (.getName file)))))

  (execute
   [ag action]
   (if (dead? ag) ag
       (try (status! ag :running)
            (let [fag (future (action ag))]
              (try (with-deref [fag]
                     (cond (run? fag) (status! fag :idle)
                           :else fag))
                   (catch Exception _ (status! ag :fail))))
            (catch AssertionError _ ag))))

  (run
   [ag]
   (execute ag (strategy ag)))

  (run
   [ag & {:as opts :keys [rec transfer]}]
   (binding [*rec* rec
             *transfer* transfer]
     (execute ag (strategy ag opts)))))

(defmacro transfering-control [& body]
  `(when (and *agent* *transfer*)
     (do ~@body)))

(defmacro recursively [& body]
  `(when (and *agent* *rec*)
     (do ~@body)))

(defn- service-dispatch
  ([] nil)
  ([ag] (:service ag))
  ([ag & opts] (:service ag)))

(defmulti pass              service-dispatch)
(defmulti idle              service-dispatch)
(defmulti fail              service-dispatch)
(defmulti die               service-dispatch)
(defmulti sleep             service-dispatch)
(defmulti parse-page        service-dispatch)
(defmulti get-host          service-dispatch)
(defmulti get-file          service-dispatch)
(defmulti get-length        service-dispatch)
(defmulti move-to-done-path service-dispatch)
(defmulti download          service-dispatch)

(defmulti reflex            service-dispatch)
(defmulti reflex-with-transfer-of-control service-dispatch)

(defn make-download-agent
  [address-line & {:as opts :keys [services strategy working-path done-path]
                   :or {services *services*
                        strategy reflex}}]

  (let [{:keys [service address]}
        (when address-line (match-service address-line services))]

    (when (and service address)
      (make-agent (Download-agent (atom :idle)
                                  (delay services)
                                  service
                                  strategy
                                  (URI. address)
                                  working-path
                                  done-path
                                  nil nil nil nil)))))

(defn download-agent? [ag]
  (= :download/Download-agent (type (derefed ag))))

(defn download-env-termination? [env] false)

(defn terminate-download-env [env] false)

(defmethod reflex :default
  [{:as ag :keys [address link host name file length done-path]} & opts]
  (cond (not address)              die
        (not (or link name))       parse-page
        (not host)                 get-host
        (not file)                 get-file
        (already-on-done-path? ag) die
        (not length)               get-length
        (out-of-space-on-work-path? ag) die
        
        (fully-loaded? ag)
        (cond (out-of-space-on-done-path? ag) die
              done-path            move-to-done-path
              :else                die)

        :else                      download))

(defn some-idle-same-host-as [ag]
  (some (fn [sag] (with-deref [ag sag] (and (same type ag sag)
                                            (same :host ag sag)
                                            (idle? sag)
                                            sag)))
        (surrounding ag)))

(defn next-alive-same-host-after [ag]
  (next-after-when (fn [sag] (with-deref [ag sag] (and (same type ag sag)
                                                       (alive? sag))))
                   ag (surrounding ag)))

(defn done [ag & opts]
  (with-return ag
    (or (transfering-control (send (or (some-idle-same-host-as *agent*)
                                       (next-alive-same-host-after *agent*))
                                   run opts))
        (when (and *agent* (download-env-termination? (env *agent*)))
          (terminate-download-env (env *agent*))))))

(defn next-unhosted-and-nothing-doing-after [ag]
  (next-after-when (fn [sag] (with-deref [ag sag] (and (same type ag sag)
                                                       (does-nothing? sag)
                                                       (not (:host sag)))))
                   ag (surrounding ag)))

(defn some-surrounding-agent-running-on-same-host-as [ag]
  (some (fn [sag] (with-deref [ag sag] (and (same type ag sag)
                                            (same :host ag sag)
                                            (run? sag))))
        (surrounding ag)))

(defmethod reflex-with-transfer-of-control :default
  [ag & opts]
  (cond (not (:host ag))
        (fn [ag]
          (let-return [ag- (reflex ag opts)]
                      (cond (dead? ag-) (transfering-control (done ag- opts))
                            (fail? ag-) (do (sleep ag- timeout-after-fail)
                                            (recursively (send *agent* run opts)))
                            (:host ag-) (do (transfering-control
                                             (send (next-unhosted-and-nothing-doing-after *agent*) run opts))
                                            (recursively (send *agent* run opts)))
                            :else (recursively (send *agent* run opts)))))

        (some-surrounding-agent-running-on-same-host-as *agent*) pass

        :else
        (fn [ag]
          (let-return [ag- (reflex ag opts)]
                      (cond (dead? ag-) (transfering-control (done ag- opts))
                            (fail? ag-) (do (sleep ag- timeout-after-fail)
                                            (transfering-control (done ag- opts)))
                            :else (recursively (send *agent* run opts)))))))

(def buffer-size          4096)
(def timeout-after-fail   3000)
(def connection-timeout   30000)
(def head-request-timeout 30000)
(def get-request-timeout  30000)

(defmethod pass :default
  [ag]
  ag)

(defmethod idle :default
  [ag]
  (status! ag :idle))

(defmethod fail :default
  [ag]
  (status! ag :fail))

(defmethod die  :default
  [ag]
  (status! ag :dead))

(defmethod sleep :default
  [ag millis]
  (with-return ag
    (Thread/sleep millis)))

(defmethod parse-page :default
  [{:as ag :keys [address]}]
  (idle (assoc ag
          :link address
          :name (second (re-find #"/([^/]+)$" (.getPath address))))))

(defmethod parse-page ::data.cod.ru
  [{:as ag :keys [address]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str address))]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout))
    (.. get getParams (setSoTimeout get-request-timeout))
    (try (let [status (.executeMethod client get)]
           (if (= status HttpStatus/SC_OK)
             (let [page (duck/slurp* (.getResponseBodyAsStream get))
                   link (URI. (re-find #"http://files[-\d\w\.]*data.cod.ru/[^\"]+" page) true)
                   name (second (re-find #"<b title=\".*\">(.*)</b>" page))
                   space (let [[_ space unit]
                               (re-find #"Вам доступно ([\d\.]+) (\p{javaUpperCase}{2})" page)]
                           (when (and space unit)
                             (int (* (Float/parseFloat space)
                                     ({"ГБ" 1073741824, "МБ" 1048576, "КБ" 1024} unit)))))]
               (if (and name link)
                 (idle (assoc ag :name name :link link))
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
 
(defmethod move-to-done-path :default
  [{:as ag :keys [#^File file, #^File done-path]}]
  (if-let [moved (move-file file done-path)]
    (idle (assoc ag :file moved))
    (die ag)))

(defmethod get-host :default
  [{:as ag :keys [link service services]}]
  (let [hosts (seq (-> services force service :hosts))
        host (or (when hosts
                  (some (fn [host] (when (re-find host (str link))
                                    (str host)))
                        hosts))
                (.getHost link))]
    (if host
      (idle (assoc ag :host host))
      (die ag))))

(defmethod get-length :default
  [{:as ag :keys [name link]}]
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

(defmethod get-file :default
  [{:as ag :keys [name working-path]}]
  (idle (assoc ag :file (join-paths working-path name))))

(defmethod download :default
  [{:as ag :keys [name host length link file]}]
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

(comment
(def d (make-download-agent "http://dsv.data.cod.ru/745448"
                            :working-path (File. "/home/haru/inbox/")))

(:action (meta (individual-reflex-strategy @d)))

(run (run (run (run (run @d)))))

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
(send-off d get-host)
(send-off d get-file)
(send-off d get-length)
(send-off d download)
)
