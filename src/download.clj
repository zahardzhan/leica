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
            progress
            verified)
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

(def *services* (ref {}))

(defn defservice [name & {:as opts :keys [address hosts]}]
  (dosync (alter *services* assoc name {:address address
                                        :hosts hosts})))

(defn remove-service [name]
  (dosync (alter *services* dissoc name)))

(defmacro access-service [service & accessors]
  `(derefed *services* ~service ~@accessors))

(defn match-service [line]
  (some (fn [[service {address-pattern :address}]]
          (when-let [address (re-find address-pattern line)]
            {:service service :address address}))
        @*services*))

(defservice ::data.cod.ru
  :address #"http://[\w\.\-]*.data.cod.ru/\d+"
  :hosts #{#"files3?.dsv.*.data.cod.ru" #"files2.dsv.*.data.cod.ru"})

(defservice ::77.35.112.8*
  :address #"http://77.35.112.8[1234]/.+")

(defservice ::dsvload.net.ftpupload
  :address #"http://dsvload.net/ftpupload/.+")

(defn- dispatch-by-service
  ([] nil)
  ([ag] (:service ag))
  ([ag & opts] (:service ag)))

(defmulti status            dispatch-by-service)
(defmulti status!           dispatch-by-service)
(defmulti status?           dispatch-by-service)

(defmulti dead?             dispatch-by-service)
(defmulti alive?            dispatch-by-service)
(defmulti run?              dispatch-by-service)
(defmulti does-nothing?     dispatch-by-service)
(defmulti ok?               dispatch-by-service)
(defmulti idle?             dispatch-by-service)
(defmulti fail?             dispatch-by-service)

(defmulti out-of-space-on-work-path? dispatch-by-service)
(defmulti out-of-space-on-done-path? dispatch-by-service)
(defmulti fully-loaded?              dispatch-by-service)
(defmulti already-on-done-path?      dispatch-by-service)

(defmulti pass              dispatch-by-service)
(defmulti idle              dispatch-by-service)
(defmulti fail              dispatch-by-service)
(defmulti die               dispatch-by-service)
(defmulti sleep             dispatch-by-service)
(defmulti parse-page        dispatch-by-service)
(defmulti get-host          dispatch-by-service)
(defmulti get-file          dispatch-by-service)
(defmulti get-length        dispatch-by-service)
(defmulti move-to-done-path dispatch-by-service)
(defmulti download          dispatch-by-service)

(defmulti reflex            dispatch-by-service)
(defmulti reflex-with-transfer-of-control dispatch-by-service)

(derive ::download-environment :agent/environment)
(derive ::download-agent       :agent/agent)

(defn make-download-env [& {:as opts :keys [type agents]}]
  (make-env :type ::download-environment
            :agents agents))

(defn download-env? [e]
  (and (env? e)
       (isa? (type e) ::download-environment)))

(def *precedence* (atom 0))

(defn reset-precedence []
  (reset! *precedence* 0))

(defn make-download-agent
  [address-line & {:as opts
                   :keys [strategy working-path done-path link file
                          environment meta validator error-handler error-mode]}]
  {:pre  [(when-supplied strategy     (or (multimethod? strategy) (fn? strategy))
                         working-path (verified/output-dir working-path)
                         done-path    (verified/output-dir done-path)
                         file         (file? file)
                         environment  (download-env? environment))]}

  (let [{:keys [service address]}
        (when address-line (match-service address-line))]

    (when (and service address)
      (make-agent :type ::download-agent
                  :precedence (swap! *precedence* inc)
                  :status (atom :idle)
                  :service service
                  :strategy (or strategy reflex)
                  :address (URI. address)
                  :working-path working-path
                  :done-path done-path
                  :host nil
                  :link nil
                  :file nil
                  :length nil
                  :environment environment
                  :meta meta
                  :validator validator
                  :error-handler error-handler
                  :error-mode error-mode))))

(defn download-agent-body? [a]
  (and (env-agent-body? a) (isa? (type a) ::download-agent)))

(defn download-agent? [a]
  (and (env-agent? a) (derefed a download-agent-body?)))

(defmethod status :default [ag]
  (deref (:status ag)))

(defmethod status! :default [ag new-status]
  {:pre [(({:idle    #{:running}
            :fail    #{:running}
            :dead    #{}
            :running #{:stoping :idle :fail :dead}
            :stoping #{:idle :fail :dead}}
           (status ag)) new-status)]}
  (with-return ag
    (reset! (:status ag) new-status)))

(defmethod status? :default [ag current-status]
   (cond (keyword? current-status) (= (status ag) current-status)
         (set? current-status) (keyword? (current-status (status ag)))))

(defmethod dead? :default [ag]
  (status? ag :dead))

(defmethod alive? :default [ag]
  (not (dead? ag)))

(defmethod run? :default [ag]
  (status? ag #{:running :stoping}))

(defmethod does-nothing? :default [ag]
  (status? ag #{:idle :fail}))
  
(defmethod ok? :default [ag]
  (status? ag #{:idle :running :stoping}))
  
(defmethod idle? :default [ag]
  (status? ag :idle))
  
(defmethod fail? :default [ag]
  (status? ag :fail))

(defmethod out-of-space-on-work-path? :default
  [{:as ag :keys [working-path length file]}]
  (when (and working-path length file)
    (< (.getUsableSpace working-path)
       (- length (file-length file)))))

(defmethod out-of-space-on-done-path? :default
  [{:as ag :keys [done-path length]}]
  (when (and done-path length)
    (< (.getUsableSpace done-path) length)))

(defmethod fully-loaded? :default
  [{:as ag :keys [length file]}]
  (when (and length file)
    (<= length (file-length file))))

(defmethod already-on-done-path? :default
  [{:as ag :keys [done-path file]}]
  (when (and done-path file)
    (.exists (File. done-path (.getName file)))))

(defmethod get-action ::download-agent [ag & opts]
  ((ag :strategy) ag opts))

(defmethod execute ::download-agent [ag action]
  (if (dead? ag) ag
      (try (status! ag :running)
           (try (let [ag- (action ag)]
                  (cond (run? ag-) (status! ag- :idle)
                        :else ag-))
                (catch Exception _ (status! ag :fail)))
           (catch AssertionError _ ag))))

(defmethod run ::download-agent [ag & opts]
  (execute ag (get-action ag opts)))

(defmethod done? ::download-agent [a]
  (dead? a))

(defmethod done? ::download-environment [e]
  (every? dead? (deref-seq (agents e))))

(defmethod terminate ::download-agent [a]
  a)

(defmethod terminate ::download-environment [e]
  e)

(def buffer-size          4096)
(def timeout-after-fail   3000)
(def connection-timeout   30000)
(def head-request-timeout 30000)
(def get-request-timeout  30000)

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

(defn alive-download-agents-without-host-after [a]
  (select :from (agents (env a)) :entirely-after a
          :order-by #(derefed % :precedence)
          :where #(and (download-agent? %)
                      (derefed % alive?)
                      (derefed % :host not))))

(defn idle-download-agents-with-same-host-as [a]
  (select :from (agents (env a)) :order-by #(derefed % :precedence) 
          :where #(and (download-agent? %) (derefed % idle?)
                      (same :host (deref a) (deref %)))))

(defn alive-download-agents-with-same-host-after [a]
  (select :from (agents (env a)) :entirely-after a
          :order-by #(derefed % :precedence)
          :where #(and (download-agent? %) (derefed % alive?)
                      (same :host (deref a) (deref %)))))

(defn download-agents-running-on-same-host-as [a]
  (select :from (surrounding a) :order-by #(derefed % :precedence)
          :where #(and (download-agent? %) (derefed % run?)
                      (same :host (deref a) (deref %)))))

(defmethod reflex-with-transfer-of-control :default [a & opts]
  (let [done #(or (send-off (or (first (idle-download-agents-with-same-host-as *agent*))
                                (first (alive-download-agents-with-same-host-after *agent*)))
                            run)
                  (when (done? (env *agent*)) (terminate (env *agent*))))]
    
    (cond (not (:host a))
          (fn [a] (let-return [a- ((reflex a) a)]
                              (cond (dead? a-) (done)
                                    (fail? a-) (do (send-off *agent* sleep timeout-after-fail)
                                                   (send-off *agent* run))
                                    (:host a-) (do (send-off (first (alive-download-agents-without-host-after *agent*)) run)
                                                   (send-off *agent* run))
                                    :else (send-off *agent* run))))

          (seq (download-agents-running-on-same-host-as *agent*)) pass
          
          :else
          (fn [a] (let-return [a- ((reflex a) a)]
                              (cond (dead? a-) (done)
                                    (fail? a-) (do (send-off *agent* sleep timeout-after-fail)
                                                   (done))
                                    :else (send-off *agent* run)))))))

(defmethod pass :default [ag]
  ag)

(defmethod idle :default [ag]
  (status! ag :idle))

(defmethod fail :default [ag]
  (status! ag :fail))

(defmethod die  :default [ag]
  (status! ag :dead))

(defmethod sleep :default [ag millis]
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
  [{:as ag :keys [link service]}]
  (let [hosts (seq (access-service service :hosts))
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

  (def aggg (agent 1))
  (send-off aggg (fn [x] (future  (. System/out println 2))))

  (def de (make-download-env))
  (def da3 (make-download-agent "http://dsv.data.cod.ru/775759"
                                :working-path (File. "/home/haru/Inbox/")
                                :strategy reflex-with-transfer-of-control
                                :environment de))
  (def da4 (make-download-agent "http://dsv.data.cod.ru/772992"
                                :working-path (File. "/home/haru/Inbox/")
                                :strategy reflex-with-transfer-of-control
                                :environment de))
  de
  da1
  da2
  [da3
   da4]
  (binded? da1)
  (binded? da2)
  (run @da3)
  (run @da1)
  (send-off da3 run)
  (execute @da3 (reflex-with-transfer-of-control  @da3))
  (execute @da4 (reflex-with-transfer-of-control  @da4))

  (run (run (run (run (run @da1)))))

  (def uri1 "let me be in http://lifehacker.ru/2010/05/06/video-stiv-dzhobs-%C2%ABostavajjtes-golodnymi-ostavajjtes-bezrassudnymi%C2%BB/ fucking uri")
  (java.net.URL. (first (re-find #"((https?|ftp|gopher|telnet|file|notes|ms-help):((//)|(\\\\))+[\w\d:#@%/;$()~_?\+-=\\\.&]*)" uri1)))
  )
