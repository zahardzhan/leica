;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Базовые действия агентов."
       :author "Роман Захаров"}
  action
  (:require progress
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux match)
  (:import (java.io File FileOutputStream InputStream InterruptedIOException)
           (org.apache.commons.httpclient URI HttpClient HttpStatus ConnectTimeoutException)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'action)

(def *default-head-request-timeout* 60000)
(def *default-get-request-timeout* 60000)
(def *default-connection-timeout* 50000)

(defmacro with-action [action-name & body]
  `(let [action-result# (do ~@body)]
     (assoc action-result# :action ~action-name)))

(defn pass [ag env]
  ag)
 
(defn fail [ag env]
  (assoc ag :fail true))
 
(defn die [ag env]
  (assoc ag :alive false))
 
(defn get-link [ag env]
  (when-let [#^URI address (ag :address)]
    (assoc ag :link address :fail false)))
 
(defn get-name [ag env]
  (when-let [#^URI link (ag :link)]
    (assoc ag :name (second (re-find #"/([^/]+)$" (.getPath link))) :fail false)))

(defn move-to-done-path [ag env]
  (when-let [#^File done-path (env :done-path)]
    (when-let [#^File file (ag :file)]
      (if-let [#^File moved (move-file file done-path)]
        (assoc ag :file moved :fail false)
        (die ag env)))))

(defn get-tag 
  ([pattern] (fn [ag env] (get-tag pattern ag env)))
  ([ag env]  (get-tag nil ag env))
  ([pattern ag env]
     (when-let [#^URI link (ag :link)]
       (when-let [tag (or (if pattern
                            (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                                  (if (sequential? pattern) pattern [pattern])))
                          (.getHost link))]
         (assoc ag :tag tag :fail false)))))

(defn get-length [ag env]
  (when-let [#^URI link (ag :link)]
    (let [#^HttpClient client (new HttpClient)
          #^HeadMethod head (HeadMethod. (str link))]
      ;; Sets the timeout until a connection is etablished.
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout *default-connection-timeout*))
      ;; Sets the default socket timeout which is the timeout for waiting for data.
      (.. head getParams (setSoTimeout *default-head-request-timeout*))
      (try (let [status (.executeMethod client head)]
             (.releaseConnection head)
             (if (= status HttpStatus/SC_OK)
               (if-let [length (.. head (getResponseHeader "Content-Length") (getValue))]
                 (assoc ag :length (Integer/parseInt length) :fail false)
                 (do (log/info (str "Невозможно узнать размер файла " (ag :name)))
                     (die ag env)))
               ((http-error-status-handler status die fail) ag env)))
           (catch java.io.InterruptedIOException e
             (do (log/info (str "Время ожидания ответа сервера истекло для " (ag :name)))
                 (fail ag env)))
           (catch java.net.ConnectException e (die ag env))
           (catch Exception e (die ag env))
           (finally (.releaseConnection head))))))

(defn get-file [ag env]
  (when-let [name (ag :name)]
    (when-let [#^File working-path (env :working-path)]
      (assoc ag :file (new File (join-paths working-path name)) :fail false))))

(def *buffer-size* 4096)

(defn download [ag env]
  (let [progress-agent (:progress-agent env)
        #^URI link (:link ag)
        #^File file (:file ag)
        #^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str link))]
    (when (and link file)
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout action/*default-connection-timeout*))
      (.. get getParams (setSoTimeout action/*default-get-request-timeout*))
		(.setRequestHeader get "Range" (str "bytes=" (file-length file) "-"))
      (try (.executeMethod client get)
           (let [content-length (.getResponseContentLength get)]
             (cond (not content-length)
                   (do (log/info (str "Невозможно проверить файл перед загрузкой " (ag :name)))
                       (fail ag env))

                   (not= content-length (- (ag :length) (file-length file)))
                   (do (log/info (str "Размер получаемого файла не совпадает с ожидаемым " (ag :name)))
                       (fail ag env))

                   :else
                   (with-open [#^InputStream input (.getResponseBodyAsStream get)
                               #^FileOutputStream output (FileOutputStream. file true)]
                     (log/info (str "Начата загрузка " (ag :name)))
                     (let [buffer (make-array Byte/TYPE *buffer-size*)]
                       (loop [progress (file-length file)]
                         (let [size (.read input buffer)]
                           (when (pos? size)
                             (do (.write output buffer 0 size)
                                 (when progress-agent
                                   (send progress-agent progress/show-progress
                                         {:tag (:tag ag) :name (:name ag) 
                                          :progress progress :total (:length ag)
                                          :time nil}))
                                 (recur (+ progress size))))))
                       (.flush output)
                       (log/info (str "Закончена загрузка " (ag :name)))
                       (assoc ag :fail false)))))
           (catch ConnectTimeoutException e
             (do (log/info (str "Время ожидания соединения с сервером истекло для " (ag :name)))
                 (fail ag env)))
           (catch InterruptedIOException e
             (do (log/info (str "Время ожидания ответа сервера истекло для " (ag :name)))
                 (fail ag env)))
           (catch Exception e 
             (do (log/info (str "Загрузка не может быть закончена " (ag :name)))
                 (die ag env)))
           (finally (send progress-agent progress/hide-progress (:tag ag))
                    (.releaseConnection get))))))