;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Базовые действия агентов."
       :author "Роман Захаров"}
  download.action
  (:require progress [clojure.contrib.logging :as log])
  (:use action aux env match)
  (:import (java.io File FileOutputStream InputStream InterruptedIOException)
           (org.apache.commons.httpclient 
            URI HttpClient HttpStatus
            ConnectTimeoutException NoHttpResponseException)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'download.action)

(def default-head-request-timeout 60000)
(def default-get-request-timeout 60000)
(def default-connection-timeout 50000)

(defn get-link [{:as ag :keys [#^URI address]}]
  (assoc ag :link address :fail false))
 
(defn get-name [{:as ag :keys [#^URI link]}]
  (assoc ag :name (second (re-find #"/([^/]+)$" (.getPath link))) :fail false))

(defn move-to-done-path [{:as ag :keys [#^File file, #^File done-path]}]
  (if-let [moved (move-file file done-path)]
    (assoc ag :file moved :fail false)
    (die ag)))

(defn get-tag [pattern {:as ag :keys [#^URI link]}]
  (when-let [tag (or (if pattern
                       (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                             (if (sequential? pattern) pattern [pattern])))
                     (.getHost link))]
    (assoc ag :tag tag :fail false)))

(defn get-length [{:as ag :keys [name, #^URI link]}]
  (let [#^HttpClient client (new HttpClient)
        #^HeadMethod head (HeadMethod. (str link))]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout default-connection-timeout))
    (.. head getParams (setSoTimeout default-head-request-timeout))
    (try (let [status (.executeMethod client head)]
           (.releaseConnection head)
           (if (= status HttpStatus/SC_OK)
             (if-let [length (.. head (getResponseHeader "Content-Length") (getValue))]
               (assoc ag :length (Integer/parseInt length) :fail false)
               (do (log/info (str "Невозможно узнать размер файла " name))
                   (die ag)))
             ((http-error-status-handler status die fail) ag)))
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

(defn get-file [{:as ag :keys [name, #^File working-path]}]
  (assoc ag :file (join-paths working-path name) :fail false))

(defn download [{:as ag :keys [name tag length progress-agent
                               #^URI link, #^File file]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str link))
        buffer-size 4096]
    (when (and link file)
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout default-connection-timeout))
      (.. get getParams (setSoTimeout default-get-request-timeout))
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