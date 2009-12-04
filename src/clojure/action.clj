;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Базовые действия агентов."
       :author "Роман Захаров"}
  action
  (:require progress
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux match env)
  (:import (java.io File FileOutputStream InputStream InterruptedIOException)
           (org.apache.commons.httpclient 
            URI HttpClient HttpStatus ConnectTimeoutException NoHttpResponseException)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'action)

(def *default-head-request-timeout* 60000)
(def *default-get-request-timeout* 60000)
(def *default-connection-timeout* 50000)

(defn execute-action [ag action]
  (let [result (atom nil)
        thread
        (Thread. 
         #(do (log/debug (str (or (ag :name) (ag :address)) \space action))
              (reset! result (((ag :actions) action) ag))
              (when-not (#{:die :fail :pass} action)
                (reset! result (assoc @result :action action)))
              (log/debug (str (or (ag :name) (ag :address))
                              \space action \space
                              (cond (dead? @result) "агент умер"
                                    (fail? @result) "агент провалился"
                                    :else "успешно")))))]
    (.start thread)
    (.join thread)
    @result))

(defn percept-and-execute [ag percept]
  (let [action ((ag :program) percept)]
    (execute-action ag action)))

(defn after
  ([action ag] (= action (:action ag)))
  ([status action ag] 
     (and (after action ag)
          (case status
                :successful (not (fail? ag))
                :failed     (fail? ag)))))

(defn pass [ag]
  ag)
 
(defn fail [ag]
  (assoc ag :fail true))
 
(defn die [ag]
  (assoc ag :alive false))
 
(defn get-link [ag]
  (when-let [#^URI address (ag :address)]
    (assoc ag :link address :fail false)))
 
(defn get-name [ag]
  (when-let [#^URI link (ag :link)]
    (assoc ag :name (second (re-find #"/([^/]+)$" (.getPath link))) :fail false)))

(defn move-to-done-path [ag]
  (when-let [#^File done-path ((deref (related-env ag)) :done-path)]
    (when-let [#^File file (ag :file)]
      (if-let [#^File moved (move-file file done-path)]
        (assoc ag :file moved :fail false)
        (die ag)))))

(defn get-tag
  ([ag] (get-tag nil ag))
  ([pattern ag]
     (when-let [#^URI link (ag :link)]
       (when-let [tag (or (if pattern
                            (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                                  (if (sequential? pattern) pattern [pattern])))
                          (.getHost link))]
         (assoc ag :tag tag :fail false)))))

(defn get-length [ag]
  (when-let [#^URI link (ag :link)]
    (let [#^HttpClient client (new HttpClient)
          #^HeadMethod head (HeadMethod. (str link))]
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout *default-connection-timeout*))
      (.. head getParams (setSoTimeout *default-head-request-timeout*))
      (try (let [status (.executeMethod client head)]
             (.releaseConnection head)
             (if (= status HttpStatus/SC_OK)
               (if-let [length (.. head (getResponseHeader "Content-Length") (getValue))]
                 (assoc ag :length (Integer/parseInt length) :fail false)
                 (do (log/info (str "Невозможно узнать размер файла " (ag :name)))
                     (die ag)))
               ((http-error-status-handler status die fail) ag)))
           (catch java.io.InterruptedIOException e
             (do (log/info (str "Время ожидания ответа сервера истекло для " (ag :name)))
                 (fail ag)))
           (catch java.net.ConnectException e (die ag))
           (catch Exception e (die ag))
           (finally (.releaseConnection head))))))

(defn get-file [ag]
  (when-let [name (ag :name)]
    (when-let [#^File working-path ((deref (related-env ag)) :working-path)]
      (assoc ag :file (new File (join-paths working-path name)) :fail false))))

(def *buffer-size* 4096)

(defn download [ag]
  (let [progress-agent (:progress-agent (deref (related-env ag)))
        #^URI link (:link ag)
        #^File file (:file ag)
        #^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str link))]
    (when (and link file)
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout action/*default-connection-timeout*))
      (.. get getParams (setSoTimeout action/*default-get-request-timeout*))
		(.setRequestHeader get "Range" (str "bytes=" (file-length file) \-))
      (try (.executeMethod client get)
           (let [content-length (.getResponseContentLength get)]
             (cond (not content-length)
                   (do (log/info (str "Невозможно проверить файл перед загрузкой " (ag :name)))
                       (fail ag))

                   (not= content-length (- (ag :length) (file-length file)))
                   (do (log/info (str "Размер получаемого файла не совпадает с ожидаемым " (ag :name)))
                       (fail ag))

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
                 (fail ag)))
           (catch InterruptedIOException e
             (do (log/info (str "Время ожидания ответа сервера истекло для " (ag :name)))
                 (fail ag)))
           (catch NoHttpResponseException e
             (do (log/info (str "Сервер не отвечает на запрос для " (ag :name)))
                 (fail ag)))
           (catch Exception e 
             (do (log/info (str "Ошибка во время загрузки " (ag :name)))
                 (fail ag)))
           (finally (when progress-agent
                      (send progress-agent progress/hide-progress (:tag ag)))
                    (.releaseConnection get))))))