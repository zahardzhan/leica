;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns download.agent
  (:use :reload agent aux)
  (:require [clojure.contrib.logging :as log]) ;; progress
  (:require :reload service.cod.data.download.action)
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

(derive ::download :env/dummy)

(defn download-agent
  [& state]
  (let [{:keys [rules line working-path done-path]}
        (apply hash-map state)]
    (make-agent :aim ::download
                ;; :address (URI. address)
                :link nil
                :file nil
                :length nil
                :working-path working-path
                :done-path done-path)))

(def default-download-agent-control-part 
     {:program download.program/reflex
      :actions {:get-link          download.action/get-link
                :get-name          download.action/get-name
                :get-tag           download.action/get-tag
                :get-file          download.action/get-file
                :get-length        download.action/get-length
                :move-to-done-path download.action/move-to-done-path
                :download          download.action/download
                :die               action/die
                :pass              action/pass}})

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     download-rules
     [[#"http://[\w\.\-]*.data.cod.ru/\d+"
       (merge-with merge default-download-agent-control-part 
                   {:actions {:get-link service.cod.data.download.action/get-link-and-name
                              :get-tag  (partial download.action/get-tag [#"files3?.dsv.*.data.cod.ru"
                                                                          #"files2.dsv.*.data.cod.ru"])}})]
      [#"http://77.35.112.8[1234]/.+" default-download-agent-control-part]
      [#"http://dsvload.net/ftpupload/.+" default-download-agent-control-part]])


(defn pass [ag] ag)

(defn ok [ag] (assoc ag :fail false))
 
(defn fail [ag] (assoc ag :fail true))
 
(defn die [ag] (assoc ag :alive false))

(defn sleep [ag millis]
  (with-return ag
    (Thread/sleep millis)))

(defn lock-tag "Замкнуть (заблокировать) таг агента."
  [ag] (reset! (tag-lock ag) true))

(defn unlock-tag "Снять замок (блокировку) с тага агента." 
  [ag] (reset! (tag-lock ag) false))

(defn tag-locked-in-env? 
  "Замкнут (заблокирован) ли хоть один агент в окружении с таким же тагом?"
  [ag] (or (tag-locked? ag)
           (some (fn-and (partial same tag ag) tag-locked? (constantly true))
                 (env ag))))

(defmacro with-locked-tag
  "Замыкает (блокирует) таг агента на время выполнения им действия, блокирующего
  действия других агентов в окружении с таким же тагом."
  [ag & body]
  `(when-not (tag-locked-in-env? ~ag)
     (lock-tag ~ag)
     (let [result# (do ~@body)]
       (unlock-tag ~ag)
       result#)))



(defn out-of-space-on-work-path? [ag]
  (when (and (working-path ag) (length ag) (file ag))
    (< (.getUsableSpace (working-path ag))
       (- (length ag) (file-length (file ag))))))

(defn out-of-space-on-done-path? [ag]
  (when (and (done-path ag) (length ag))
    (< (.getUsableSpace (done-path ag)) (length ag))))

(defn fully-loaded? [ag]
  (when (and (length ag) (file ag))
    (<= (length ag) (file-length (file ag)))))

(defn already-on-done-path? [ag]
  (when (and (done-path ag) (file ag))
    (.exists (File. (done-path ag) (.getName (file ag))))))


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

(defn next-alive-untagged-after [ag]
  (next-after-when (fn-and alive? (no tag) (partial same type-dispatch ag))
                   (self ag) (env ag)))

(defn alive-unfailed-with-same-tag [ag]
  (some (fn-and alive? (no fail?) (partial same tag ag) identity)
        (env ag)))

(defn next-alive-with-same-tag-after [ag]
  (next-after-when (fn-and alive? (partial same tag ag))
                   (self ag) (env ag)))

(defmethod done ::download-agent [ag]
  (or (when-not (debug? ag) (run (or (alive-unfailed-with-same-tag ag)
                                     (next-alive-with-same-tag-after ag))))
      (when (termination? ag) (terminate ag)))
  ag)

(def timeout-after-fail 3000)

(defmethod run ::download [ag]
  (let [reflex (fn [ag]
                 (cond (dead? ag)             :pass
                       (no :address ag)  :die
                       (no :actions ag)  :die
                       (no :link ag)     :get-link
                       (no :tag ag)      :get-tag
                       (no :name ag)     :get-name
                       (no :file ag)     :get-file
                       (already-on-done-path? ag) :die
                       (no :length ag)   :get-length
                       (out-of-space-on-work-path? ag) :die
        
                       (fully-loaded? ag) 
                       (cond (out-of-space-on-done-path? ag) :die
                             (:done-path ag)  :move-to-done-path
                             :else :die)

                       :else                   :download))]
    (cond (dead? ag) ag

          ((no tag) ag)
          (let [new-state (action/percept-and-execute ag)]
            (cond (dead? new-state) (done *agent*)
                  (fail? new-state) (do (sleep *agent* timeout-after-fail)
                                        (when-not (debug? ag) (run *agent*)))
                  (tag new-state) (when-not (debug? ag)
                                    (run (next-alive-untagged-after ag))
                                    (run *agent*))
                  :else (when-not (debug? ag) (run *agent*)))
            new-state)

          (tag-locked-in-env? ag) ag

          :else (let [new-state 
                      (with-locked-tag ag (action/percept-and-execute ag))]
                  (cond (dead? new-state) (done *agent*)
                        (fail? new-state) (do (sleep *agent* timeout-after-fail)
                                              (done *agent*))
                        :else (when-not (debug? ag) (run *agent*)))
                  new-state))))