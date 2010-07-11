;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns download
  (:use :reload aux agent)
  (:require [clojure.contrib.logging :as log]
            [clojure.contrib.duck-streams :as duck]

            fn
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

(def *services* (ref {}))

(defn defservice [name & {:as opts :keys [address hosts]}]
  (dosync (alter *services* assoc name {:address address
                                        :hosts hosts})))

(defn remove-service [name]
  (dosync (alter *services* dissoc name)))

(defmacro access-service [service & accessors]
  `(derefed *services* ~service ~@accessors))

(defn match-service [line]
  {:pre  [(string? line)]}
  (when-let [url (extract-url line)]
    (rule-based-translator url (seq @*services*)
                           :rule-pattern (comp :address val)
                           :rule-response key
                           :matcher re-find
                           :action (fn [result rule-response]
                                     {:address result,
                                      :service rule-response}))))

(defservice ::data.cod.ru
  :address #"http://[\w\.\-]*.data.cod.ru/\d+"
  :hosts #{#"files3?.dsv.*.data.cod.ru" #"files2.dsv.*.data.cod.ru"})

(defservice ::77.35.112.8*
  :address #"http://77.35.112.8[1234]/.+")

(defservice ::dsvload.net.ftpupload
  :address #"http://dsvload.net/ftpupload/.+")

(defprotocol Download-Environment-Protocol)

(defrecord Download-Environment [agents-ref]
  Environment-Protocol
  (done? [e] (every? dead? (agents e)))
  (terminate [e] e))

(defn make-download-env [& {:keys [agents]}]
  {:pre [(when-supplied agents (seq? (sequence agents)))]}
  (new Download-Environment (ref (set agents))))

(defprotocol Download-Agent-Protocol
  (status  [a])
  (status! [a new-status])
  (status? [a in-status])
  (size    [a])
  (dead?   [a])
  (alive?  [a])
  (run?    [a])
  (does-nothing? [a])
  (ok?     [a])
  (idle?   [a])
  (fail?   [a])
  (pass    [a])
  (idle    [a])
  (fail    [a])
  (die     [a])
  (sleep   [a millis])
  (out-of-space-on-work-path? [a])
  (out-of-space-on-done-path? [a])
  (fully-loaded?              [a])
  (already-on-done-path?      [a]))

(defrecord Download-Agent [env-ref precedence status-atom
                           service strategy address size-atom
                           working-path done-path
                           host link file length]
  Agent-Protocol
  (get-action  [a] (strategy a))
  (execute     [a action] (if (dead? a) a
                              (try (status! a :running)
                                   (try (let [a- (action a)]
                                          (cond (run? a-) (status! a- :idle)
                                                :else a-))
                                        (catch Exception _ (status! a :fail)))
                                   (catch AssertionError _ a))))
  (run         [a] (execute a (get-action a)))
  (performance [a])

  Download-Agent-Protocol
  ;; (status  [a] (deref status-atom))
  ;; (status! [a new-status]
  ;;          (when-not (({:idle    #{:running}
  ;;                       :fail    #{:running}
  ;;                       :dead    #{}
  ;;                       :running #{:stoping :idle :fail :dead}
  ;;                       :stoping #{:idle :fail :dead}}
  ;;                      (status a)) new-status)
  ;;            (throw (new AssertionError
  ;;                        "Download agent cannot change state.")))
  ;;          (with-return a (reset! status-atom new-status)))
  ;; (status? [a in-status]
  ;;          (case (type in-status)
  ;;                clojure.lang.Keyword (= in-status (status a))
  ;;                clojure.lang.PersistentHashSet (keyword? (in-status (status a)))))
  (size    [a] (deref size-atom))
  ;; (dead?   [a] (status? a :dead))
  ;; (alive?  [a] (not (dead? a)))
  ;; (run?    [a] (status? a #{:running :stoping}))
  ;; (does-nothing? [a] (status? a #{:idle :fail}))
  ;; (ok?     [a] (status? a #{:idle :running :stoping}))
  ;; (idle?   [a] (status? a :idle))
  ;; (fail?   [a] (status? a :fail))
  ;; (pass    [a] a)
  ;; (idle    [a] (status! a :idle))
  ;; (fail    [a] (status! a :fail))
  ;; (die     [a] (status! a :dead))
  ;; (sleep   [a millis] (with-return a (Thread/sleep millis)))
  ;; (out-of-space-on-work-path? [a] (when (and working-path length file)
  ;;                                   (< (.getUsableSpace working-path)
  ;;                                      (- length (file-length file)))))
  ;; (out-of-space-on-done-path? [a] (when (and done-path length)
  ;;                                   (< (.getUsableSpace done-path) length)))
  ;; (fully-loaded?              [a] (when (and length file)
  ;;                                   (<= length (file-length file))))
  ;; (already-on-done-path?      [a] (when (and done-path file)
  ;;                                   (.exists (File. done-path (.getName file)))))
  )

(def *precedence* (atom 0))

(defn make-download-agent
  [address-line & {:as opts
                   :keys [strategy precedence environment
                          working-path done-path link file]}]

  {:pre  [(when-supplied strategy     (invocable? strategy)
                         precedence   (number? precedence)
                         working-path (verified/output-dir working-path)
                         done-path    (verified/output-dir done-path)
                         file         (file? file)
                         environment  (instance? download.Download-Environment environment))]}

  (let [{:keys [service address]}
        (when address-line (match-service address-line))]

    (when (and service address)
      (let-return [a (agent (new Download-Agent
                                 (delay (ref environment)) ;; env-ref
                                 (or precedence (swap! *precedence* inc)) ;; precedence
                                 (atom :idle) ;; status-atom
                                 service
                                 strategy
                                 address
                                 (atom nil)
                                 working-path
                                 (when (not= working-path done-path) done-path)
                                 nil ;; host
                                 nil ;; link
                                 nil ;; file
                                 nil ;; length
                                 ))]
                  (when environment (bind a environment))))))

(defn- dispatch-by-service [a & opts]
  (:service a))

(defmulti parse-page        dispatch-by-service)
(defmulti get-host          dispatch-by-service)
(defmulti get-file          dispatch-by-service)
(defmulti get-length        dispatch-by-service)
(defmulti move-to-done-path dispatch-by-service)
(defmulti download          dispatch-by-service)

(defmulti reflex            dispatch-by-service)
(defmulti reflex-with-transfer-of-control dispatch-by-service)

(def buffer-size          4096)
(def timeout-after-fail   3000)
(def connection-timeout   30000)
(def head-request-timeout 30000)
(def get-request-timeout  30000)

;; (defmethod console-progress ::download-agent
;;   [{:as a :keys [name length]}]
;;   (let [size       (size a)
;;         percent    #(int (if (and (pos? length) (pos? size))
;;                            (* 100 (/ size length))
;;                            0))
;;         first-part #(apply str (take 5 name))
;;         last-part  #(apply str (take-last 7 name))]
;;     (cond (and name length size)
;;           (str \[ (first-part) ".." (last-part) \space (percent) \% \])

;;           :else nil)))

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
  (select-items :from (agents (env a)) :entirely-after a
                :order-by #(derefed % :precedence)
                :where #(and (agent? %)
                             (derefed % alive?)
                             (derefed % :host not))))

(defn idle-download-agents-with-same-host-as [a]
  (select-items :from (agents (env a)) :order-by #(derefed % :precedence) 
                :where #(and (agent? %) (derefed % idle?)
                             (same :host (deref a) (deref %)))))

(defn alive-download-agents-with-same-host-after [a]
  (select-items :from (agents (env a)) :entirely-after a
                :order-by #(derefed % :precedence)
                :where #(and (agent? %) (derefed % alive?)
                             (same :host (deref a) (deref %)))))

(defn download-agents-running-on-same-host-as [a]
  (select-items :from (surrounding a) :order-by #(derefed % :precedence)
                :where #(and (agent? %) (derefed % run?)
                             (same :host (deref a) (deref %)))))

(defmethod reflex-with-transfer-of-control :default [a & opts]
  (let [done #(or (send-off (or (first (idle-download-agents-with-same-host-as *agent*))
                                (first (alive-download-agents-with-same-host-after *agent*)))
                            run)
                  (when-let [e (env *agent*)] (done? e) (terminate e)))]
    
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
                   link (re-find #"http://files[-\d\w\.]*data.cod.ru/[^\"]+" page)
                   name (second (re-find #"<b title=\".*\">(.*)</b>" page))
                   space (let [[_ space unit]
                               (re-find #"Вам доступно ([\d\.]+) (\p{javaUpperCase}{2})" page)]
                           (when (and space unit)
                             (int (* (Float/parseFloat space)
                                     ({"ГБ" 1073741824, "МБ" 1048576, "КБ" 1024} unit)))))]
               (if (and name link)
                 (idle (assoc ag :name name :link (URI. link true)))
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
  [{:as ag :keys [name host length link size file]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str link))]
    (when (and link file)
      (try
       (.. client getHttpConnectionManager getParams 
           (setConnectionTimeout connection-timeout))
       (.. get getParams (setSoTimeout get-request-timeout))
       (.setRequestHeader get "Range" (str "bytes=" (file-length file) \-))
       (.executeMethod client get)
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
                   (loop [file-size (file-length file)]
                     (let [read-size (.read input buffer)]
                       (when (pos? read-size)
                         (let [new-size (+ file-size read-size)]
                           (.write output buffer 0 read-size)
                           (reset! size new-size)
                           (recur new-size)))))
                   (.flush output))
                 (log/info (str "Закончена загрузка " name))
                 (idle ag))))
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
       (finally (do (.releaseConnection get)))))))

(comment
  (def de (make-download-env))
  (def da1 (make-download-agent "http://dsv.data.cod.ru/778222"
                                :working-path (File. "/home/haru/Inbox/")
                                :strategy reflex-with-transfer-of-control
                                :environment de))
  de
  da1
  (send-off da1 run)
  (run @da5)
  (run (run (run (run (run (run @da4))))))
  )
