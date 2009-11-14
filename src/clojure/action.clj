;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Базовые действия агентов."
       :author "Роман Захаров"}
  action
  (:require [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux match)
  (:import (java.io File FileOutputStream)
           (org.apache.commons.httpclient URI HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'action)

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

(defn get-tag [pattern ag env]
  (when-let [#^URI link (ag :link)]
    (when-let [tag (or (if pattern
                         (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                               (if (sequential? pattern) pattern [pattern])))
                       (.getHost link))]
      (assoc ag :tag tag :fail false))))
 
(defn get-length [ag env]
  (when-let [#^URI link (ag :link)]
    (let [#^HttpClient client (new HttpClient)
          #^HeadMethod head (HeadMethod. (str link))]
      (try (let [status (.executeMethod client head)]
             (.releaseConnection head)
             (if (= status HttpStatus/SC_OK)
               (if-let [length (.. head (getResponseHeader "Content-Length") (getValue))]
                 (assoc ag :length (Integer/parseInt length) :fail false)
                 (die ag env))
               ((http-error-status-handler status die fail) ag env)))
           (catch java.net.ConnectException e (die ag env))
           (catch Exception e (fail ag env))
           (finally (.releaseConnection head))))))

(defn get-file [ag env]
  (when-let [name (ag :name)]
    (when-let [#^File working-path (env :working-path)]
      (assoc ag :file (new File (join-paths working-path name)) :fail false))))

(defn download [ag env]
  (when-let [#^URI link (ag :link)]
    (when-let [#^File file (ag :file)]
      (try
       (let [loader (ha/http-agent
                     (str link)
                     :headers {"Range" (str "bytes=" (file-length file) "-")}
                     :handler (fn [remote]
                                (with-open [local (FileOutputStream. file true)]
                                  (duck/copy (ha/stream remote) local))))]
         (log/info (str "Начата загрузка " (ag :name)))
         (ha/result loader)
         (if (and (ha/done? loader) (ha/success? loader))
           (do (log/info (str "Закончена загрузка " (ag :name)))
               (assoc ag :fail false))
           ((http-error-status-handler
             (ha/status loader)
             #(do (log/info (str "Загрузка не может быть закончена " (ag :name)))
                  (die %1 %2))
             #(do (log/info (str "Прервана загрузка " (ag :name)))
                  (fail %1 %2))) ag env)))
       (catch java.net.SocketException e (die ag env))
       (catch java.net.ConnectException e (die ag env))
       (catch Exception e (fail ag env))))))
