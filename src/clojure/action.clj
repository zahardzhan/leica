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
           (org.apache.commons.httpclient.methods GetMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

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
 
(defn get-tag [pattern ag env]
  (when-let [#^URI link (ag :link)]
    (when-let [tag (or (if pattern
                         (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                               (if (sequential? pattern) pattern [pattern])))
                       (.getHost link))]
      (assoc ag :tag tag :fail false))))
 
(defn get-length [ag env]
  (when-let [#^URI link (ag :link)]
    (let [length-request (ha/http-agent link :method "HEAD")]
      (ha/result length-request)
      (if (and (ha/done? length-request) (ha/success? length-request))
        (if-let [length (:content-length (ha/headers length-request))]
          (assoc ag :length (Integer/parseInt length) :fail false)
          (die ag env))
        ((http-error-status-handler length-request
                                    die fail) ag env)))))
 
(defn get-file [ag env]
  (when-let [name (ag :name)]
    (when-let [#^File working-path (env :working-path)]
      (assoc ag :file (new File (join-paths working-path name)) :fail false))))
 
(defn download [ag env]
  (when-let [#^URI link (ag :link)]
    (when-let [#^File file (ag :file)]
      (let [loader (ha/http-agent
                    link
                    :headers {"Range" (str "bytes=" (file-length file) "-")}
                    :handler (fn [remote]
                               (with-open [local (FileOutputStream. file true)]
                                 (duck/copy (ha/stream remote) local))))]
        (log/info (str "Начата загрузка " (ag :name)))
        (ha/result loader)
        (if (and (ha/done? loader) (ha/success? loader))
          (do (log/info (str "Закончена загрузка " (ag :name)))
              (die ag env))
          ((http-error-status-handler
            loader
            #(do (log/info (str "Загрузка не может быть закончена " (ag :name)))
                 (die %1 %2))
            #(do (log/info (str "Прервана загрузка " (ag :name)))
                 (fail %1 %2))) ag env))))))