;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Действия агента, работающего с data.cod."
       :author "Роман Захаров"}
  datacod.action
  (:require datacod.account
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux)
  (:import (java.io File)
           (org.apache.commons.httpclient URI HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod PostMethod)
           (org.apache.commons.httpclient.methods.multipart
            FilePart MultipartRequestEntity Part StringPart)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'datacod.action)

(defn get-link-and-name [ag env]
  (when-let [#^URI address (ag :address)]
    (let [#^HttpClient client (new HttpClient)
          #^GetMethod get (GetMethod. (str address))]
      ;; Use HttpConnectionParams.setConnectionTimeout(int), HttpConnectionManager.getParams().
      (try (let [status (.executeMethod client get)]
             (if (= status HttpStatus/SC_OK)
               (let [parsed (datacod.account/parse-page
                             (duck/slurp* (.getResponseBodyAsStream get)))]
                 (if (and (parsed :name) (parsed :link))
                   (assoc ag :name (parsed :name) :link (parsed :link) :fail false)
                   (action/die ag env)))
               ((http-error-status-handler status action/die action/fail) ag env)))
           (catch java.net.ConnectException e (action/die ag env))
           (catch Exception e (action/fail ag env))
           (finally (.releaseConnection get))))))

(defn report-and-die [ag env]
  (when-let [#^File report-file (env :report-file)]
    (duck/with-out-append-writer report-file
      (print (format-link-for-forum (ag :name) (ag :address)))))
  (action/die ag env))

(defn upload [ag env]
  ;; 14:37:30 I/O exception (java.net.ConnectException) caught when processing request: Connection timed out
  ;; 14:37:30 Retrying request
  (let [domain ((env :account) :domain)
        referer (str "http://" domain "/cabinet/upload/")
        #^HttpClient client (new HttpClient)
        #^PostMethod post (PostMethod. referer)
        parts (into-array Part
                          [(StringPart. "action" "file_upload")
                           (FilePart.   "sfile"  (transliterate (ag :name))
                                        (ag :file))
                           (StringPart. "agree"  "1")
                           (StringPart. "password" (or (ag :password) ""))
                           (StringPart. "description" (or (ag :description) ""))])]
    (datacod.account/with-auth client (env :account)
      (.addRequestHeader post "Referer" referer)
      (try (.setRequestEntity
            post (MultipartRequestEntity. parts (.getParams post)))
           (log/info (str "Начата загрузка " (ag :name)))
           (if (= HttpStatus/SC_MOVED_TEMPORARILY (.executeMethod client post))
             (if-let [location (.. post (getResponseHeader "Location") (getValue))]
               (do (log/info (str "Закончена загрузка " (ag :name)))
                   (assoc ag :address (str "http://" domain location) :fail false :alive false))
               (do (log/info (str "Загрузка не может быть закончена " (ag :name)))
                   (action/die ag env)))
             (do (log/info (str "Прервана загрузка " (ag :name)))
                 (action/fail ag env)))
           (catch Exception exc
             (do (log/info (str "Прервана загрузка " (ag :name)))
                 (action/fail ag env)))
           (finally (.releaseConnection post))))))

