;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Действия агента, работающего с data.cod."
       :author "Роман Захаров"}
  service.cod.data.action
  (:require action
            service.cod.data.account
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux env)
  (:import (java.io File InterruptedIOException)
           (org.apache.commons.httpclient 
            URI HttpClient HttpStatus ConnectTimeoutException NoHttpResponseException)
           (org.apache.commons.httpclient.methods GetMethod PostMethod)
           (org.apache.commons.httpclient.methods.multipart
            FilePart MultipartRequestEntity Part StringPart)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'service.cod.data.action)

(defn report [ag]
  (when-let [#^File report-file ((deref (related-env ag)) :report-file)]
    (duck/with-out-append-writer report-file
      (print (format-link-for-forum (ag :name) (ag :address)))))
  ag)

(defn upload [ag]
  (let [domain (((deref (related-env ag)) :account) :domain)
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
    (datacod.account/with-auth client ((deref (related-env ag)) :account)
      (.addRequestHeader post "Referer" referer)
      (try (.setRequestEntity
            post (MultipartRequestEntity. parts (.getParams post)))
           (log/info (str "Начата загрузка " (ag :name)))
           (if (= HttpStatus/SC_MOVED_TEMPORARILY (.executeMethod client post))
             (if-let [location (.. post (getResponseHeader "Location") (getValue))]
               (do (log/info (str "Закончена загрузка " (ag :name)))
                   (assoc ag :address (str "http://" domain location) :fail false))
               (do (log/info (str "Загрузка не может быть закончена " (ag :name)))
                   (action/die ag)))
             (do (log/info (str "Прервана загрузка " (ag :name)))
                 (action/fail ag)))
           (catch Exception exc
             (do (log/info (str "Прервана загрузка " (ag :name)))
                 (action/fail ag)))
           (finally (.releaseConnection post))))))