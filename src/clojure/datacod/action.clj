;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Действия агента, работающего с data.cod."
       :author "Роман Захаров"}
  datacod.action
  (:require action
            datacod.account
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

(in-ns 'datacod.action)

(defn get-link-and-name [ag]
  (when-let [#^URI address (ag :address)]
    (let [#^HttpClient client (new HttpClient)
          #^GetMethod get (GetMethod. (str address))]
      (try (let [status (.executeMethod client get)]
             (if (= status HttpStatus/SC_OK)
               (let [parsed (datacod.account/parse-page
                             (duck/slurp* (.getResponseBodyAsStream get)))]
                 (if (and (parsed :name) (parsed :link))
                   (assoc ag :name (parsed :name) :link (parsed :link) :fail false)
                   (do (log/info (str "Невозможно получить имя файла и ссылку с адреса " address))
                       (action/die ag))))
               ((http-error-status-handler status action/die action/fail) ag)))
           (catch ConnectTimeoutException e
             (do (log/info (str "Время ожидания соединения с сервером истекло для " address))
                 (action/fail ag)))
           (catch InterruptedIOException e
             (do (log/info (str "Время ожидания ответа сервера истекло для " address))
                 (action/fail ag)))
           (catch NoHttpResponseException e
             (do (log/info (str "Сервер не отвечает на запрос для " address))
                 (action/fail ag)))
           (catch Exception e 
             (do (log/info (str "Ошибка во время получения имени файла и ссылки с адреса " address))
                 (action/fail ag)))
           (finally (.releaseConnection get))))))

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