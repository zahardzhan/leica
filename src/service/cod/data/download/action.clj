;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Действия агента, скачивающего с data.cod."
       :author "Роман Захаров"}
  service.cod.data.download.action
  (:require action download.action
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux action env [service.cod.data.account :only (parse-page)])
  (:import (java.io File InterruptedIOException)
           java.net.ConnectException
           (org.apache.commons.httpclient 
            URI HttpClient HttpStatus ConnectTimeoutException NoHttpResponseException)
           (org.apache.commons.httpclient.methods GetMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)))

(in-ns 'service.cod.data.download.action)

(defn get-link-and-name [{:as ag :keys [#^URI address]}]
  (let [#^HttpClient client (new HttpClient)
        #^GetMethod get (GetMethod. (str address))]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout download.action/default-connection-timeout))
    (.. get getParams (setSoTimeout download.action/default-get-request-timeout))
    (try (let [status (.executeMethod client get)]
           (if (= status HttpStatus/SC_OK)
             (let [parsed (parse-page (duck/slurp* (.getResponseBodyAsStream get)))]
               (if (and (parsed :name) (parsed :link))
                 (assoc ag :name (parsed :name) :link (parsed :link) :fail false)
                 (do (log/info (str "Невозможно получить имя файла и ссылку с адреса " address))
                     (die ag))))               
             ((http-error-status-handler status die fail) ag)))
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