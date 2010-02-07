;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Работа с аккаунтом data.cod.ru."
       :author "Роман Захаров"}
  service.cod.data.account
  (:require [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux clojure.contrib.seq-utils)
  (:import (java.io File FileOutputStream)

           (org.apache.commons.httpclient URI HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod PostMethod)
           (org.apache.commons.httpclient.methods.multipart
            FilePart MultipartRequestEntity Part StringPart)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'service.cod.data.account)

(defn datacod-account [domain login password]
  (when (and login password)
    {:domain (str (when domain (str domain ".")) "data.cod.ru")
     :login login :password password}))

(defmacro with-auth
  "Авторизация http-клиента на датакоде."
  [#^HttpClient client account & body]
  `(let [#^PostMethod post# (new PostMethod "http://nvwh.cod.ru/link/auth/")]
     (doto post#
       (.addParameter "refURL" (str "http://" (~account :domain)))
       (.addParameter "email" (~account :login))
       (.addParameter "password" (~account :password)))
     (try (when (= HttpStatus/SC_OK (.executeMethod ~client post#))
            (.releaseConnection post#)
            ~@body)
          (catch Exception exc# nil)
          (finally (.releaseConnection post#)))))

(defn parse-page
  "Парсит текст страницы датакода."
  [page]
  {:link (URI. (re-find #"http://files[-\d\w\.]*data.cod.ru/[^\"]+" page) true)
   :name (second (re-find #"<b title=\".*\">(.*)</b>" page))
   :space (let [[_ space unit]
                (re-find #"Вам доступно ([\d\.]+) (\p{javaUpperCase}{2})" page)]
            (when (and space unit)
              (int (* (Float/parseFloat space)
                      ({"ГБ" 1073741824, "МБ" 1048576, "КБ" 1024} unit)))))})

(defn free-space
  "Свободное место на датакод-аккаунте.
   Использовать после авторизации на датакоде."
  [#^HttpClient client account]
  (try
   (let [#^GetMethod get (GetMethod. (str "http://" (account :domain)))]
     (when (= HttpStatus/SC_OK (.executeMethod client get))
       (:space (parse-page
                (EncodingUtil/getString
                 (duck/slurp* (.getResponseBodyAsStream get)) "UTF-8")))))
   (catch Exception exc nil)
   (finally (.releaseConnection get))))