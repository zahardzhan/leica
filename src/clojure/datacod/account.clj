;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Работа с аккаунтом data.cod.ru."
       :author "Роман Захаров"}
  datacod.account
  (:require [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux match clojure.contrib.seq-utils)
  (:import (java.io File FileOutputStream)
           (java.net URI)

           (org.apache.commons.httpclient HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod PostMethod)
           (org.apache.commons.httpclient.methods.multipart
            FilePart MultipartRequestEntity Part StringPart)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)

           (org.htmlparser Parser)
           (org.htmlparser.util ParserException)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags Div LinkTag)
           (org.htmlparser.nodes TagNode)))

(defmacro with-auth
  "Авторизация http-клиента на датакоде."
  [#^HttpClient client account & body]
  `(let [#^PostMethod post# (new PostMethod "http://nvwh.cod.ru/link/auth/")]
     (doto post#
       (.addParameter "refURL" "http://dsv.data.cod.ru")
       (.addParameter "email" (~account :login))
       (.addParameter "password" (~account :password)))
     (try (when (= HttpStatus/SC_OK (.executeMethod ~client post#))
            (.releaseConnection post#)
            ~@body)
          (catch Exception exc# nil)
          (finally (.releaseConnection post#)))))
  ;; } catch (HttpException e) {
  ;;     System.err.println("Fatal protocol violation: " + e.getMessage());
  ;;     e.printStackTrace();
  ;;   } catch (IOException e) {
  ;;     System.err.println("Fatal transport error: " + e.getMessage());
  ;;     e.printStackTrace();
  ;;   } finally {
  ;;     // Release the connection.
  ;;     method.releaseConnection();

(defn parse-page
  "Парсит текст страницы датакода."
  [page]
  (let [name (atom nil)
        link (atom nil)
        space (atom nil)
        parser (Parser/createParser page nil)
        visitor
        (proxy [NodeVisitor] []
          (visitTag
           [tag]
           (cond (instance? LinkTag tag)
                 (when-let [parsed-link (re-find #"http://files[\d\w\.]*data.cod.ru/.+"
                                              (.extractLink tag))]
                   (reset! link parsed-link))

                 (and (instance? TagNode tag) (= "b" (.getRawTagName tag)))
                 (when-let [parsed-name (.getAttribute tag "title")]
                   (reset! name parsed-name))

                 (instance? Div tag)
                 (let [[_ space-str unit-str]
                       (re-find #"Вам доступно ([\d\.]+) (\p{javaUpperCase}{2})" 
                                (.getStringText tag))]
                   (when (and space-str unit-str)
                     (let [space-val (Float/parseFloat space-str)
                           unit-val ({"ГБ" 1073741824 "МБ" 1048576 "КБ" 1024} unit-str)]
                       (reset! space (int (* space-val unit-val)))))))))]
    (try (.visitAllNodesWith parser visitor)
         {:name @name :link (when @link (new URI @link)) :space @space}
         (catch ParserException _ nil))))

(defn free-space
  "Свободное место на датакод-аккаунте.
   Использовать после авторизации на датакоде."
  [#^HttpClient client]
  (try
   (let [#^GetMethod get (GetMethod. "http://dsv.data.cod.ru")]
     (when (= HttpStatus/SC_OK (.executeMethod client get))
       (:space (parse-page
                (EncodingUtil/getString
                 (duck/slurp* (.getResponseBodyAsStream get)) "UTF-8")))))
   (catch Exception exc nil)
   (finally (.releaseConnection get))))