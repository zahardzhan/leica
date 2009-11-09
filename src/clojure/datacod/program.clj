;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров, Александр Золотов"}
  datacod.program
  (:require :reload datacod.account)
  (:use aux match)
  (:import (org.apache.commons.httpclient HttpClient HttpStatus)
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

(defn reflex-upload
  "Простая рефлексная программа агента для заливки."
  [percept]
  (letfn [(out-of-space ;; недостаточно места на сервере
           [percept] 
           (let [client (new HttpClient)]
             (datacod.account/with-auth client ((percept :env) :account)
               (let [free-space (datacod.account/free-space client)]
                 (if free-space
                   (< free-space ((percept :self) :length))
                   true)))))
          (missing [key] (fn [percept] (not ((percept :self) key))))
          (has [key] (fn [percept] ((percept :self) key)))
          (otherwise [_] true)]
    (match percept
           [[(missing :actions) :die]
            [(missing :file)    :die]
            [(missing :name)    :die]
            [(missing :length)  :die]
            [(has     :address) :die]
            [out-of-space       :die]
            [otherwise          :upload]])))