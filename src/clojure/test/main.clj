;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  test.main
  (:use :reload aux match env env.download env.upload rules)
  (:use clojure.test [clojure.contrib seq-utils])
  (:require :reload action program
            datacod.account datacod.action datacod.program
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:import (java.io File FileOutputStream InputStream)
           (java.util Date)
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)
           (org.apache.commons.httpclient URI HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'test.main)

(deftest download-test
  (let [e (download-environment {:working-path (File. "/home/haru/inbox/dsv")})
        a (download-agent "http://dsv.data.cod.ru/507882" *download-rules*)]
    (add-agent e a)
    (await e)
    
    (println "Тест скачивания файла:")
    (loop [ag @a]
      (println "Последнее действие:" \space (ag :action))
      (let [new-ag (action/execute-action ag {:self ag})]
        (if (dead? new-ag)
          (do (is (action/after :successful :download new-ag))
              (when (.exists (ag :file)) (.delete (ag :file))))
          (recur new-ag))))))

(deftest bind-test
  (let [make-e (fn [] (agent {:a nil}))
        make-a (fn [] (agent {:e nil}))
        bindea (fn [e a]
                 (send a assoc :e (fn [] e))
                 (send e assoc :a (fn [] a)))
        e (make-e)
        a (make-a)]
    
    (bindea e a)
    
    (is (= a (((deref e) :a))))))