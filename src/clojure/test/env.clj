;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты окружения."
       :author "Роман Захаров"}
  test.env
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

(in-ns 'test.env)

(deftest add-agent-test
  (let [e1 (download-environment {:working-path (File. "/home/haru/inbox/dsv")})
        e2 (download-environment {:working-path (File. "/home/haru/inbox/dsv")})
        ags (download-agents ["http://dsv.data.cod.ru/507882"
                              "http://dsv.data.cod.ru/507882"
                              "http://dsv.data.cod.ru/507883"]
                             *download-rules*)]
    (add-agent e2 (first ags))
    (add-agent e2 (second ags))
    (add-agent e2 (first (rest (rest ags)))) 
    (await e2)
    (is (= 2 (count (agents e2))))

    (add-agents e1 ags)
    (await e1)
    (is (= 2 (count (agents e1))))))

(deftest download-test ;; add :done-path test
  (let [e (download-environment {:working-path (File. "/home/haru/inbox/dsv")
                                 :debug true})
        a (download-agent "http://dsv.data.cod.ru/507882" *download-rules*)]
    (add-agent e a)
    (await e)
    
    (println "Тест скачивания файла:")
    (loop []
      (println "Последнее действие:" \space (@a :action))
      (run-agent a)
      (await a)
      (if (dead? a)
        (do (is (action/after :successful :download @a))
            (when (.exists (@a :file)) (.delete (@a :file))))
        (recur)))))

(deftest upload-test
  (let [account (datacod.account/datacod-account
                 "dsv" "zahardzhan@gmail.com" "zscxadw")
        file (File. "/home/haru/inbox/dsv/.jobs")
        ;; report-file (verified-log-file report)
        e (upload-environment account
                              {;:report-file report-file
                               :debug true})
        a (upload-agent file *upload-rules*)]
    (add-agent e a)
    (await e)
    
    (println "Тест закачивания файла:")
    (loop []
      (println "Последнее действие:" \space (@a :action))
      (run-agent a)
      (await a)
      (if (dead? a)
        (is (action/after :report @a))
        (recur)))))

(deftest same-type-dispatch-test
  (are [x y] (= x y)
       :env/different-types (same-type-dispatch (agent {:type :a}) (agent {:type :b}))
       :a (same-type-dispatch (agent {:type :a}) (agent {:type :a}))))

(deftest bind-test
  (let [make-e (fn [] (agent {:a nil}))
        make-a (fn [] (agent {:e nil}))
        bindea (fn [e a]
                 (send a assoc :e (fn [] e))
                 (send e assoc :a (fn [] a))
                 (await a e))
        e (make-e)
        a (make-a)]
    
    (bindea e a)
    
    (is (= a (((deref e) :a))))))