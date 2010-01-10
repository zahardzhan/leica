;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тест скачивающих агентов."
       :author "Роман Захаров"}
  download.test-env
  (:use :reload aux match env download.env rules)
  (:use clojure.test clojure.set clojure.contrib.seq-utils)
  (:require :reload action)
  (:import java.io.File))

(in-ns 'download.test-env)

(deftest download-test
  (let [a (download-agent download-rules "http://dsv.data.cod.ru/578008"
                          :working-path (File. "/home/haru/inbox/dsv")
                          :done-path (File. "/home/haru/inbox/dsv/done")
                          :debug true)]
    
    (println "Тест скачивания файла:")
    
    (loop []
      (println "Последнее действие:" \space (@a :action))
      (run a)
      (await a)
      (if (dead? a)
        (do (is (action/after :successful :move-to-done-path a))
            (is (.exists (file a)))
            (when (.exists (file a)) (.delete (file a))))
        (recur)))))

(deftest accessors-test
  (let [a (download-agent download-rules "http://dsv.data.cod.ru/578008"
                          :working-path (File. "/home/haru/inbox/dsv")
                          :done-path (File. "/home/haru/inbox/dsv/done")
                          :debug true)]
    (is (= (address a) (org.apache.commons.httpclient.URI. "http://dsv.data.cod.ru/578008")))
    
    (is (= [nil nil nil false]
           [(out-of-space-on-work-path? a)
            (out-of-space-on-done-path? a)
            (fully-loaded? a)
            (termination? a)]))))