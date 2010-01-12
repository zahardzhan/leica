;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты действий агентов."
       :author "Роман Захаров"}
  download.test-action
  (:use :reload aux action download.action)
  (:use clojure.test)
  (:import (org.apache.commons.httpclient URI)))

(in-ns 'download.test-action)

(deftest get-name-test
  (let [ag {:type :env/default-agent}]
    (are [link name] 
         (= name (:name (get-name (assoc ag :link (URI. link)))))
         "http://www.google.ru/adasd.rar" "adasd.rar")))

(deftest get-tag-test
  (let [ag {:type :env/default-agent}]
    (are [link pattern tag] 
         (= tag (:tag (get-tag pattern (assoc ag :link (URI. link)))))
         "http://www.google.ru/xxx?yyy"
         nil
         "www.google.ru"

         "http://files.dsv.data.cod.ru/?xxx" 
         [#"files3?.dsv.data.cod.ru" #"files2.dsv.data.cod.ru"]
         "files3?.dsv.data.cod.ru"

         "http://files2.dsv.data.cod.ru/?xxx" 
         [#"files3?.dsv.data.cod.ru" #"files2.dsv.data.cod.ru"]
         "files2.dsv.data.cod.ru"

         "http://files4.dsv.data.cod.ru/?xxx" 
         [#"files3?.dsv.data.cod.ru" #"files2.dsv.data.cod.ru"]
         "files4.dsv.data.cod.ru")))