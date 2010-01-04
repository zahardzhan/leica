;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты действий агентов."
       :author "Роман Захаров"}
  test.action
  (:use :reload aux action)
  (:use clojure.test)
  (:import (org.apache.commons.httpclient URI)))

(in-ns 'test.action)

(deftest after-test
  (let [ag {:type :env/default-agent}]
    (are [action result] (= result action)
         (after :watch-anime (assoc ag :action :eat)) false
         (after :watch-anime (assoc ag :action :watch-anime)) true
         (after :successful :eat (assoc ag :action :watch-anime)) false
         (after :successful :eat (assoc ag :action :eat :fail true)) false
         (after :successful :eat (assoc ag :action :eat)) true)))

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