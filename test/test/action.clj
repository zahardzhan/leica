;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты действий агентов."
       :author "Роман Захаров"}
  test.action
  (:use :reload aux action)
  (:use clojure.test))

(in-ns 'test.action)

(deftest after-test
  (let [ag {:type :env/default-agent}]
    (are [action result] (= result action)
         (after :watch-anime (assoc ag :action :eat)) false
         (after :watch-anime (assoc ag :action :watch-anime)) true
         (after :successful :eat (assoc ag :action :watch-anime)) false
         (after :successful :eat (assoc ag :action :eat :fail true)) false
         (after :successful :eat (assoc ag :action :eat)) true)))