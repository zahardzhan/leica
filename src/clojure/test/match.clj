;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты сопоставителя с образцом."
       :author "Роман Захаров"}
  test.match
  (:use :reload match)
  (:use clojure.test))

(in-ns 'test.match)

(deftest match-test
  (are [x y] (= x y)
       (:a 2) (match :a [[:b 1]
                         [:a 2]]
                     {:action list})

       3 (case :c
               :a 1
               :b 2
               :c 3)))