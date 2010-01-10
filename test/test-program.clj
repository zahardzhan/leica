;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тест базовых функций программы агента."
       :author "Роман Захаров"}
  test-program
  (:use :reload aux match program)
  (:use clojure.test))

(in-ns 'test-program)

(deftest missing-test
  (are [key val] (true? ((missing key) val))
       :a {:a 1}
       :a {:b 1}))