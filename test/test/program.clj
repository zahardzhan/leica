;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тест базовых функций программы агента."
       :author "Роман Захаров"}
  test.program
  (:use :reload aux match program)
  (:use clojure.test))

(in-ns 'test.program)

(deftest missing-test
  (is (false? ((missing :a) {:a 1})))
  (is (false? ((missing :a) (atom {:a 1}))))
  (is (true?  ((missing :a) {:b 1})))
  (is (true?  ((missing :a) (atom {:b 1})))))