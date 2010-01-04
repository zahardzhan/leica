;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты вспомогательных функций."
       :author "Роман Захаров"}
  test.aux
  (:use :reload aux)
  (:use clojure.test))

(in-ns 'test.aux)

(deftest next-after-when-test
  (let [xs [{:a 1} {:a 0} {:a 2} {:b 1}]]
    (are [x y] (= x y)
         {:b 1} (next-after-when :b {:a 2} xs)
         {:a 1} (next-after-when :a {:a 2} xs)
         {:a 2} (next-after-when #(= 2 (:a %)) {:a 2} xs))))

(deftest transliterate-test
  (are [str translit] (= (transliterate str) translit)
       "В рот мне ноги!" "V rot mne nogi!"
       "Twitter в ударе" "Twitter v udare"))

(deftest fn-test
  (are [f fs arg result] (= result ((apply f fs) arg))
       fn-and [pos? even?]  2 true
       fn-and [pos? even?] -2 false

       fn-or  [pos? even?] -2 true
       fn-or  [pos? odd?]  -2 false))