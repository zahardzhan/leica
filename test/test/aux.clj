;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты вспомогательных функций."
       :author "Роман Захаров"}
  test.aux
  (:use :reload aux)
  (:use clojure.test))

(in-ns 'test.aux)

(deftest derefed-test
  (is (= nil (derefed)))
  (let [r (ref {:a {:b :c}})]
    (is (= @r (derefed r)))
    (is (= (:a @r) (derefed r :a)))
    (is (= (:b (:a @r)) (derefed r :a :b)))))

(deftest with-deref-test
  (let [x (atom 1), y (agent 2), z (delay 3), w 4]
    (is (= nil (with-deref [])))
    (is (= 1 (with-deref [x] x)))
    (is (= [1 4] (with-deref [x y w] [x w])))
    (is (= [1 2 3] (with-deref [x y z] [x y z])))))

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