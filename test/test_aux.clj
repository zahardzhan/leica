;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты вспомогательных функций."
       :author "Роман Захаров"}
  test-aux
  (:use :reload aux)
  (:use clojure.test))

(in-ns 'test-aux)

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

(deftest with-deref-test  
  (let [x (atom 1), y (agent 2), z (delay 3)]
    (is (= [1 2 3] (with-deref [x y z] [x y z])))))

(deftest agent-or-type-dispatch-test
  (let [a (agent {:type :asdf})]
    (are [x y] (= x (apply agent-or-type-dispatch y))
         nil    [nil]
         :agent [a]
         :agent [a 1 2 3]
         :asdf  [@a])))

;; user=>  (defprotocol p (foo [this]))
;; p
;; user=>  (deftype a [f] p (foo []))
;; #'user/a
;; user=> (foo (a nil))
;; nil
;; user=> (deftype a [f] :as this p (foo [] [this]))
;; #'user/a
;; user=> (foo (a nil))
;; [#:a{:f nil}] 