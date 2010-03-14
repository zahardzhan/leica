;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns fn 
  (:use clojure.test)
  (:refer-clojure :exclude [not and or]))

(in-ns 'fn)

(declare no not and or)

(defn not
  "Alias for complement function.
  When used with more than one argument returns intersection of
  complementary functions.

  (not f) => (complement f)
  (not f g h ...) => (and (not f) (not g) (not h) ...)"
  ([f] (complement f))
  ([f & fs] (and (not f) 
                 (apply and (map not fs)))))

(defn and
  "Functions intersection returns function that returns something,
  when all intersection's functions returns something.

  (and f g h ...) => (fn [xs] (and (f xs) (g xs) (h xs) ...))"
  ([f] f)
  ([f & fs] (let [chain (apply and fs)]
              (fn [& xs] (clojure.core/and (apply f xs) 
                                           (apply chain xs))))))

(defn or
  "Functions union return function that returns something,
  when one of union's functions returns something.

  (or f g h ...) => (fn [xs] (or (f xs) (g xs) (h xs) ...))"
  ([f] f)
  ([f & fs] (let [chain (apply or fs)]
               (fn [& xs] (clojure.core/or (apply f xs)
                                           (apply chain xs))))))

(deftest fn-test
  (are [pred val res] (= res (pred val))
       (or pos? odd? zero?)  0  true
       (or pos? odd? zero?) -2  false

       (and pos? even?) -2 false
       (and pos? even?)  2 true

       (not even? pos?) -1 true
       (not even?) 1 true

       no 'asdf nil))