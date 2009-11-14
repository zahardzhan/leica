;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Программа агента, работающего с data.cod.ru."
       :author "Роман Захаров"}
  datacod.program
  (:require :reload datacod.account)
  (:use aux match)
  (:import (org.apache.commons.httpclient HttpClient)))

(defn reflex-upload
  "Простая рефлексная программа агента для заливки."
  [percept]
  (letfn [(out-of-space ;; недостаточно места на сервере
           [percept] 
           (let [client (new HttpClient)]
             (datacod.account/with-auth client ((percept :env) :account)
               (let [free-space (datacod.account/free-space client ((percept :env) :account))]
                 (if free-space
                   (< free-space ((percept :self) :length))
                   true)))))
          (missing [key] (fn [percept] (not ((percept :self) key))))
          (has [key] (fn [percept] ((percept :self) key)))
          (otherwise [_] true)]
    (match percept
           [[(missing :actions) :die]
            [(missing :file)    :die]
            [(missing :name)    :die]
            [(missing :length)  :die]
            [(has     :address) :report-and-die]
            [out-of-space       :die]
            [otherwise          :upload]])))