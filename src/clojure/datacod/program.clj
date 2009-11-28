;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Программа агента, работающего с data.cod.ru."
       :author "Роман Захаров"}
  datacod.program
  (:require datacod.account)
  (:use aux match env)
  (:import (org.apache.commons.httpclient HttpClient)))

(defn after
  ([action] (fn [percept] (= action ((percept :self) :action))))
  ([status action] 
     (fn [percept] 
       (when ((after action) percept)
         (cond (= status :successful) (= ((percept :self) :fail) false)
               (= status :failed) (= ((percept :self) :fail) true))))))

(defn out-of-space [percept] 
  (let [client (new HttpClient)
        account ((deref (related-env (percept :self))) :account)]
    (datacod.account/with-auth client account
      (let [free-space (datacod.account/free-space client account)]
        (if free-space
          (< free-space ((percept :self) :length))
          true)))))

(defn agent-is-dead? [percept] (not ((percept :self) :alive)))
(defn missing [key] (fn [percept] (not ((percept :self) key))))
(defn has [key] (fn [percept] ((percept :self) key)))
(defn otherwise [_] true)

(defn reflex-upload
  "Простая рефлексная программа агента для заливки."
  [percept]
  (match percept
         [[agent-is-dead?              :pass]
          [(missing :actions)          :die]
          [(missing :file)             :die]
          [(missing :name)             :die]
          [(missing :length)           :die]
          [(after :successful :upload) :report]
          [(after :report)             :die]
          [out-of-space                :die]
          [otherwise                   :upload]]))