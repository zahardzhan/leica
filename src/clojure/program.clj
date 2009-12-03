;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программы агентов."
       :author "Роман Захаров"}
  program
  (:use aux match env)
  (:require action env.download))

(defn has [key]
  (comp key :self))

(defn missing [key]
  (comp not key :self))

(def otherwise (constantly true))

(defn reflex-download
  "Простая рефлексная программа агента для скачивания."
  [percept]
  (match percept
         [[(comp dead? :self) :pass]
          [(missing :address) :die]
          [(missing :actions) :die]
          [(missing :link)    :get-link]
          [(missing :tag)     :get-tag]
          [(missing :name)    :get-name]
          [(missing :file)    :get-file]
          [(comp env.download/already-on-done-path? :self) :die]
          [(missing :length)  :get-length]
          [(comp env.download/out-of-space-on-work-path? :self) :die]
          [(fn-and
            (comp env.download/fully-loaded? :self)
            (comp env.download/out-of-space-on-done-path? :self)) :die]
          [(fn-and 
            (comp env.download/fully-loaded? :self)
            (comp env.download/done-path-set? :self)) :move-to-done-path]
          [(comp env.download/fully-loaded? :self) :die]
          [otherwise          :download]]))
