;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программы агентов."
       :author "Роман Захаров"}
  program
  (:use aux match env env.download)
  (:require action))

(in-ns 'program)

(def no complement)

(def otherwise (constantly true))

(defn reflex-download
  "Простая рефлексная программа агента для скачивания."
  [ag percept]
  (match ag
         [[dead?         :pass]
          [(no :address) :die]
          [(no :actions) :die]
          [(no :link)    :get-link]
          [(no :tag)     :get-tag]
          [(no :name)    :get-name]
          [(no :file)    :get-file]
          [already-on-done-path? :die]
          [(no :length)  :get-length]
          [out-of-space-on-work-path? :die]
          [(fn-and fully-loaded?
                   out-of-space-on-done-path?) :die]
          [(fn-and fully-loaded? 
                   :done-path) :move-to-done-path]
          [fully-loaded? :die]
          [otherwise     :download]]))
