;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программа агента для скачивания."
       :author "Роман Захаров"}
  download.program
  (:use aux action env download.env))

(in-ns 'download.program)

(defn reflex
  "Простая рефлексная программа агента для скачивания."
  [ag percept]
  (cond (dead? ag)             :pass
        (no :address ag)  :die
        (no :actions ag)  :die
        (no :link ag)     :get-link
        (no :tag ag)      :get-tag
        (no :name ag)     :get-name
        (no :file ag)     :get-file
        (already-on-done-path? ag) :die
        (no :length ag)   :get-length
        (out-of-space-on-work-path? ag) :die
        
        (fully-loaded? ag) 
        (cond (out-of-space-on-done-path? ag) :die
              (:done-path ag)  :move-to-done-path
              :else :die)

        :else                   :download))