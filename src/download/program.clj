;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программа агента для скачивания."
       :author "Роман Захаров"}
  download.program
  (:use aux match action program env download.env))

(in-ns 'download.program)

(defn reflex
  "Простая рефлексная программа агента для скачивания."
  [ag percept]
  (match ag
         [[dead?                         :pass]
          [(missing :address)            :die]
          [(missing :actions)            :die]
          [(missing :link)               :get-link]
          [(missing :tag)                :get-tag]
          [(missing :name)               :get-name]
          [(missing :file)               :get-file]
          [already-on-done-path?         :die]
          [(missing :length)             :get-length]
          [out-of-space-on-work-path?    :die]
          [(fn-and
            fully-loaded?
            out-of-space-on-done-path?)  :die]
          [(fn-and 
            fully-loaded?
            done-path)                   :move-to-done-path]
          [fully-loaded?                 :die]
          [else                          :download]]))