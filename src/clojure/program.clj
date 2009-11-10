;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программы агентов."
       :author "Роман Захаров"}
  program
  (:use aux match)
  (:import (java.io File)))

(defn- out-of-space-here [percept]
  (when-let [#^File file ((percept :self) :file)]
    (< (.getUsableSpace file) (file-length file))))

(defn- fully-loaded [percept]
  (<= ((percept :self) :length)
      (file-length ((percept :self) :file))))

(defn reflex-download
  "Простая рефлексная программа агента для скачивания."
  [percept]
  (letfn [(missing [key] (fn [percept] (not ((percept :self) key))))
          (otherwise [_] true)]
    (match percept
           [[(missing :address) :die]
            [(missing :actions) :die]
            [(missing :link)    :obtain-link]
            [(missing :tag)     :obtain-tag]
            [(missing :name)    :obtain-name]
            [(missing :file)    :obtain-file]
            [(missing :length)  :obtain-length]
            [fully-loaded       :die]
            [out-of-space-here  :die]
            [otherwise          :download]])))