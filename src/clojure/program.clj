;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

(ns #^{:doc
       "Программы агентов."
       :author "Роман Захаров"}
  program
  (:require)
  (:use aux match)
  (:import (java.io File)))

(defn reflex-download
  "Простая рефлексная программа агента для скачивания."
  [percept]
  (letfn [(out-of-space [percept]
                        (when-let [#^File file ((percept :self) :file)]
                          (< (.getUsableSpace file) (file-length file))))
          (fully-loaded [percept]
                        (<= ((percept :self) :length) 
                            (file-length ((percept :self) :file))))
          (missing [key] (fn [percept] (not ((percept :self) key))))
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
            [out-of-space       :die]
            [otherwise          :download]])))