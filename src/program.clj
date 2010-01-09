;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программы агентов."
       :author "Роман Захаров"}
  program
  (:use aux match)
  (:require action))

(in-ns 'program)

(defn missing [key] (comp no key derefed))

(def else (constantly true))