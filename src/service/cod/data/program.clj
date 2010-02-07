;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Программа агента, работающего с data.cod.ru."
       :author "Роман Захаров"}
  service.cod.data.program
  (:use aux env)
  ;; (:require action upload))
  )

(in-ns 'service.cod.data.program)

;; (defn reflex-upload
;;   "Простая рефлексная программа агента для заливки."
;;   [percept]
;;   (match percept
;;          [[(comp dead? :self)          :pass]
;;           [(missing :actions)          :die]
;;           [(missing :file)             :die]
;;           [(missing :name)             :die]
;;           [(missing :length)           :die]
;;           [(comp (partial action/after :successful :upload) :self) :report]
;;           [(comp (partial action/after :report) :self)             :die]
;;           [(comp env.upload/out-of-space-on-account? :self)        :die]
;;           [otherwise                   :upload]]))