;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты проверки файловых путей."
       :author "Роман Захаров"}
  test.verified
  (:use :reload aux)
  (:use clojure.test)
  (:import java.io.File))

(in-ns 'test.verified)