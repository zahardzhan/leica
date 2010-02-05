;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты проверки файловых путей."
       :author "Роман Захаров"}
  test.verified
  (:use :reload aux verified)
  (:use clojure.test)
  (:import java.io.File))

(in-ns 'test.verified)

;; (upload-files ["/home/haru/etc/emacs/rc"
;;                "/home/haru/inbox/dsv"
;;                "/home/haru/share/leica/src/leica.clj"
;;                "/home/haru/inbox/dsv"
;;                nil])