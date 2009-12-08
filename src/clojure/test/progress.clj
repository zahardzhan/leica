;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты отображения прогресса."
       :author "Роман Захаров"}
  test.progress
  (:use :reload progress)
  (:use clojure.test))

(in-ns 'test.progress)

(deftest console-progress-test
  (let [a (console-progress-agent)]
    (with-out-str
      (show-progress a {:tag 1 :name "Boogie-bop phantom - 01.mkv" :progress 10 :total 20 :time nil})
      (show-progress a {:tag 2 :name "Evangelion ReTake[03].rar"   :progress 10 :total 20 :time nil})
      (show-progress a {:tag 2 :name "Evangelion ReTake[03].rar"   :progress 20 :total 20 :time nil})
      (hide-progress a 2)
      (show-progress a {:tag 1 :name "Boogie-bop phantom - 01.mkv" :progress 20 :total 20 :time nil}))))