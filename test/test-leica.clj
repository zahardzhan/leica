;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты окружения."
       :author "Роман Захаров"}
  test.env
  (:use :reload aux match env env.download env.upload rules)
  (:use clojure.test clojure.set [clojure.contrib seq-utils])
  (:require :reload action program
            datacod.account datacod.action datacod.program)
  (:import (java.io File)))

(in-ns 'test.env)

(deftest upload-test
  (let [account (datacod.account/datacod-account
                 "dsv" "zahardzhan@gmail.com" "zscxadw")
        file (File. "/home/haru/inbox/dsv/.jobs")
        ;; report-file (verified-log-file report)
        e (upload-environment account
                              {;:report-file report-file
                               :debug true})
        a (upload-agent file *upload-rules*)]
    (add-agent e a)
    (await e)
    
    (println "Тест закачивания файла:")
    (loop []
      (println "Последнее действие:" \space (@a :action))
      (run-agent a)
      (await a)
      (if (dead? a)
        (is (action/after :report @a))
        (recur)))))