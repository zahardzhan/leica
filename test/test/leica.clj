;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты окружения."
       :author "Роман Захаров"}
  test.leica
  (:use :reload aux match)
  (:use clojure.test))

(in-ns 'test.leica)

;; (deftest upload-test
;;   (let [account (datacod.account/datacod-account
;;                  "dsv" "zahardzhan@gmail.com" "zscxadw")
;;         file (File. "/home/haru/inbox/dsv/.jobs")
;;         ;; report-file (verified-log-file report)
;;         e (upload-environment account
;;                               {;:report-file report-file
;;                                :debug true})
;;         a (upload-agent file *upload-rules*)]
;;     (add-agent e a)
;;     (await e)
    
;;     (println "Тест закачивания файла:")
;;     (loop []
;;       (println "Последнее действие:" \space (@a :action))
;;       (run-agent a)
;;       (await a)
;;       (if (dead? a)
;;         (is (action/after :report @a))
;;         (recur)))))