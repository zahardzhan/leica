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

(deftest add-agent-test
  (let [ags (download-agents ["http://dsv.data.cod.ru/507882"
                              "http://dsv.data.cod.ru/507882"
                              "http://dsv.data.cod.ru/507883"]
                             *download-rules*)]
    (let [e (download-environment {})]
      (doseq [ag ags] (add-agent e ag))
      (await e)
      (is (= 2 (count (agents e))))
      (is (every? #(= e (related-env %)) (agents e))))

    (let [e (download-environment {})]
      (add-agents e ags)
      (await e)
      (is (= 2 (count (agents e))))
      (is (every? #(= e (related-env %)) (agents e))))))

(deftest download-test
  (let [e (download-environment {:working-path (File. "/home/haru/inbox/dsv")
                                 :done-path (File. "/home/haru/inbox/dsv/done")
                                 :debug true})
        a (download-agent "http://dsv.data.cod.ru/526633" *download-rules*)]
    (add-agent e a)
    (await e)
    
    (println "Тест скачивания файла:")
    (loop []
      (println "Последнее действие:" \space (@a :action))
      (run-agent a)
      (await a)
      (if (dead? a)
        (do (is (action/after :successful :move-to-done-path @a))
            (is (.exists (@a :file)))
            (when (.exists (@a :file)) (.delete (@a :file))))
        (recur)))))

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