;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты окружения."
       :author "Роман Захаров"}
  test.env
  (:use :reload aux match env env.download env.upload rules)
  (:use clojure.test [clojure.contrib seq-utils])
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
        a (download-agent "http://dsv.data.cod.ru/519222" *download-rules*)]
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

(deftest same-type-dispatch-test
  (are [x y] (= x y)
       :env/different-types (same-type-dispatch (agent {:type :a}) (agent {:type :b}))
       :a (same-type-dispatch (agent {:type :a}) (agent {:type :a}))))

(deftest bind-test
  (let [make-e (fn [] (agent {:a nil}))
        make-a (fn [] (agent {:e nil}))
        bindea (fn [e a]
                 (send a assoc :e (fn [] e))
                 (send e assoc :a (fn [] a))
                 (await a e))
        e (make-e)
        a (make-a)]
    
    (bindea e a)
    
    (is (= a (((deref e) :a))))))