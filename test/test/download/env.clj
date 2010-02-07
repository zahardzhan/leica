;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тест скачивающих агентов."
       :author "Роман Захаров"}
  test.download.env
  (:use :reload aux env download.env rules)
  (:use clojure.test
        clojure.set
        [clojure.contrib.duck-streams :as duck]
        clojure.contrib.seq-utils)
  (:require :reload action progress verified)
  (:import java.io.File))

(in-ns 'test.download.env)

(def adr-1 "http://dsv.data.cod.ru/633651")
(def adr-2 "http://dsv.data.cod.ru/629381")
(def adr-3 "http://dsv.data.cod.ru/633985")

(comment
(let [wp (File. "/home/haru/inbox/dsv")
      dp (File. "/home/haru/inbox/dsv/done")
      debug true]
  (def a (download-agent download-rules adr-1 :working-path wp :done-path dp :debug true))
  (def b (download-agent download-rules adr-2 :working-path wp :done-path dp :debug true))
  (def c (download-agent download-rules adr-3 :working-path wp :done-path dp :debug true)))
)

(deftest download-test
  (let [a (download-agent download-rules adr-2
                          :working-path (File. "/home/haru/inbox/dsv")
                          :done-path (File. "/home/haru/inbox/dsv/done")
                          :debug true)]
    
    (println "Тест скачивания.")
    
    (loop []
      (println "Последнее действие:" \space (@a :action))
      (run a)
      (await a)
      (if (dead? a)
        (do (is (action/after :successful :move-to-done-path a))
            (is (.exists (file a)))
            (when (.exists (file a)) (.delete (file a))))
        (recur)))))

(deftest async-download-test
  (let [wp (File. "/home/haru/inbox/dsv")
        dp (File. "/home/haru/inbox/dsv/done")
        a (download-agent download-rules adr-1
                          :working-path wp, :done-path dp)
        b (download-agent download-rules adr-2
                          :working-path wp, :done-path dp)]

    (println "Тест асинхронного скачивания.")
    
    (bind a b)
    
    (loop []
      (doseq [x (env a)]
        (println "Последнее действие:" \space (@x :address) (@x :action))
        (run x))
      (await a b)
      (if (and (dead? a) (dead? b))
        (doseq [x (env a)]
          (is (action/after :successful :move-to-done-path x))
          (is (.exists (file x)))
          (when (.exists (file x)) (.delete (file x))))
        (recur)))))

(deftest main-download-test
  (let [remaining-args ["/home/haru/inbox/dsv/.jobs" "/home/haru/inbox/dsv"]
        jobs-file    (some verified/jobs-file remaining-args)
        working-path (or (some verified/output-dir remaining-args)
                         (verified/output-dir (System/getProperty "user.dir")))
        done-path    (verified/path "/home/haru/inbox/dsv/done")]
    (when (and jobs-file working-path)
      (let [progress-agent (progress/console-progress-agent)
            agents (filter
                    env-agent?
                    (for [line (duck/read-lines jobs-file)]
                      (download-agent download-rules line
                                      :working-path working-path
                                      :done-path (when (not= working-path done-path) done-path)
                                      :progress-agent progress-agent)))]
        (apply env/bind agents)))))

(deftest accessors-test
  (let [a (download-agent download-rules "http://dsv.data.cod.ru/578008"
                          :working-path (File. "/home/haru/inbox/dsv")
                          :done-path (File. "/home/haru/inbox/dsv/done")
                          :debug true)]
    (is (= (address a) (org.apache.commons.httpclient.URI. "http://dsv.data.cod.ru/578008")))
    
    (is (= [nil nil nil false]
           [(out-of-space-on-work-path? a)
            (out-of-space-on-done-path? a)
            (fully-loaded? a)
            (termination? a)]))))