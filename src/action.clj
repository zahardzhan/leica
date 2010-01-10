;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Базовые действия агентов."
       :author "Роман Захаров"}
  action
  (:require [clojure.contrib.logging :as log])
  (:use aux match env))

(in-ns 'action)

(defn execute [ag action]
  (with-deref [ag]
    (let [fag (future (assoc (((ag :actions) action) ag)
                        :action (if (#{:die :fail :pass} action)
                                  (ag :action)
                                  action)))]
      (log/debug (str (or (ag :name) (ag :address)) \space action))
      (with-deref [fag]
        (log/debug (str (or (fag :name) (fag :address)) \space action \space 
                        (cond (dead? fag) "агент умер"
                              (fail? fag) "агент провалился"
                              :else        "успешно")))
        fag))))

(defn percept 
  ([ag] (percept ag {}))
  ([ag perception] (with-deref [ag] ((ag :program) ag perception))))

(defn percept-and-execute
  ([ag] (percept-and-execute ag {}))
  ([ag perception] (with-deref [ag]
                     (let [action (percept ag perception)]
                       (execute ag action)))))

(defn after
  ([action ag] (with-deref [ag] (= action (:action ag))))
  ([status action ag] 
     (with-deref [ag]
       (and (after action ag)
            (case status
                  :successful ((no fail?) ag)
                  :failed     (fail? ag))))))

(defn pass [ag]
  ag)
 
(defn fail [ag]
  (assoc ag :fail true))
 
(defn die [ag]
  (assoc ag :alive false))