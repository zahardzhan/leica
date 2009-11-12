;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Программы агентов."
       :author "Роман Захаров"}
  program
  (:use aux match)
  (:require env)
  (:import (java.io File)))

(defn- out-of-space-on-work-path? [percept]
  (when-let [#^File working-path ((percept :env) :working-path)]
    (when-let [#^File file ((percept :self) :file)]
      (when-let [full-length ((percept :self) :length)]
        (< (.getUsableSpace working-path) (- full-length (file-length file)))))))

(defn- out-of-space-on-done-path? [percept]
  (when-let [#^File done-path ((percept :env) :done-path)]
    (when-let [full-length ((percept :self) :length)]
      (< (.getUsableSpace done-path) full-length))))

(defn- done-path-set? [percept]
  ((percept :env) :done-path))

(defn- fully-loaded? [percept]
  (<= ((percept :self) :length)
      (file-length ((percept :self) :file))))

(defn- already-on-done-path? [percept]
  (when-let [#^File done-path ((percept :env) :done-path)]
    (when-let [#^File file ((percept :self) :file)]
      (.exists (File. done-path (.getName file))))))

(defn reflex-download
  "Простая рефлексная программа агента для скачивания."
  [percept]
  (letfn [(dead? [percept] (not ((percept :self) :alive)))
          (missing [key] (fn [percept] (not ((percept :self) key))))
          (otherwise [_] true)]
    (match percept
           [[dead?                                :pass]
            [(missing :address)                   :die]
            [(missing :actions)                   :die]
            [(missing :link)                      :get-link]
            [(missing :tag)                       :get-tag]
            [(missing :name)                      :get-name]
            [(missing :file)                      :get-file]
            [already-on-done-path?                :die]
            [(missing :length)                    :get-length]
            [out-of-space-on-work-path?           :die]
            [#(and (fully-loaded? %)
                   (out-of-space-on-done-path? %)) :die]
            [#(and (fully-loaded? %)
                   (done-path-set? %))            :move-to-done-path]                    
            [fully-loaded?      :die]
            [otherwise          :download]])))