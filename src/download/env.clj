;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для скачивания."
       :author "Роман Захаров"}
  download.env
  (:use env aux match clojure.contrib.def)
  (:require :reload action)
  (:import (java.io File)))

(in-ns 'download.env)

(def timeout-after-fail 3000)

(derive ::download-agent :env/default-agent)

(defnk download-agent
  "Агент для скачивания."  
  [rules line :working-path nil, :done-path nil, :progress-agent nil]
  (let [[address, {actions :actions, program :program}]
        (match line rules {:action list})]
    (when (and address actions program)
      (send (default-agent 
              :type ::download-agent
              :actions actions
              :program program)
            assoc
            :address (org.apache.commons.httpclient.URI. address) ; адрес задания
            :link nil   ; прямая ссылка на файл, который нужно скачать
            :file nil   ; файл в который сохраняется скачанное
            :length nil ; размер файла, что нужно скачать
            :working-path working-path ; директория в которую качаются файлы
            :done-path done-path ; директория в которую отправляются скачанные файлы
            :progress-agent progress-agent ; агент-отображатель прогресса
            ))))

(defn address [ag] (-> ag derefed :address))
(defn length [ag] (-> ag derefed :length))
(defn file [ag] (-> ag derefed :file))
(defn done-path [ag] (-> ag derefed :done-path))
(defn working-path [ag] (-> ag derefed :working-path))

(defn out-of-space-on-work-path? [ag]
  (< (.getUsableSpace (working-path ag))
     (- (length ag) (file-length (file ag)))))

(defn out-of-space-on-done-path? [ag]
  (< (.getUsableSpace (done-path ag)) (length ag)))

(defn fully-loaded? [ag]
  (<= (length ag) (file-length (file ag))))

(defn already-on-done-path? [ag]
  (.exists (File. (done-path ag) (.getName (file ag)))))

(defn next-alive-untagged-after [ag]
  (next-after-when (fn-and alive? (no tag) (partial same type-dispatch ag))
                   ag (env ag)))

(defn alive-unfailed-with-same-tag [ag]
  (some (fn-and alive? (no fail?) (partial same tag ag) identity)
        (env ag)))

(defn next-alive-with-same-tag-after [ag]
  (next-after-when (fn-and alive? (partial same tag ag))
                   ag (env ag)))

(defmethod done ::download-agent [ag]
  (or (run (or (alive-unfailed-with-same-tag ag)
               (next-alive-with-same-tag-after ag)))
      (when (termination? ag) (terminate ag)))
  ag)

(defmethod run ::download-agent [ag]
  (let [run (fn [ag] (when-not (debug? ag) (run ag)))] ;;; !!!
    (cond (dead? ag) ag

          ((no tag) ag)
          (let [new-state (action/percept-and-execute ag)]
            (cond (dead? new-state) (done ag)
                  (fail? new-state) (do (sleep ag timeout-after-fail)
                                        (run ag))
                  (tag new-state) (do (run (next-alive-untagged-after ag))
                                      (run ag))
                  :else (run ag))
            new-state)

          (tag-locked-in-env? ag) ag

          :else (let [new-state 
                      (with-locked-tag ag (action/percept-and-execute ag))]
                  (cond (dead? new-state) (done ag)
                        (fail? new-state) (do (sleep ag timeout-after-fail)
                                              (done ag))
                        :else (run ag))
                  new-state))))