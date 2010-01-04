;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для скачивания."
       :author "Роман Захаров"}
  env.download
  (:use env aux match)
  (:require :reload [action :only 'percept-and-execute])
  (:import (java.io File)
           (org.apache.commons.httpclient URI)))

(in-ns 'env.download)

(derive ::download-agent :env/default-agent)

(def *timeout-after-fail* 3000)

(defn download-agent 
  "Агент для скачивания."  
  [rules line & [{:keys [working-path done-path progress-agent]}]]
  (let [[address {actions :actions program :program}]
        (match line rules {:action list})]
    (when (and address actions program)
      (send (default-agent {:type ::download-agent
                            :actions actions
                            :program program})
            assoc
            :address (URI. address)     ; адрес задания
            :link nil   ; прямая ссылка на файл, который нужно скачать
            :file nil   ; файл в который сохраняется скачанное
            :length nil ; размер файла, что нужно скачать
            :working-path working-path ; директория в которую качаются файлы
            :done-path done-path ; директория в которую отправляются скачанные файлы
            :progress-agent progress-agent ; агент-отображатель прогресса
            ))))

(defn download-agents [rules lines]
  (remove (comp not agent?) 
          (map (partial download-agent rules) lines)))

(defn address [ag] (-> ag self deref :address))
(defn length [ag] (-> ag self deref :length))
(defn file [ag] (-> ag self deref :file))
(defn done-path [ag] (-> ag self deref :done-path))
(defn working-path [ag] (-> ag self deref :working-path))

(defn out-of-space-on-work-path? [ag]
  (< (.getUsableSpace (working-path ag))
     (- (length ag) (file-length (file ag)))))

(defn out-of-space-on-done-path? [ag]
  (< (.getUsableSpace (done-path ag)) (length ag)))

(defn fully-loaded? [ag]
  (<= (length ag) (file-length (file ag))))

(defn already-on-done-path? [ag]
  (.exists (File. (done-path ag) (.getName (file ag)))))

(def same-download-agents (fn-or (partial same identity)
                                 (partial same address)))

(defn next-alive-untagged-after [ag]
  (next-after-when (fn-and alive? (complement tag) (same type-dispatch (self ag)))
                   ag (env ag)))

(defn alive-unfailed-with-same-tag [ag]
  (some (fn-and alive? (complement fail?) (partial same tag ag) identity)
        (env ag)))

(defn next-alive-with-same-tag-after [ag]
  (next-after-when (fn-and alive? (partial same tag ag))
                   ag (env ag)))

(defmethod done [::download-agent :state] [ag]
  (or (run (or (alive-unfailed-with-same-tag ag)
               (next-alive-with-same-tag-after ag)))
      (when (termination? ag) ((ag :termination) ag)))
  ag)

(defmethod run [::download-agent :state] [ag]
  (cond (dead? ag) ag

        (not (tag ag)) (let [new-state (action/percept-and-execute ag {})]
                         (cond (dead? new-state) (done ag)
                               (fail? new-state) (do (sleep ag *timeout-after-fail*)
                                                     (when-not (debug? ag) (run ag)))
                               (tag new-state) (do (run (next-alive-untagged-after ag))
                                                   (run ag))
                               :else (when-not (debug? ag) (run ag)))
                         new-state)

        (tag-locked? ag) ag

        :else (let [new-state (locking-tag 
                               ag (action/percept-and-execute ag {}))]
                (cond (dead? new-state) (done ag)
                      (fail? new-state) (do (sleep ag *timeout-after-fail*)
                                            (done ag))
                      :else (when-not (debug? ag) (run ag)))
                new-state)))