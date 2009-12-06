;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для скачивания."
       :author "Роман Захаров"}
  env.download
  (:use env aux match)
  (:require :reload action)
  (:import (java.io File)
           (org.apache.commons.httpclient URI)))

(in-ns 'env.download)

(derive ::download-agent :env/default-agent)
(derive ::download-env   :env/default-env)

(def *timeout-after-fail* 3000)

;;;; Агент

(defn download-agent 
  "Агент для скачивания.

  :address адрес задания
  :link    прямая ссылка на файл, который нужно скачать
  :tag     идентификатор по которому разделяются потоки загрузок
  :file    файл в который сохраняется скачанное
  :length  размер файла, что нужно скачать"
  
  [line rules]
  (let [[address {actions :actions program :program}]
        (match line rules {:action list})]
    (when (and address actions)
      (agent {:type ::download-agent
              :address (URI. address)
              :env empty-fn
              :link nil :name nil :tag nil :file nil :length nil
              :actions actions
              :program program
              :alive true :fail false :percept nil :action :create}))))

(defmethod same? ::download-agent [ag1 ag2]
  (or (= ag1 ag2)
      (= (:address @ag1) (:address @ag2))))

(defn download-agents [lines rules]
  (remove (comp not agent?) (map #(download-agent % rules) lines)))

(defmethod run-agent [::download-agent :state] [ag-state]
  (let [tag (:tag ag-state)
        env (related-env ag-state)]
    (cond (dead? ag-state) ag-state

          (not tag) (let [new-state (action/percept-and-execute ag-state {:self ag-state})]
                      (cond (dead? new-state) (done env *agent*)
                            (fail? new-state) (do (sleep *agent* *timeout-after-fail*)
                                                  (when-not (debug? *agent*)
                                                    (run-agent *agent*)))
                            (:tag new-state) (do (add-tag env (:tag new-state))
                                                 (received-tag env *agent*))
                            :else (when-not (debug? *agent*) (run-agent *agent*)))
                      new-state)

          (tag-locked? env tag) ag-state

          :else (let [new-state (with-lock-env-tag env tag
                                  (action/percept-and-execute ag-state {:self ag-state}))]
                  (cond (dead? new-state) (done env *agent*)
                        (fail? new-state) (do (sleep *agent* *timeout-after-fail*)
                                              (done env *agent*))
                        :else (when-not (debug? *agent*) (run-agent *agent*)))
                  new-state))))

;;;; Окружение

(defn download-environment [& [{:keys [working-path done-path progress-agent 
                                       debug termination]
                                :or   {working-path nil
                                       done-path nil
                                       progress-agent nil
                                       debug false
                                       termination empty-fn}}]]
  (agent {:type ::download-env :agents '() :tags {}
          :working-path working-path
          :done-path done-path
          :progress-agent progress-agent
          :termination termination}))

(defmethod received-tag [::download-env :state] [env-state ag]
  (when-let [next-alive-untagged-agent
             (next-after-when #(and (alive? %) (not (:tag (deref %))))
                              ag (:agents env-state))]
    (run-agent next-alive-untagged-agent))
  (run-agent ag)
  env-state)

(defmethod done [::download-env :state] [env-state ag]
  (let [alive-unfailed-with-same-tag
        (some #(when (and (= (:tag @ag) (:tag @%)) (alive? %) (not (fail? %))) %)
              (:agents env-state))
        next-alive-with-same-tag
        (next-after-when #(and (= (:tag @ag) (:tag @%)) (alive? %))
                         ag (:agents env-state))]
    
    (cond alive-unfailed-with-same-tag
          (run-agent alive-unfailed-with-same-tag)

          next-alive-with-same-tag
          (run-agent next-alive-with-same-tag)

          (termination? env-state) ((env-state :termination) env-state)))
  env-state)

(defmulti out-of-space-on-work-path? type-agent-dispatch)
(defmulti out-of-space-on-done-path? type-agent-dispatch)
(defmulti done-path-set?             type-agent-dispatch)
(defmulti fully-loaded?              type-agent-dispatch)
(defmulti already-on-done-path?      type-agent-dispatch)

(defmethod out-of-space-on-work-path? [::download-agent :agent] [ag]
  (out-of-space-on-work-path? (deref ag)))

(defmethod out-of-space-on-work-path? [::download-agent :state] [ag]
  (when-let [#^File working-path ((deref (related-env ag)) :working-path)]
    (when-let [#^File file (ag :file)]
      (when-let [full-length (ag :length)]
        (< (.getUsableSpace working-path) (- full-length (file-length file)))))))

(defmethod out-of-space-on-done-path? [::download-agent :agent] [ag]
  (out-of-space-on-done-path? (deref ag)))

(defmethod out-of-space-on-done-path? [::download-agent :state] [ag]
  (when-let [#^File done-path ((deref (related-env ag)) :done-path)]
    (when-let [full-length (ag :length)]
      (< (.getUsableSpace done-path) full-length))))

(defmethod fully-loaded? [::download-agent :agent] [ag]
  (fully-loaded? (deref ag)))

(defmethod fully-loaded? [::download-agent :state] [ag]
  (<= (ag :length) (file-length (ag :file))))

(defmethod already-on-done-path? [::download-agent :agent] [ag]
  (already-on-done-path? (deref ag)))

(defmethod already-on-done-path? [::download-agent :state] [ag]
  (when-let [#^File done-path ((deref (related-env ag)) :done-path)]
    (when-let [#^File file (ag :file)]
      (.exists (File. done-path (.getName file))))))

(defmethod done-path-set? [::download-agent :agent] [env]
  (done-path-set? (deref env)))

(defmethod done-path-set? [::download-agent :state] [env]
  (:done-path env))
