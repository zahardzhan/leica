;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для скачивания."
       :author "Роман Захаров"}
  env.download
  (:use env aux match)
  (:require :reload action program)
  (:import (org.apache.commons.httpclient URI)))

(derive ::download-agent :env/default-agent)
(derive ::download-env   :env/default-env)

;;;; Агент

(defn download-agent 
  "Агент для скачивания.

  :address адрес задания
  :link    прямая ссылка на файл, который нужно скачать
  :tag     идентификатор по которому разделяются потоки загрузок
  :file    файл в который сохраняется скачанное
  :length  размер файла, что нужно скачать"
  
  [line rules]
  (let [[address actions] (match line rules {:action list})]
    (when (and address actions)
      (agent {:type ::download-agent
              :address (URI. address)
              :env empty-fn
              :link nil :name nil :tag nil :file nil :length nil
              :actions actions
              :program program/reflex-download
              :alive true :fail false :percept nil :action nil}))))

(defn download-agents [lines rules]
  (remove (comp not agent?) (map #(download-agent % rules) lines)))

(defmethod run-agent [::download-agent :state] [ag-state]
  (let [tag (:tag ag-state)
        env (related-env ag-state)]
    (cond (dead? ag-state) ag-state

          (not tag) (let [new-state (action/execute-action ag-state)]
                      (cond (dead? new-state) (done env *agent*)
                            (fail? new-state) (run-agent *agent*)
                            (:tag new-state) (do (add-tag env (:tag new-state))
                                                 (received-tag env *agent*))
                            :else (run-agent *agent*))
                      new-state)

          (tag-locked? env tag) ag-state

          :else (let [new-state (with-lock-env-tag env tag
                                  (action/execute-action ag-state))]
                  (cond (dead? new-state) (done env *agent*)
                        (fail? new-state) (done env *agent*)
                        :else (run-agent *agent*))
                  new-state))))

;;;; Окружение

(defn download-environment [& [{:keys [working-path done-path progress-agent 
                                       termination]
                                :or   {working-path nil
                                       done-path nil
                                       progress-agent nil
                                       termination empty-fn}}]]
  (agent {:type ::download-env :agents '() :tags {}
          :working-path working-path
          :done-path done-path
          :progress-agent progress-agent
          :termination termination}))

(defmethod run-env [::download-env :state] [env-state]
  (doseq [ag (:agents env-state)] (run-agent ag))
  env-state)

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