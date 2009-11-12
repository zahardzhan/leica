;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для скачивания."
       :author "Роман Захаров"}
  env.download
  (:use env aux match)
  (:require :reload program)
  (:import (org.apache.commons.httpclient URI)))

;;;; Агент

(defn download-agent 
  "Агент для скачивания.

  :name    имя агента
  :program слот программы агента, содержит функцию одного аргумента,
           результата восприятия окружения, и возвращает действие
  :actions набор функций-действий агента
  :percept последнее восприятие
  :action  последнее действие
  :alive   определяет жив агент или умер

  :address адрес задания
  :link    прямая ссылка на файл, который нужно скачать
  :tag     идентификатор по которому разделяются потоки загрузок
  :file    файл в который сохраняется скачанное
  :length  размер файла, что нужно скачать"
  
  [line rules]
  (let [[address actions] (match line rules {:action list})]
    (when (and address actions)
      (agent {:type :download
              :address (URI. address)
              :link nil :name nil :tag nil :file nil :length nil
              :actions actions
              :program program/reflex-download
              :alive true :fail false :percept nil :action nil}))))

(defn download-agents [lines rules]
  (remove (comp not agent?) (map #(download-agent % rules) lines)))

(defmethod run-agent- :download [ag-state env]
  (let [tag (:tag ag-state)]
    (cond (dead?- ag-state) ag-state

          (not tag) (let [new-state (execute-action ag-state @env)]
                      (cond (dead?- new-state) (done env *agent*)
                            (fail?- new-state) (run-agent *agent* env)
                            (:tag new-state) (do (add-tag env (:tag new-state))
                                                 (received-tag env *agent*))
                            :else (run-agent *agent* env))
                      new-state)

          (tag-locked? env tag) ag-state

          :else (let [new-state (with-lock-env-tag env tag
                                  (execute-action ag-state @env))]
                  (cond (dead?- new-state) (done env *agent*)
                        (fail?- new-state) (done env *agent*)
                        :else (run-agent *agent* env))
                  new-state))))

;;;; Окружение

(defn download-environment [& [{:keys [working-path termination]
                       :or   {working-path nil
                              termination #()}}]]
  (agent {:type :download :agents '() :tags {}
          :working-path working-path :termination termination}))

(defmethod run-env- :download [env-state]
  (doseq [ag (:agents env-state)] (run-agent ag *agent*))
  env-state)

(defmethod received-tag- :download [env-state ag]
  (when-let [next-alive-untagged-agent
             (next-after-when #(and (alive? %) (not (:tag (deref %))))
                              ag (:agents env-state))]
    (run-agent next-alive-untagged-agent *agent*))
  (run-agent ag *agent*)
  env-state)

(defmethod done- :download [env-state ag]
  (let [alive-unfailed-with-same-tag
        (some #(when (and (= (:tag @ag) (:tag @%)) (alive? %) (not (fail? %))) %)
              (:agents env-state))
        next-alive-with-same-tag
        (next-after-when #(and (= (:tag @ag) (:tag @%)) (alive? %))
                         ag (:agents env-state))]
    
    (cond alive-unfailed-with-same-tag
          (run-agent alive-unfailed-with-same-tag *agent*)

          next-alive-with-same-tag
          (run-agent next-alive-with-same-tag *agent*)

          (termination?- env-state) ((env-state :termination))))
  env-state)