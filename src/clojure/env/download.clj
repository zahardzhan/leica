;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

(ns #^{:doc "asdasd."
       :author "RA"}
  env.download
  (:require :reload program)
  (:use :reload env aux match))

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
      (agent {:type ::download
              :address (new java.net.URI address)
              :link nil :name nil :tag nil :file nil :length nil
              :actions actions
              :program program/reflex-download
              :alive true :fail false :percept nil :action nil}))))

(defn download-agents [lines rules]
  (remove (comp not agent?) (map #(download-agent % rules) lines)))

(defmethod run-agent ::download [ag env]
  (let [tag (:tag ag)]
    (cond (dead? *agent*) ag 

          (not tag) (do (let [result (execute-action *agent* env)]
                          (cond (not (:alive result)) (send env done *agent*)
                                (:fail result) (send-off *agent* run-agent env)
                                (:tag result) (do (send env add-tag (:tag result))
                                                  (send env received-tag *agent*))
                                :else (send-off *agent* run-agent env))
                          result))

          (tag-locked? env tag) ag

          :else (do (let [result (with-lock-env-tag env tag
                                   (execute-action *agent* env))]
                      (cond (not (:alive result)) (send env done *agent*)
                            (:fail result) (send env done *agent*)
                            :else (send-off *agent* run-agent env))
                      result)))))

;;;; Окружение

(defn download-environment [& [{:keys [working-path termination]
                       :or   {working-path nil
                              termination #()}}]]
  (agent {:type ::download :agents '() :tags {}
          :working-path working-path :termination termination}))

(defmethod run-env ::download [env]
  (doseq [ag (:agents env)]
    (send-off ag run-agent *agent*))
  env)


(defmethod received-tag ::download [env ag]
  (when-let [next-alive-untagged-agent
             (next-after-when #(and (alive? %) (not (tag %)))
                              ag (agents *agent*))]
    (send-off next-alive-untagged-agent run-agent *agent*))
  (send-off ag run-agent *agent*)
  env)

(defmethod done ::download [env ag]
  (let [alive-unfailed-with-same-tag
        (some #(when (and (= (tag ag) (tag %)) (alive? %) (not (fail? %))) %)
              (agents *agent*))
        next-alive-with-same-tag
        (next-after-when #(and (= (tag ag) (tag %)) (alive? %))
                         ag (agents *agent*))]
    
    (cond alive-unfailed-with-same-tag
          (send-off alive-unfailed-with-same-tag run-agent *agent*)

          next-alive-with-same-tag
          (send-off next-alive-with-same-tag run-agent *agent*)

          (termination? *agent*) ((env :termination))))
  env)