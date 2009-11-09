;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

;;; env.clj: интерфейс окружения.

(ns #^{:doc
       "Агент это что-то, что воспринимает свое окружение и действует.
  У агента есть тело, которое хранит состояние, и программа,
  которая выбирает действие, основываясь на состоянии агента и 
  восприятии окружения. Действие агента возвращается обратно в окружение
  для его выполнения. Представление восприятия и действия зависит от конкретного
  окружения и агента."
       :author "Роман Захаров"}
  env
  (:require [clojure.contrib.logging :as log])
  (:use aux match))

;;;; Агент

(defstruct agent-body :type :name :alive :program :actions)

(defn- agent-type-dispatch
  ([ag] (:type @ag))
  ([ag env] (:type @ag)))

;;;; Мутаторы агента

(defmulti run-agent  agent-type-dispatch)
(defmulti stop-agent agent-type-dispatch)

;;;; Ацессоры агента

(defmulti alive?  agent-type-dispatch)
(defmulti dead?   agent-type-dispatch)
(defmulti fail?   agent-type-dispatch)

(defmulti name-   agent-type-dispatch)
(defmulti tag     agent-type-dispatch)

;;;; Реализация ацессоров

(defmethod alive? :default [ag] (:alive @ag))
(defmethod dead?  :default [ag] (not (:alive @ag)))
(defmethod fail?  :default [ag] (:fail @ag))

(defmethod name-  :default [ag] (:name @ag))
(defmethod tag    :default [ag] (:tag @ag))

;;;; Закрытые мутаторы

(defn execute-action [ag env]
  (let [result (atom nil)
        thread
        (Thread. 
         #(let [percept {:self @ag :env @env}
                action  ((@ag :program) percept)]
            (log/debug (str (or (@ag :name) (@ag :address)) " " action))
            (let [new-state (((@ag :actions) action) @ag @env)]
              (reset! result new-state)
              (log/debug (str (or (@ag :name) (@ag :address)) " " action " "
                              (cond (:not (:alive new-state)) "агент умер"
                                    (:fail new-state) "агент провалился"
                                    :else "успешно"))))))]
    (.start thread)
    (.join thread)
    @result))

;;;; Окружение

(defstruct env-body :type :agents :termination)

(defmulti add-agent (fn [env ag] (:type env)))
(defmulti add-agents (fn [env ags] (:type env)))
(defmulti add-tag (fn [env tag] (:type env)))
(defmulti run-env :type)

(defmulti agents #(:type @%))
(defmulti tags #(:type @%))
(defmulti termination? #(:type @%))

(defmulti received-tag (fn [env ag] (:type env)))
(defmulti done (fn [env ag] (:type env)))

(defmethod add-agent :default [env ag]
  (if-not (agent? ag) env
          (assoc env :agents (push (env :agents) ag))))

(defmethod add-agents :default [env agents]
  (doseq [ag agents]
    (send *agent* add-agent ag))
  env)

(defmethod add-tag :default [env tag]
  (if (or (nil? tag) (contains? (env :tags) tag)) env
      (assoc env :tags (assoc (env :tags) tag (atom false)))))
  
(defmethod agents :default [env]
  (@env :agents))

(defmethod tags :default [env]
  (@env :tags))

(defmethod termination? :default [env]
  (or (empty? (agents env)) (every? dead? (agents env))))

(defn tag-locked? [env tag]
  (when (contains? (@env :tags) tag)
    @((@env :tags) tag)))

(defn tag-lock [env tag]
  (when (contains? (@env :tags) tag)
    (reset! ((@env :tags) tag) true)))

(defn tag-unlock [env tag]
  (when (contains? (@env :tags) tag)
    (reset! ((@env :tags) tag) false)))

(defmacro with-lock-env-tag [env tag & body]
  `(do (tag-lock ~env ~tag)
       (let [result# ~@body]
         (tag-unlock ~env ~tag)
         result#)))