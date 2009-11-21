;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Интерфейс и дефолтные функции окружения и агентов.

  Агент это что-то, что воспринимает свое окружение и действует.
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

(defstruct agent-body :type :name :alive :program :actions :action)

(defn- agent-or-state-dispatch [ag]
  (cond (agent? ag) [(:type @ag) :agent]
        :else       [(:type ag)  :state]))

(defn- type-dispatch
  ([ag] (agent-or-state-dispatch ag))
  ([ag args] (agent-or-state-dispatch ag)))

;;;; Интерфейс к агенту

(defmulti run-agent   type-dispatch)
(defmulti stop-agent  type-dispatch)
(defmulti alive?      type-dispatch)
(defmulti dead?       type-dispatch)
(defmulti fail?       type-dispatch)

(defmethod run-agent [::default-agent :agent] [ag env] 
  (send-off ag run-agent env))

(defmethod stop-agent [::default-agent :agent] [ag env] 
  (send-off ag stop-agent env))

(defmethod alive? [::default-agent :agent] [ag] (alive? (deref ag)))
(defmethod dead?  [::default-agent :agent] [ag] (dead? (deref ag)))
(defmethod fail?  [::default-agent :agent] [ag] (fail? (deref ag)))

(defmethod alive? [::default-agent :state] [ag] (:alive ag))
(defmethod dead?  [::default-agent :state] [ag] (not (:alive ag)))
(defmethod fail?  [::default-agent :state] [ag] (:fail ag))

;;;; Закрытые мутаторы

(defn execute-action [ag env]
  (let [result (atom nil)
        thread
        (Thread. 
         #(let [percept {:self ag :env env}
                action  ((ag :program) percept)]
            (log/debug (str (or (ag :name) (ag :address)) " " action))
            (let [new-state (((ag :actions) action) ag env)]
              (reset! result new-state)
              (log/debug (str (or (ag :name) (ag :address)) " " action " "
                              (cond (dead? new-state) "агент умер"
                                    (fail? new-state) "агент провалился"
                                    :else "успешно"))))))]
    (.start thread)
    (.join thread)
    @result))

;;;; Окружение

(defstruct env-body :type :agents :termination)

(defmulti add-agent    type-dispatch)
(defmulti add-agents   type-dispatch)
(defmulti add-tag      type-dispatch)
(defmulti run-env      type-dispatch)

(defmulti received-tag type-dispatch)
(defmulti done         type-dispatch)

(defmulti termination? type-dispatch)

(defmulti agents       type-dispatch)
(defmulti tags         type-dispatch)

(defmethod add-agent    [::default-env :agent] [env ag]  (send env add-agent ag))
(defmethod add-agents   [::default-env :agent] [env ags] (send env add-agents ags))
(defmethod add-tag      [::default-env :agent] [env tag] (send env add-tag tag))
(defmethod run-env      [::default-env :agent] [env]     (send env run-env))

(defmethod received-tag [::default-env :agent] [env ag]  (send env received-tag ag))
(defmethod done         [::default-env :agent] [env ag]  (send env done ag))

(defmethod termination? [::default-env :agent] [env] (termination? (deref env)))

(defmethod agents       [::default-env :agent] [env] (agents (deref env)))
(defmethod tags         [::default-env :agent] [env] (tags   (deref env)))

(defmethod add-agent [::default-env :state] [env ag]
  (if-not (agent? ag) env
          (assoc env :agents (push (env :agents) ag))))

(defmethod add-agents [::default-env :state] [env agents]
  (doseq [ag agents] (send *agent* add-agent ag))
  env)

(defmethod add-tag [::default-env :state] [env tag]
  (if (or (nil? tag) (contains? (env :tags) tag)) env
      (assoc env :tags (assoc (env :tags) tag (atom false)))))
  
(defmethod termination? [::default-env :state] [env]
  (or (empty? (env :agents)) (every? dead? (env :agents))))

(defmethod agents [::default-env :state] [env]
  (:agents env))

(defmethod tags [::default-env :state] [env]
  (:tags env))

(defn tag-locked? [env tag]
  (when (contains? (tags env) tag)
    @((tags env) tag)))

(defn tag-lock [env tag]
  (when (contains? (tags env) tag)
    (reset! ((tags env) tag) true)))

(defn tag-unlock [env tag]
  (when (contains? (tags env) tag)
    (reset! ((tags env) tag) false)))

(defmacro with-lock-env-tag [env tag & body]
  `(do (tag-lock ~env ~tag)
       (let [result# ~@body]
         (tag-unlock ~env ~tag)
         result#)))