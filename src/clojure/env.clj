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

(defn- agent-state-type-dispatch
  ([ag] (:type ag))
  ([ag arg] (:type ag)))

;;;; Интерфейс к состоянию агента

(defmulti run-agent-  agent-state-type-dispatch)
(defmulti stop-agent- agent-state-type-dispatch)
(defmulti alive?-     agent-state-type-dispatch)
(defmulti dead?-      agent-state-type-dispatch)
(defmulti fail?-      agent-state-type-dispatch)

(defn run-agent  [ag env] (send-off ag run-agent- env))
(defn stop-agent [ag env] (send-off ag stop-agent- env))
(defn alive? [ag] (alive?- (deref ag)))
(defn dead?  [ag] (dead?- (deref ag)))
(defn fail?  [ag] (fail?- (deref ag)))

;;;; Реализация ацессоров

(defmethod alive?- :default [ag] (:alive ag))
(defmethod dead?-  :default [ag] (not (:alive ag)))
(defmethod fail?-  :default [ag] (:fail ag))

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
                              (cond (dead?- new-state) "агент умер"
                                    (fail?- new-state) "агент провалился"
                                    :else "успешно"))))))]
    (.start thread)
    (.join thread)
    @result))

;;;; Окружение

(defstruct env-body :type :agents :termination)

(defmulti add-agent-    agent-state-type-dispatch)
(defmulti add-agents-   agent-state-type-dispatch)
(defmulti add-tag-      agent-state-type-dispatch)
(defmulti run-env-      agent-state-type-dispatch)

(defmulti received-tag- agent-state-type-dispatch)
(defmulti done-         agent-state-type-dispatch)

(defmulti termination?- agent-state-type-dispatch)

(defn add-agent    [env ag]  (send env add-agent- ag))
(defn add-agents   [env ags] (send env add-agents- ags))
(defn add-tag      [env tag] (send env add-tag- tag))
(defn run-env      [env]     (send env run-env-))

(defn received-tag [env ag]  (send env received-tag- ag))
(defn done         [env ag]  (send env done- ag))

(defn agents       [env] (:agents (deref env)))
(defn tags         [env] (:tags   (deref env)))
(defn termination? [env] (termination?- (deref env)))

(defmethod add-agent- :default [env ag]
  (if-not (agent? ag) env
          (assoc env :agents (push (env :agents) ag))))

(defmethod add-agents- :default [env agents]
  (doseq [ag agents] (send *agent* add-agent- ag))
  env)

(defmethod add-tag- :default [env tag]
  (if (or (nil? tag) (contains? (env :tags) tag)) env
      (assoc env :tags (assoc (env :tags) tag (atom false)))))
  
(defmethod termination?- :default [env]
  (or (empty? (env :agents)) (every? dead? (env :agents))))

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