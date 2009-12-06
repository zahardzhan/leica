;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Интерфейс окружений и агентов.

  Агент это что-то, что воспринимает свое окружение и действует.
  У агента есть тело, которое хранит состояние, и программа,
  которая выбирает действие, основываясь на состоянии агента и 
  восприятии окружения.

  Окружение координирует работу агентов. Агенты и окружение взаимосвязаны,
  т.е. ссылаются друг на друга.

  И окружения и агенты являются агентами Clojure, 
  и взаимодействуют посредством асинхронной отправки сообщений.

  Управление окружением и агентами осуществляется небольшим набором
  функций (асинхронных сообщений).

  API агента:
  run-agent запускает выполнение действий агентом (выполнение некоторых 
           действий может быть заблокировано другими агентами)
  stop-agent останавливает выполнение действий агентом. не реализовано

  API окружения:
  add-agent добавление агента в окружение (скорее, взаимное связывание агента
           с окружением)
  run-env  запуск агентов окружения

  Основное содержание тела агента:
  :type    тип агента (агент для загрузки/скачивания/...)
  :name    имя агента
  :alive   определяет жив агент или умер
  :env     окружение к которому привязан агент, ссылка на окружение находится
           внутри замыкания во избежание переполнения стэка
  :tag     идентификатор по которому определяются общие с другими агентами ресурсы.
           агент с соответствующим тагом блокирует таг в окружении на время
           выполнения действия, разрешённого только одному агенту с таким тагом.
  :program программа агента, содержит функцию одного аргумента,
           результата восприятия окружения, и возвращает действие
  :actions словарь с действиями агента {:имя-действия функция}
           результата восприятия окружения, и возвращает действие.
           восприятие передается в программу как словарь типа
           {:self собственное-тело-агента, ... еще какие-нибудь данные}
  :action  последнее действие агента
  :percept последнее восприятие агента
  
  Основное содержание тела окружения:
  :type    тип окружения (окружение загружающих/скачивающих/... агентов)
  :agents  список агентов в окружении
  :tags    словарь {tag (atom с условной переменной)} для управления
           взаимно-блокирующими агентами
  :debug   работа окружения в режиме отладки, агенты действуют пошагово
           (функция run-agent не вызывает рекурсивно сама себя)
  :termination продолжение вызываемое после остановки окружения"

       :author "Роман Захаров"}
  env
  (:require [clojure.contrib.logging :as log])
  (:use aux match))

(in-ns 'env)

(letfn [(dispatch [ag] ((if (agent? ag) @ag ag) :type))]
  (defn type-dispatch
    "Диспетчер по типу агента."
    ([ag] (dispatch ag))
    ([ag & args] (dispatch ag))))

(letfn [(dispatch [ag] (if (agent? ag) :agent :state))]
  (defn agent-dispatch
    "Диспетчер по ссылке на агент (:agent) и по телу агента (:state)."
    ([ag] (dispatch ag))
    ([ag & args] (dispatch ag))))

(letfn [(dispatch [ag] [(type-dispatch ag) (agent-dispatch ag)])]
  (defn type-agent-dispatch
    "Диспетчер по типу агента, и по ссылке на агент."
    ([ag] (dispatch ag))
    ([ag & args] (dispatch ag))))

;;;; Интерфейс к агенту

(defmulti run-agent 
  "Запуск агента на выполнение действий.
  При включенном в окружении дебаге агент выполнит только одно действие."
  type-agent-dispatch)
(defmulti stop-agent
  "Останавливает выполнение действий агентом."
  type-agent-dispatch)
(defmulti sleep
  "Усыпляет агента на некоторое время (в миллисекундах).
  Агент может спать только до/после выполнения действия."
  type-agent-dispatch)
(defmulti alive?
  "Жив ли агент?"
  type-agent-dispatch)
(defmulti dead?
  "Мёртв ли агент?"
  type-agent-dispatch)
(defmulti fail?
  "Агент провалил предыдущее действие?"
  type-agent-dispatch)
(defmulti related-env
  "Взаимосвязанное с агентом окружение.
  Возвращает замыкание с агентом-окружением внутри."
  type-agent-dispatch)

;;;; Интерфейс к окружению

(defmulti add-agent
  "Добавить агента в окружение."
  type-agent-dispatch)
(defmulti add-agents
  "Добавить много агентов в окружение."
  type-agent-dispatch)
(defmulti add-tag
  "Добавить таг в окружение."
  type-agent-dispatch)
(defmulti run-env
  "Запустить агентов в окружении."
  type-agent-dispatch)

(defmulti received-tag
  "Сообщение от агента о получении тага."
  type-agent-dispatch)
(defmulti done
  "Сообщение от агента о завершении работы агента."
  type-agent-dispatch)

(defmulti termination? 
  "Окружение закончило свою работу?"
  type-agent-dispatch)

(defmulti agents
  "Агенты в окружении."
  type-agent-dispatch)
(defmulti tags
  "Таги в окружении."
  type-agent-dispatch)

(defmulti debug?
  "Окружение/агент в режиме дебага?"
  type-agent-dispatch)

;;;; Реализация

(defmethod run-agent  [::default-agent :agent] [ag] (send-off ag run-agent))
(defmethod stop-agent [::default-agent :agent] [ag] (send-off ag stop-agent))

(defmethod sleep      [::default-agent :agent] [ag millis] (send-off ag sleep millis))
(defmethod sleep      [::default-agent :state] [ag millis] (Thread/sleep millis) ag)

(defmethod alive? [::default-agent :agent] [ag] (alive? (deref ag)))
(defmethod alive? [::default-agent :state] [ag] (:alive ag))

(defmethod dead?  [::default-agent :agent] [ag] (dead? (deref ag)))
(defmethod dead?  [::default-agent :state] [ag] (not (:alive ag)))

(defmethod fail?  [::default-agent :agent] [ag] (fail? (deref ag)))
(defmethod fail?  [::default-agent :state] [ag] (:fail ag))

(defmethod related-env [::default-agent :agent] [ag] ((:env (deref ag))))
(defmethod related-env [::default-agent :state] [ag] ((:env ag)))

(defmethod debug? [::default-agent :agent] [ag] (:debug (deref (related-env ag))))
(defmethod debug? [::default-agent :state] [ag] (:debug (deref (related-env ag))))
(defmethod debug? [::default-env   :agent] [ag] (:debug (deref ag)))
(defmethod debug? [::default-env   :state] [ag] (:debug ag))

(defmethod add-agent [::default-env :agent] [env ag]
  (send ag assoc :env (fn [] env))
  (send env add-agent ag))

(defmethod add-agent [::default-env :state] [env ag]
  (if-not (agent? ag) env
          (assoc env :agents (push (env :agents) ag))))

(defmethod add-agents [::default-env :agent] [env ags] (send env add-agents ags))
(defmethod add-agents [::default-env :state] [env agents]
  (doseq [ag agents] (add-agent *agent* ag))
  env)

(defmethod add-tag [::default-env :agent] [env tag] (send env add-tag tag))
(defmethod add-tag [::default-env :state] [env tag]
  (if (or (nil? tag) (contains? (env :tags) tag)) env
      (assoc env :tags (assoc (env :tags) tag (atom false)))))

(defmethod run-env [::default-env :agent] [env] (send env run-env))
(defmethod run-env [::default-env :state] [env]
  (doseq [ag (agents env)] (run-agent ag))
  env)

(defmethod received-tag [::default-env :agent] [env ag] (send env received-tag ag))
(defmethod done         [::default-env :agent] [env ag] (send env done ag))

(defmethod termination? [::default-env :agent] [env] (termination? (deref env)))
(defmethod termination? [::default-env :state] [env]
  (or (empty? (env :agents)) (every? dead? (env :agents))))

(defmethod agents       [::default-env :agent] [env] (agents (deref env)))
(defmethod agents       [::default-env :state] [env] (:agents env))

(defmethod tags         [::default-env :agent] [env] (tags (deref env)))
(defmethod tags         [::default-env :state] [env] (:tags env))

(defn tag-locked? [env tag]
  (when (contains? (tags env) tag)
    @((tags env) tag)))

(defn tag-lock [env tag]
  (when (contains? (tags env) tag)
    (reset! ((tags env) tag) true)))

(defn tag-unlock [env tag]
  (when (contains? (tags env) tag)
    (reset! ((tags env) tag) false)))

(defmacro with-lock-env-tag
  [env tag & body]
  `(do (tag-lock ~env ~tag)
       (let [result# (do ~@body)]
         (tag-unlock ~env ~tag)
         result#)))