;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Интерфейс агентов.

  Под термином агент понимаются сущности, наблюдающие за окружающей средой
  и действующие в ней, при этом их поведение рационально в том смысле, что 
  они способны к пониманию и их действия всегда направлены на достижение 
  какой-либо цели.

  Управление агентами осуществляется через API:

  bind связывает агентов вместе
  run  запускает выполнение действий агентом (выполнение некоторых 
       действий может быть заблокировано другими агентами)
  stop останавливает выполнение действий агентом. не реализовано"

       :author "Роман Захаров"}
  env
  (:use aux match clojure.set clojure.contrib.def))

(in-ns 'env)

(defnk default-agent
  "Базовый агент."
  [:type ::default-agent, :name nil, :actions {}, :program empty-fn, :tag nil,
   :debug false, :termination empty-fn]
  (let [a
        (agent 
         { ;; Тип агента по-умолчанию это ::default-agent,
          ;; и все производные типы агентов должны наследовать от этого типа.
          :type type,
          
          ;; Имя агента.
          :name name,

          ;; Хэш с действия агента {:имя-действия действие},
          ;; где действие - это функция от тела агента, возвращающая новое тело агента.
          :actions actions,
  
          ;; Последнее действие агента. После создания агента это :create.
          :action :create,

          ;; Последнее восприятие агента.
          :percept nil,

          ;; Агент находится в некотором состоянии и управляется агентской программой.
          ;; Простая агентная программа может быть описана как функция, которая отображает 
          ;; результат восприятия на дальнейшее действие.
          :program program,

          ;; Жив ли агент?
          :alive true,

          ;; Удачно ли агент выполнил предыдущее действие?
          :fail false,

          ;; Идентификатор по которому определяются общие с другими агентами ресурсы.
          :tag tag,

          ;; Атомный замок тага (atom bool) запирается на время выполнения агентом
          ;; действия, блокирующего действия агентов с тем же тагом.
          :tag-lock (atom false),

          ;; Метка режим отладки агента, в котором агент действует пошагово,
          ;; тоесть функция run-agent не вызывает рекурсивно сама себя.
          :debug debug,

          ;; Продолжение вызываемое после смерти всех агентов и остановки окружении.
          :termination termination})]
    (send a assoc
          ;; Задержанная ссылка на самого себя.
          :self (delay a)
          
          ;; Окружение агента --- это ссылка на задержанное множество всех агентов
          ;; с которыми связан агент:
          :env (ref (delay #{a})),          
          ;; После создания агент должен быть связан с другими агентами.
          ;; Связывание производится процедурой bind, все агенты синхронно ссылаются
          ;; на новое, общее для связываемых агентов окружение. Задержка используется
          ;; из-за самореферентности окружения.
          )
    (await a)
    a))

(defmulti bind 
  "Связывает агентов друг с другом."
  agent-or-type-dispatch)

(defmulti run
  "Запуск агента на выполнение действий.
  При включенном в окружении дебаге агент выполнит только одно действие."
  agent-or-type-dispatch)

(defmulti done
  "Агент закончил свою работу."
  agent-or-type-dispatch)

(defmulti stop
  "Останавливает выполнение действий агентом."
  agent-or-type-dispatch)

(defmulti sleep
  "Усыпляет агента на некоторое время (в миллисекундах).
  Агент может спать только до/после выполнения действия."
  agent-or-type-dispatch)

(defn self "Ссылка на самого себя." 
  [ag] (-> ag derefed :self))

(defn env "Множество агентов окружения." 
  [ag] (-> ag derefed :env deref force))

(defmethod bind nil [x] nil)
(defmethod bind :agent [x y & zs]
  (let [unified-env (union #{x y} (env x) (env y) (set zs) (apply union (map env zs)))
        delayed-env (delay unified-env)]
    (dosync (doseq [ag unified-env] (ref-set (@ag :env) delayed-env)))
    unified-env))

(defmethod run nil [ag] nil)
(defmethod run :agent [ag] (send-off ag run))
(defmethod run ::default-agent [ag] ag)

(defmethod done nil [ag] nil)
(defmethod done :agent [ag] (send-off ag done))
(defmethod done ::default-agent [ag] ag)

(defmethod sleep nil [ag] nil)
(defmethod sleep :agent [ag millis] (send-off ag sleep millis))
(defmethod sleep ::default-agent [ag millis] (Thread/sleep millis) ag)

(defn run-env "Запустить агентов в окружении."
  [ag] (doseq [a (env ag)] (run a)))

(defn alive? "Жив ли агент?"
  [ag] (-> ag derefed :alive))

(defn dead? "Мёртв ли агент?" 
  [ag] (-> ag derefed :alive not))

(defn fail? "Агент провалил предыдущее действие?"
  [ag] (-> ag derefed :fail))

(defn debug? "Окружение/агент в режиме дебага?"
  [ag] (-> ag derefed :debug))

(defn termination? "Агент и его окружение закончило свою работу?"
  [ag] (or (empty? (env ag))
           (every? dead? (env ag))))

(defn terminate "Вызвать убийственное продолжение."
  [ag] ((-> ag derefed :termination) ag))

(defn tag "Таг агента."
  [ag] (-> ag derefed :tag))

(defn tag-lock "Атом замка тага агента."
  [ag] (-> ag derefed :tag-lock))

(defn tag-locked? "Таг агента на замке?"
  [ag] (-> ag derefed :tag-lock deref))

(defn lock-tag "Замкнуть таг агента."
  [ag] (reset! (tag-lock ag) true))

(defn unlock-tag "Снять замок с тага агента." 
  [ag] (reset! (tag-lock ag) false))

(defn tag-locked-in-env? "Замкнут ли хоть один агент в окружении с тем же тагом?"
  [ag] (or (tag-locked? ag)
           (some (fn-and (partial same tag ag) tag-locked? (constantly true))
                 (env ag))))

(defmacro with-locked-tag [ag & body]
  `(when-not (tag-locked-in-env? ~ag)
     (lock-tag ~ag)
     (let [result# (do ~@body)]
       (unlock-tag ~ag)
       result#)))