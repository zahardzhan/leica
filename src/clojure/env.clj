;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Интерфейс агентов.

  Под термином агент понимаются сущности, наблюдающие за окружающей средой
  и действующие в ней, при этом их поведение рационально в том смысле, что 
  они способны к пониманию и их действия всегда направлены на достижение 
  какой-либо цели.

  Управление агентами осуществляется через API, большая часть которого ---
  асинхронные сообщения:

  bind связывает агентов вместе
  run  запускает выполнение действий агентом (выполнение некоторых 
       действий может быть заблокировано другими агентами)
  stop останавливает выполнение действий агентом. не реализовано"

       :author "Роман Захаров"}
  env
  (:use aux match clojure.set))

(in-ns 'env)

(defn default-agent
  "Базовый агент."
  [& [{:keys [type name actions program tag debug termination]
       :or   {type ::default-agent, name nil, actions {}, program empty-fn,
              tag nil, debug false, termination empty-fn}}]]
  (let [a 
        (agent 
         { ;; Тип агента по-умолчанию это ::default-agent,
          ;; и все производные типы агентов должны наследовать от этого типа.
          :type type,
          
          ;; Имя агента.
          :name name,

          ;; Окружение агента --- это ссылка на задержанное множество всех агентов
          ;; с которыми связан агент:
          :env (ref (delay #{})),          
          ;; После создания агент должен быть связан с другими агентами.
          ;; Связывание производится процедурой bind, все агенты синхронно ссылаются
          ;; на новое, общее для связываемых агентов окружение. Задержка используется
          ;; из-за самореферентности окружения.

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
          :debug false,

          ;; Продолжение вызываемое после смерти всех агентов в остановки окружении.
          :termination termination})]
    (send a assoc
          ;; Задержанная ссылка на самого себя.
          :self (delay a))))

(defmulti bind 
  "Связывает агентов друг с другом."
  agent-dispatch)

(defmulti run
  "Запуск агента на выполнение действий.
  При включенном в окружении дебаге агент выполнит только одно действие."
  type-agent-dispatch)

(defmulti done
  "Агент закончил свою работу."
  type-agent-dispatch)

(defmulti stop
  "Останавливает выполнение действий агентом."
  type-agent-dispatch)

(defmulti sleep
  "Усыпляет агента на некоторое время (в миллисекундах).
  Агент может спать только до/после выполнения действия."
  type-agent-dispatch)

(defmulti self
  "Ссылка на самого себя."
  agent-dispatch)

(defmulti env
  "Множество агентов окружения."
  agent-dispatch)

(defmulti alive?
  "Жив ли агент?"
  type-agent-dispatch)

(defmulti dead?
  "Мёртв ли агент?"
  type-agent-dispatch)

(defmulti fail?
  "Агент провалил предыдущее действие?"
  type-agent-dispatch)

(defmulti tag
  "Таг агента."
  type-agent-dispatch)

(defmulti tag-lock
  "Атом замка тага агента."
  agent-dispatch)

(defmulti lock-tag
  "Замкнуть таг агента."
  agent-dispatch)

(defmulti unlock-tag 
  "Снять замок с тага агента."
  agent-dispatch)

(defmulti tag-locked?
  "Замкнут ли хоть один агент в окружении с тем же тагом?"
  agent-dispatch)
  
(defmulti run-env
  "Запустить агентов в окружении."
  type-agent-dispatch)

(defmulti termination? 
  "Агент и его окружение закончило свою работу?"
  agent-dispatch)

(defmulti debug?
  "Окружение/агент в режиме дебага?"
  agent-dispatch)

;;;; Реализация

(defmethod run nil [ag] nil)
(defmethod run [::default-agent :agent] [ag] (send-off ag run))

(defmethod done [::default-agent :agent] [ag] (send-off ag done))

(defmethod run-env :agent [ag] (doseq [a (env ag)] (run a)))
(defmethod run-env :state [ag] (doseq [a (env ag)] (run a)))

(defmethod stop [::default-agent :agent] [ag] (send-off ag stop))

(defmethod sleep [::default-agent :agent] [ag millis] (send-off ag sleep millis))
(defmethod sleep [::default-agent :state] [ag millis] (Thread/sleep millis) ag)

(defmethod alive? [::default-agent :agent] [ag] (:alive @ag))
(defmethod alive? [::default-agent :state] [ag] (:alive ag))

(defmethod dead? [::default-agent :agent] [ag] (not (:alive @ag)))
(defmethod dead? [::default-agent :state] [ag] (not (:alive ag)))

(defmethod fail? [::default-agent :agent] [ag] (:fail @ag))
(defmethod fail? [::default-agent :state] [ag] (:fail ag))

(defmethod env :agent [ag] (-> @ag :env deref force))
(defmethod env :state [ag] (->  ag :env deref force))

(defmethod self :agent [ag] ag)
(defmethod self :state [ag] (force (:self ag)))

(defmethod debug? :agent [ag] (:debug @ag))
(defmethod debug? :state [ag] (:debug ag))

(defmethod bind :agent [x y & zs]
  (let [unified-env (union #{x y} (env x) (env y) (set zs) (apply union (map env zs)))
        delayed-env (delay unified-env)]
    (dosync (doseq [ag unified-env] (ref-set (@ag :env) delayed-env)))
    unified-env))

(defmethod termination? :agent [ag] (termination? (deref ag)))
(defmethod termination? :state [ag] (or (empty? (env ag)) 
                                        (every? dead? (env ag))))

(defmethod tag [::default-agent :agent] [ag] (:tag @ag))
(defmethod tag [::default-agent :state] [ag] (:tag ag))

(defmethod tag-lock :agent [ag] (:tag-lock @ag))
(defmethod tag-lock :state [ag] (:tag-lock ag))

(defmethod tag-locked? :agent [ag] (tag-locked? @ag))
(defmethod tag-locked? :state [ag]
  (or (deref (tag-lock ag))
      (some (fn-and (comp deref tag-lock) (partial same tag ag))
            (env ag))))

(defmethod lock-tag :agent [ag] (reset! (tag-lock ag) true))
(defmethod lock-tag :state [ag] (reset! (tag-lock ag) true))

(defmethod unlock-tag :agent [ag] (reset! (tag-lock ag) false))
(defmethod unlock-tag :state [ag] (reset! (tag-lock ag) false))

(defmacro locking-tag [ag & body]
  `(when-not (tag-locked? ~ag)
     (lock-tag ~ag)
     (let [result# (do ~@body)]
       (unlock-tag ~ag)
       result#)))