;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Агенты и окружение.

  Агенты - это сущности, наблюдающие за своим окружением
  и действующие в нём, при этом их поведение рационально в том смысле, что 
  они способны к пониманию и их действия всегда направлены на достижение 
  какой-либо цели.

  Агенты конструируются из базового агента с помощью конструктора
  default-agent, который вызывается внутри конструкторов производных агентов.

  После создания агент должен быть связан с другими агентами в окружение.
  Связывание производится процедурой bind, все агенты синхронно ссылаются
  на новое, общее для связываемых агентов окружение, которое представляет
  собой задержанное множество связанных агентов. Задержка используется
  из-за самореферентности агентов в окружении.

  После связывания агенты запускаются процедурой run. Многие агенты 
  взаимоблокирующие, поэтому они сами определяют порядок, в котором они будут
  выполнять дейтсвия.

  Процедура stop останавливает выполнение действий агентом (не реализовано)."

       :author "Роман Захаров"}
  env
  (:use aux match clojure.set clojure.contrib.def))

(in-ns 'env)

(defnk default-agent
  "Конструктор базового агента, основа для всех производных агентов.
  Тело агента представляет собой простой хэш с ключами-свойствами.
  Для расширения тела агента конструктор принимает аргумент 

  state - хэш с дополнительными свойставами, который сливается с
    основным телом агента.

  Остальные ключи конструктора определяют некоторые базовы свойства агента.

  :type key

  Тип агента по-умолчанию это ::default-agent, все производные типы 
  агентов должны наследовать от этого типа.

  :name string

  Имя агента.

  :actions {:action-name function}
  
  Хэш с действиями агента. Каждое действие - это функция от тела агента, 
  возвращающая новое тело агента.

  :action :action-name-key

  Последнее действие агента. После создания агента это :create.

  :percept nil

  Последнее восприятие агента.

  :program function

  Агент находится в некотором состоянии и управляется агентской программой.
  Простая агентная программа может быть описана как функция, которая отображает 
  результат восприятия на дальнейшее действие.

  :alive boolean
 
  Определяет, жив ли агент.

  :fail boolean

  Определяет, удачно ли агент выполнил предыдущее действие.

  :tag nil

  Идентификатор по которому определяются общие с другими агентами ресурсы.

  :tag-lock (atom boolean)

  Атомн-замок тага (atom bool) запирается на время выполнения агентом
  действия, блокирующего действия агентов с тем же тагом.

  :debug debug

  Метка режима отладки агента, в котором агент действует пошагово,
  тоесть run не вызывает рекурсивно сама себя.

  :termination continuation

  Продолжение вызываемое после смерти всех агентов и остановки окружения.

  Также, в теле агента есть две особых ссылки:

  :self (delay agent)

  Задержанная ссылка на самого себя.

  :env (ref (delay #{sorted set of agents}))

  Окружение агента - это ссылка на задержанное множество всех агентов
  с которыми связан агент."
  [state, 
   :type ::default-agent, 
   :name nil,
   :actions {},
   :program empty-fn,
   :tag nil,
   :debug false,
   :termination empty-fn]
  (let [a (agent (merge state {:type type,
                               :name name,
                               :actions actions,
                               :action :create,
                               :percept nil,
                               :program program,
                               :alive true,
                               :fail false,
                               :tag tag,
                               :tag-lock (atom false),
                               :debug debug,
                               :termination termination}))]
    (send a assoc 
          :self (delay a)
          :env (ref (delay #{a})))
    (await a)
    a))

(defmulti bind 
  "Связывает агентов и их окружения в единое окружение."
  {:arglists '([ag1 ag2 & ags])}
  agent-or-type-dispatch)

(defmulti run
  "Запуск агента на выполнение действий.
  При включенном в окружении дебаге агент выполнит только одно действие."
  {:arglists '([ag])}
  agent-or-type-dispatch)

(defmulti done
  "Процедура вызывается самим агентом, когда он заканчивает свою работу."
  {:arglists '([ag])}
  agent-or-type-dispatch)

(defmulti stop
  "Останавливает выполнение действий агентом."
  {:arglists '([ag])}
  agent-or-type-dispatch)

(defmulti sleep
  "Усыпляет агента на некоторое время (в миллисекундах).
  Агент может спать только до/после выполнения действия."
  {:arglists '([ag])}
  agent-or-type-dispatch)

(defn self 
  "Возвращает самого агента, в качестве аргумента принимается агент или тело агента."
  [ag] (-> ag derefed :self))

(defn env "Возвращает множество агентов в окружении этого агента (включая его самого)."
  [ag] (-> ag derefed :env deref force))

(defmethod bind nil [x] nil)
(defmethod bind :agent [x y & zs]
  (let [unified-env (delay (union #{x y} (env x) (env y) 
                                  (set zs) (apply union (map env zs))))]
    (dosync (doseq [ag @unified-env] (ref-set (@ag :env) unified-env)))
    @unified-env))

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

(defn terminate 
  "Для агента в окружении вызывается `continuation' продолжение-терминатор."
  [ag] ((-> ag derefed :termination) ag))

(defn tag "Таг агента."
  [ag] (-> ag derefed :tag))

(defn tag-lock "Атом-замок тага агента."
  [ag] (-> ag derefed :tag-lock))

(defn tag-locked? "Замкнут (заблокирован) ли таг агента?"
  [ag] (-> ag derefed :tag-lock deref))

(defn lock-tag "Замкнуть (заблокировать) таг агента."
  [ag] (reset! (tag-lock ag) true))

(defn unlock-tag "Снять замок (блокировку) с тага агента." 
  [ag] (reset! (tag-lock ag) false))

(defn tag-locked-in-env? 
  "Замкнут (заблокирован) ли хоть один агент в окружении с таким же тагом?"
  [ag] (or (tag-locked? ag)
           (some (fn-and (partial same tag ag) tag-locked? (constantly true))
                 (env ag))))

(defmacro with-locked-tag
  "Замыкает (блокирует) таг агента на время выполнения им действия, блокирующего
  действия других агентов в окружении с таким же тагом."
  [ag & body]
  `(when-not (tag-locked-in-env? ~ag)
     (lock-tag ~ag)
     (let [result# (do ~@body)]
       (unlock-tag ~ag)
       result#)))