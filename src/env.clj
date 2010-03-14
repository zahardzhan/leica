;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns env
  (:use aux clojure.set)
  (:require fn))

(in-ns 'env)

(declare precedence)

(defn- become [x y] y)

(defn- set-sorted-by-precedence [& xs] 
  (apply (partial sorted-set-by 
                  (comparator (fn [x y] (with-deref [x y]
                                          (< (precedence x) (precedence y))))))
         xs))

(defn agent? [x]
  (isa? (type (derefed x)) ::agent))

(let [precedence-counter (atom 0)]
  (defn make-agent
    [& state] 
    {:post [agent?]}
    (let [state (apply array-map state)
          subtype (or (:subtype state) ::agent)
          state (dissoc state :subtype)]
      (agent state
             :validator map?
             :meta {:subtype subtype
                    :precedence (swap! precedence-counter inc)
                    :env (ref (delay {:agents (set-sorted-by-precedence)}))
                    :alive true
                    :fail false}))))

(defn precedence [ag]
  (derefed ag meta :precedence))

(defn this [ag] {:pre  [agent?]
                 :post [agent?]}
  (derefed ag :this force))

(defn env [ag] {:pre  [agent?]
                :post [map?]}
  (derefed ag :env deref force))

(defn bind
  {:post [map?]}
  ([x] {:pre [agent?]}
     (env x))
  ([x y & zs] {:pre  [(agent? x) (agent? y) (every? agent? zs)]}
     (let [unified (delay {:agents (union (:agents (env x))
                                          (:agents (env y))
                                          (apply union (map (comp :agents env) zs))
                                          (set-sorted-by-precedence x y)
                                          (apply set-sorted-by-precedence zs))})]
       (dosync (doseq [ag (derefed unified :agents) :when (agent? ag)]
                 (ref-set (@ag :env) unified)))
       (force unified))))

(defn percept [ag] {:pre  [agent?]
                    :post [fn?]}
  (with-deref [ag]
    ((:program ag) ag)))

(defn execute [action] {:pre  [fn?]
                        :post [agent?]}
  (deref (future (action))))

(defmulti run
  {:arglists '([ag])} 
  type)

(defmethod run nil [ag] nil)

(defmethod run clojure.lang.Agent [ag] 
  (send-off ag run))

(defmethod run ::agent [ag]
  (with-deref [ag]
    (execute (percept ag))))

(defn fail? [ag]
  (derefed ag :fail))

(defn alive? [ag]
  (derefed ag :alive))

(defn dead? (fn/not alive?))

(defn termination?
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

;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Действия агента.

  Действие агента это функция одного аргумента, обычно с побочным эффектом, -
  принимает тело агента и возвращает новое."
       :author "Роман Захаров"}
  action
  (:require [clojure.contrib.logging :as log])
  (:use aux env))

(in-ns 'action)


(defn after
  "Возвращает true, если последнее действие агента совпадает с аргументом.
  В качестве статуса `status' можно указать ключи:
  :successful - удачное завершение последнего действия и
  :failed - неудачное. "
  ([action ag] (with-deref [ag] (= action (:action ag))))
  ([status action ag] 
     (with-deref [ag]
       (and (after action ag)
            (case status
                  :successful ((no fail?) ag)
                  :failed     (fail? ag))))))

(defn pass [ag] ag)

(defn ok [ag] (assoc ag :fail false))
 
(defn fail [ag] (assoc ag :fail true))
 
(defn die [ag] (assoc ag :alive false))

(defn sleep [ag millis]
  (with-return ag
    (Thread/sleep millis)))