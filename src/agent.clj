;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns agent
  (:use aux clojure.set))

(in-ns 'agent)

(def aim (comp :aim meta))

(def precedence (comp :precedence meta))

(def env (comp deref deref :env meta))

(def agents (comp :agents env))

(defn- set-sorted-by-precedence [& xs] 
  (apply (partial sorted-set-by 
                  (comparator #(< (precedence %1) (precedence %2))))
         xs))

(defn- make-env [& state]
  {:post [ref?]}
  (let [{:keys [agents]
         :or {agents (set-sorted-by-precedence)}}
        (apply array-map state)]
    (ref {:agents agents})))

(let [precedence-counter (atom 0)]
  (defn make-agent [& state]
    {:post [agent?]}
    (let [state (apply array-map state)]
      (agent (dissoc state :aim)
             :validator map?
             :meta {:aim (or (:aim state) ::dummy)
                    :precedence (swap! precedence-counter inc)
                    :env (ref (make-env))}))))

(defn bind
  {:post [map?]}
  ([x y & zs] {:pre  [(agent? x) (agent? y) (every? agent? zs)]}
     (let [unified (make-env 
                    :agents (union (agents x)
                                   (agents y)
                                   (apply union (map agents zs))
                                   (set-sorted-by-precedence x y)
                                   (apply set-sorted-by-precedence zs)))]
       (dosync (doseq [ag (derefed unified :agents)]
                 (ref-set (:env (meta ag)) unified)))
       (force unified))))