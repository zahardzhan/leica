;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns agent
  (:use aux clojure.set clojure.test))

(defprotocol Environment-Protocol
  (done? [e]     "Environment is finished with its current tasks.")
  (terminate [e] "Destroy agent or environment when it is done."))

(defprotocol Agent-Protocol
  (get-action  [a] "Execute agent program, get next action.")
  (execute     [a] [a action] "Agent (if the agent is alive and has specified a legal
  action) takes the action.")
  (run         [a] [a & opts] "Run agent program and execute an action.")
  (performance [a] "Return a number saying how well this agent is doing."))

(defrecord Environment [agents-ref])

(defrecord Agent [env-ref])

(defn agents "The agents belonging to this environment." [e]
  (when e (deref (:agents-ref e))))

(defn env "The environment belonging to this agent." [a]
  (when a (derefed a :env-ref force deref)))

(defn surrounding "The agents belonging to environment this agent belonging to." [a]
  (when-let [e (env a)] (difference (agents e) #{a})))

(defn binded? "Agent is binded to environment and vice-versa?"
  ([a]   (boolean (when-let [ags (agents (env a))] (ags a))))
  ([a e] (and (identical? e (env a)) (binded? a))))

(defmulti bind   "Bind agent to environment and vice-versa."     dispatch-by-derefed-type-of-2-args)

(defmulti unbind "Unbind agent from environment and vice-versa." dispatch-by-derefed-type)

(defmethod bind :default [a e]
  (with-return e
    (when-not (binded? a e)
      (when (binded? a) (unbind a))
      (dosync (alter (:agents-ref e) union #{a})
              (ref-set (derefed a :env-ref force) e)))))

(defmethod unbind :default [a]
  (with-return a
    (when (binded? a)
      (dosync (alter (:agents-ref (env a)) difference #{a})
              (ref-set (derefed a :env-ref force) nil)))))

(defn make-env
  "The world in which agents exist. Add new slots to hold various
  state information."  
  [& {:keys [agents]}]
  {:pre [(when-supplied agents (seq? (sequence agents)))]}
  (new Environment (ref (set agents))))

(defn make-agent [& {:keys [environment]}]
  {:pre  [(when-supplied environment (instance? Environment environment))]}
  (let-return [a (agent (new Agent (delay (ref environment))))]
              (when environment (bind a environment))))

(deftest agent-bind-test
  (let [e1 (make-env)
        a1 (make-agent :environment e1)
        a2 (make-agent :environment e1)]
    (is (and (binded? a1 e1) (binded? a1) (binded? a2 e1) (binded? a2)))
    (unbind a1) (unbind a2)
    (is (not (or (binded? a1 e1) (binded? a1) (binded? a2 e1) (binded? a2))))
    (bind a1 e1)
    (is (and (binded? a1 e1) (binded? a1) (not (binded? a2 e1)) (not (binded? a2))))
    (bind a2 e1)
    (is (and (binded? a1 e1) (binded? a1) (binded? a2 e1) (binded? a2)))))
