;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns agent
  (:use aux clojure.set))

(defprotocol Environment-Protocol
  (done? [e]     "Environment is finished with its current tasks.")
  (terminate [e] "Destroy agent or environment when it is done."))

(defprotocol Agent-Protocol
  (get-action [a] "Execute agent program, get next action.")
  (execute [a] "Agent (if the agent is alive and has specified a legal
  action) takes the action.")
  (run [a] "Run agent program and execute an action.")
  (performance [a] "Return a number saying how well this agent is doing."))

(defrecord Environment [agents-ref])

(defrecord Agent [env-ref])

(defn agents "The agents belonging to this environment." [e]
  (deref (:agents-ref e)) )

(defn env "The environment belonging to this agent." [a]
  (derefed a :env-ref force deref))

(defn surrounding "The agents belonging to environment this agent belonging to." [a]
  (when-let [e (env a)] (difference (agents e) #{a})))

(defn binded? "Agent is binded to environment and vice-versa?"
  ([a]   (if ((agents (env a)) a) true false))
  ([a e] (and (identical? e (env a)) (binded? a))))

(defmulti bind   "Bind agent to environment and vice-versa."     dispatch-by-derefed-type-of-2-args)

(defmulti unbind "Unbind agent from environment and vice-versa." dispatch-by-derefed-type)

(defmethod bind [agent.Agent agent.Environment] [a e]
  (with-return e
    (when-not (binded? a e)
      (when (binded? a) (unbind a))
      (dosync (alter (:agents-ref e) union #{a})
              (ref-set (derefed a :env-ref force) e)))))

(defmethod unbind agent.Agent [a]
  (with-return a
    (when (binded? @a)
      (dosync (alter (:agents-ref (env a)) difference #{a})
              (ref-set (derefed a :env-ref force) nil)))))

(defn make-env
  "The world in which agents exist. Add new slots to hold various
  state information."  
  [& {:keys [agents]}]
  {:pre [(when-supplied agents (seq? (sequence agents)))]}
  (new Environment (ref (set agents))))

(defn make-agent [& {:keys [environment]}]
  {:pre  [(when-supplied environment (env? environment))]}
  (let-return [a (agent (new Agent (delay (ref environment))))]
              (when environment (bind a environment))))
