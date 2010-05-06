;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/

(ns agent
  (:use aux clojure.set))

(in-ns 'agent)

(defn make-env
  "The world in which agents exist. Add new slots to hold various
  state information."  
  [& {:as opts :keys [type agents]}]
  {:post [(map? %)]}
  (with-meta (merge (dissoc opts :type :agents)
                    {:agents (ref (if (seq? (sequence agents))
                                    (set agents)
                                    (set nil)))})
    {:type (or type ::environment)}))

(defn env? [e]
  (and (map? e)
       (isa? (type e) ::environment)))

(defn agents "The agents belonging to this environment." [e]
  {:pre  [(env? e)]
   :post [(set? %)]}
  (deref (:agents e)))

(defn make-agent
  "An agent is something that perceives and acts. The action will be
  performed in the environment (if legal). Each agent also has a slot
  for the agent program, and one for its score as determined by the
  performance measure. Agents take actions (based on percepts of the
  agent program) and receive a score (based on the performance
  measure)."
  [& {:as opts
      :keys [type environment meta validator error-handler error-mode]}]
  {:pre  [(when-supplied type (keyword? type))
          (when-supplied environment (env? environment))]
   :post [(agent? %)]}
  (let-return [a (agent (with-meta (merge (dissoc opts :type :environment
                                                  :meta :validator
                                                  :error-handler :error-mode)
                                          {:env (delay (ref environment))})
                          {:type (or type ::agent)})
                        :meta meta
                        :validator validator
                        :error-handler error-handler
                        :error-mode error-mode)]))

(defn env-agent? [a]
  (and (agent? a)
       (isa? (derefed a type) ::agent)))

(defn env "The environment belonging to this agent." [a]
  {:pre  [(env-agent? a)]
   :post [(or (nil? %) (env? %))]}
  (derefed a :env force deref))

(defn surrounding "The agents belonging to environment this agent belonging to." [a]
  {:pre  [(env-agent? a)]
   :post [(set? %)]}
  (if-let [e (env a)]
    (agents e)
    (set nil)))

(defn binded? "Agent is binded to environment and vice-versa?"
  ([a]   (and ((surrounding a) a) true))
  ([a e] (and (identical? e (env a)) (binded? a))))

(defn unbind! "Unbind agent from environment and vice-versa." [a]
  {:pre  [(env-agent? a)]}
  (with-return a
    (when (binded? a)
      (dosync (alter (:agents (env a)) difference #{a})
              (ref-set (derefed a :env force) nil)))))

(defn bind! "Bind agent to environment and vice-versa." [a e]
  {:pre  [(env? e) (env-agent? a)]}
  (with-return e
    (when-not (binded? a e)
      (when (binded? a) (unbind! a))
      (dosync (alter (:agents e) union #{a})
              (ref-set (derefed a :env force) e)))))

;;;; Generic Functions that must be defined for each environment and
;;;; agent type.

;;; For each new type of environment or agent you want to define, you
;;; will need a define a type that derives from ::agent or
;;; ::environment, and you will need to write new methods (or inherit
;;; existing methods) for each of the following functions.  Here are
;;; the ones that will change for each new environment:

(defn type-dispatch
    ([x] (type x))
    ([x & args] (type x)))

(defn type2-dispatch
  [x y] [(type x) (type y)])

(defn type2-derefed-dispatch
  [x y] [(derefed x type) (derefed y type)])

(defmulti get-action
  "Execute agent program, get next action."
  type-dispatch)

(defmulti legal-actions
  "A list of the action operators that an agent can do."
  type-dispatch)

(defmulti performance
  "Return a number saying how well this agent is doing."
  type-dispatch)

(defmulti done?
  "True if Environment is finished with its current task(s)."
  type-dispatch)

(defmulti terminate
  "Destroy agent or environment when it is done."
  type-dispatch)

(defmulti execute
  "Agent (if the agent is alive and has specified a legal action)
  takes the action."
  type-dispatch)

(defmulti run
  "Run agent program and execute an action."
  type-dispatch)

(defmulti add-agent
  "Add an agent to the Environment."
  type2-derefed-dispatch)

(defmulti remove-agent
  "Remove an agent from the environment."
  type2-derefed-dispatch)

(defmethod add-agent [::environment ::agent] [e a]
  (with-return e
    (bind! a e)))

(defmethod remove-agent [::environment ::agent] [e a]
  (with-return e
    (unbind! a)))
