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

;;;; Generic Functions that must be defined for each environment and
;;;; agent type.

;;; For each new type of environment or agent you want to define, you
;;; will need a define a type that derives from ::agent or
;;; ::environment, and you will need to write new methods (or inherit
;;; existing methods) for each of the following functions.  Here are
;;; the ones that will change for each new environment:

(defmulti get-action
  "Execute agent program, get next action."
  dispatch-by-type)

(defmulti legal-actions
  "A list of the action operators that an agent can do."
  dispatch-by-type)

(defmulti performance
  "Return a number saying how well this agent is doing."
  dispatch-by-type)

(defmulti done?
  "True if agent or environment is finished with its current task(s)."
  dispatch-by-type)

(defmulti terminate
  "Destroy agent or environment when it is done."
  dispatch-by-type)

(defmulti execute
  "Agent (if the agent is alive and has specified a legal action)
  takes the action."
  dispatch-by-type)

(defmulti run
  "Run agent program and execute an action."
  dispatch-by-type)

(defmulti bind
  "Bind agent to environment and vice-versa."
  dispatch-by-derefed-type-of-2-args)

(defmulti unbind
  "Unbind agent from environment and vice-versa."
  dispatch-by-derefed-type)

(defmethod done? nil [] nil)
(defmethod terminate nil [] nil)
(defmethod execute nil [] nil)
(defmethod run nil [] nil)

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
  {:pre  [(when-supplied type (keyword? type)
                         environment (env? environment))]
   :post [(agent? %)]}
  (let-return [a (agent (with-meta
                          (merge (dissoc opts :type :environment
                                         :meta :validator
                                         :error-handler :error-mode)
                                 {:env (delay (ref environment))})
                          {:type (or type ::agent)})
                        :meta meta
                        :validator validator
                        :error-handler error-handler
                        :error-mode error-mode)]
              (when environment (bind a environment))))

(defn env-agent-body? [a]
  (and (map? a) (isa? (type a) ::agent)))

(defn env-agent? [a]
  (and (agent? a) (env-agent-body? @a)))

(defn env-agent-or-body? [a]
  (isa? (derefed a type) ::agent))

(defn env "The environment belonging to this agent." [a]
  {:pre  [(env-agent-or-body? a)]
   :post [(or (nil? %) (env? %))]}
  (derefed a :env force deref))

(defn surrounding "The agents belonging to environment this agent belonging to." [a]
  {:pre  [(env-agent? a)]
   :post [(set? %)]}
  (if-let [e (env a)]
    (difference (agents e) #{a})
    (set nil)))

(defn binded? "Agent is binded to environment and vice-versa?"
  ([a]   (if ((agents (env a)) a) true false))
  ([a e] (and (identical? e (env a)) (binded? a))))

(defn bind-agent-to-environment [a e]
  {:pre  [(env? e) (env-agent? a)]}
  (with-return e
    (when-not (binded? a e)
      (when (binded? a) (unbind a))
      (dosync (alter (:agents e) union #{a})
              (ref-set (derefed a :env force) e)))))

(defn unbind-agent [a]
  {:pre  [(env-agent? a)]}
  (with-return a
    (when (binded? a)
      (dosync (alter (:agents (env a)) difference #{a})
              (ref-set (derefed a :env force) nil)))))

(defmethod bind [::agent ::environment] [a e]
  (bind-agent-to-environment a e))

(defmethod unbind ::agent [a]
  (unbind-agent a))
