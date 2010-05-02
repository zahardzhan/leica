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

(defprotocol PEnvironment
  "An abstract description of possible discrete Environments in which Agent(s) 
can perceive and act."
  (agents [e] "The Agents belonging to this Environment.")
  (add-agent [e, ag] "Add an agent to the Environment.")
  (remove-agent [e, ag] "Remove an agent from the environment.")
  (done? [e] "True if Environment is finished with its current task(s)."))

(defrecord Environment
  [#^clojure.lang.Ref -agents]
  PEnvironment
  (agents [e] (deref -agents)))

(defn make-env [& {:keys [agents]}]
  (new Environment (ref (or (set agents) (set nil)))))

(defn make-agent [state & {:as opts
                           :keys [meta validator error-handler error-mode]}]
  (let-return [a (agent state
                        :meta meta
                        :validator validator
                        :error-handler error-handler
                        :error-mode error-mode)]
              (alter-meta! a assoc :env (ref (make-env :agents [a])))))

(defn env? [e]
  (extends? PEnvironment (type e)))

(defn env-agent? [a]
  (and (agent? a) (env? (env a))))

(defn env [ag]
  (-> ag meta :env deref))

(defn surrounding [ag]
  (-> ag env agents))

(defn bind
  {:post [env?]}
  ([x y & zs] {:pre  [(env-agent? x) (env-agent? y) (every? env-agent? zs)]}
     (let-return [unified (make-env :agents (union (surrounding x)
                                                   (surrounding y)
                                                   (apply union (map surrounding zs))
                                                   (set [x y])
                                                   (set zs)))]
                 (dosync (doseq [ag (agents unified)]
                           (ref-set ((meta ag) :env) unified))))))
