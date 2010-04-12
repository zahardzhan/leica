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

(defn tag [ag]
  (-> ag meta :tag))

(defn precedence [ag]
  (-> ag meta :precedence))

(defn env [ag]
  (-> ag meta :env deref))

(defn surrounding [ag]
  (-> ag env deref :agents))

(defn- set-sorted-by-precedence [& xs] 
  (apply (partial sorted-set-by 
                  (comparator #(< (precedence %1) (precedence %2))))
         xs))

(defn- make-env [& {:as state :keys [agents]}]
  (ref (merge state {:agents (or (apply set-sorted-by-precedence agents)
                                 (set-sorted-by-precedence))})))

(let [precedence-counter (atom 0)]
  (defn make-agent [state & {:as opts
                             :keys [tag meta validator
                                    error-handler error-mode]
                             :or {tag ::agent}}]
    (let [a (agent state
                   :meta meta
                   :validator validator
                   :error-handler error-handler
                   :error-mode error-mode)]
      (with-return a
        (alter-meta! a assoc
                     :tag tag
                     :precedence (swap! precedence-counter inc))
        (alter-meta! a assoc :env (ref (make-env :agents [a])))))))

(defn bind
  {:post [ref?]}
  ([x y & zs] {:pre  [(agent? x) (agent? y) (every? agent? zs)]}
     (let [unified (make-env :agents (union (surrounding x)
                                            (surrounding y)
                                            (apply union (map surrounding zs))
                                            (set-sorted-by-precedence x y)
                                            (apply set-sorted-by-precedence zs)))]
       (with-return unified
         (dosync (doseq [ag (@unified :agents)]
                   (ref-set ((meta ag) :env) unified)))))))
