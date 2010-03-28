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
        (apply hash-map state)]
    (ref {:agents agents})))

(let [precedence-counter (atom 0)]
  (defn make-agent [& state]
    {:post [agent?]}
    (let [state (apply hash-map state)]
      (agent (dissoc state :aim)
             :validator map?
             :meta {:aim (or (:aim state) ::agent)
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
