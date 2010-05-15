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

(ns cli
  (:use agent aux clojure.set))

(in-ns 'cli)

(def spaces (memoize (fn [spcs] (str \return (apply str (repeat spcs \space)) \return))))

(def *console-width* 80)

(def *spaces* (spaces *console-width*))

(def *console-progress* {:agents (atom #{})})

(defmulti  console-progress dispatch-by-type)
(defmethod console-progress nil [_ & __] nil)

(defn show-console-progress [a]
  (swap! (*console-progress* :agents) union #{a}))

(defn hide-console-progress [a]
  (.print System/out *spaces*)
  (swap! (*console-progress* :agents) difference #{a}))

(defn hide-all-console-progress []
  (doseq [a (derefed (:agents *console-progress*))]
    (hide-console-progress a)))

(defn update-console-progress []
  (.print System/out "\r")
  (doseq [a (derefed (:agents *console-progress*))]
    (.print System/out (derefed a console-progress))))
