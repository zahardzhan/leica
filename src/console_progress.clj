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

(ns console-progress
  (:use agent aux clojure.set))

(in-ns 'console-progress)

(def *console-width* 80)

(defn make-console-progress-agent []
  (make-agent :type ::console-progress-agent
              :environment nil
              :agents #{}))

(def *console-progress-agent* (make-console-progress-agent))

(defmulti show-console-progress   dispatch-by-type)
(defmulti hide-console-progress   dispatch-by-type)
(defmulti update-console-progress dispatch-by-type)

(defmulti console-progress dispatch-by-type)

(defmethod show-console-progress   nil [_ & __] nil)
(defmethod hide-console-progress   nil [_ & __] nil)
(defmethod update-console-progress nil [_ & __] nil)
(defmethod console-progress        nil [_ & __] nil)

(defmethod  show-console-progress ::console-progress-agent [console-progress-agent a]
  (assoc console-progress-agent :agents
         (union (:agents console-progress-agent) (set a))))

(defmethod hide-console-progress ::console-progress-agent [console-progress-agent a]
  (.print System/out (str "\r" (apply str (repeat *console-width* \space)) "\r")) 
  (assoc console-progress-agent :agents
         (difference (:agents console-progress-agent) (set a))))

(defmethod update-console-progress ::console-progress-agent [console-progress-agent]
  (with-return console-progress-agent
    (.print System/out "\r")
    (doseq [a (deref-seq (:agents console-progress-agent))]
      (.print System/out (console-progress a)))))
