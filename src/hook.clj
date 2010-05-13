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

(ns hook
  (use :reload aux))

(in-ns 'hook)

(defn make-hook
  ([] (atom {}))
  ([body] (atom body)))

(defmulti add-hook    dispatch-by-derefed-type)
(defmulti remove-hook dispatch-by-derefed-type)
(defmulti run-hook    dispatch-by-derefed-type)
(defmulti run-hooks   dispatch-by-derefed-type)

(defmethod add-hook clojure.lang.PersistentVector [hook function & {:keys [append]}]
  (with-return hook
    (when-not (some #(identical? function %) @hook)
      (if append
        (swap! hook into [function])
        (swap! hook (swap-args into) [function])))))

(defmethod run-hook clojure.lang.PersistentVector
  ([hook] (doseq [function @hook] (function)))
  ([hook & args] (doseq [function @hook] (apply function args))))

(defmethod add-hook clojure.lang.PersistentArrayMap [hook key function]
  (with-return hook (swap! hook assoc key function)))

(defmethod remove-hook clojure.lang.PersistentArrayMap [hook key]
  (with-return hook (swap! hook dissoc key)))

(defmethod run-hook clojure.lang.PersistentArrayMap
  ([hook]        (doseq [[key function] @hook] (function)))
  ([hook & args] (doseq [[key function] @hook] (apply function args))))
