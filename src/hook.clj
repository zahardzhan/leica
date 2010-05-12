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

(defn make-hook []
  (atom []))

(defmulti add-hook
  "(add-hook HOOK FUNCTION &optional APPEND LOCAL)  

  Add to the value of HOOK the function FUNCTION.  FUNCTION is not
  added if already present.  FUNCTION is added (if necessary) at the
  beginning of the hook list unless the optional argument APPEND is
  non-nil, in which case FUNCTION is added at the end.

  HOOK should be a symbol, and FUNCTION may be any valid function.  If
  HOOK is void, it is first set to nil.  If HOOK's value is a single
  function, it is changed to a list of functions."

  dispatch-by-derefed-type)

(defmulti run-hook dispatch-by-derefed-type)

(defmethod add-hook clojure.lang.PersistentVector
  [hook function & {:keys [append]}]
  (with-return hook
    (when-not (some #(identical? function %) @hook)
      (if append
        (swap! hook into [function])
        (swap! hook (swap-args into) [function])))))

(defmethod run-hook clojure.lang.PersistentVector
  ([hook] (doseq [function @hook] (function)))
  ([hook & args] (doseq [function @hook] (apply function args))))
