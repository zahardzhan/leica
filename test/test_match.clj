;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты сопоставителя с образцом."
       :author "Роман Захаров"}
  test-match
  (:use :reload match)
  (:use clojure.test))

(in-ns 'test-match)

(deftest match-test
  (are [x y] (= x y)
       '(:a 2) (match :a [[:b 1]
                          [:a 2]]
                      {:action list})

       3 (case :c
               :a 1
               :b 2
               :c 3)))

;; (defmacro cond
;;   "Takes a set of test/expr pairs. It evaluates each test one at a
;;   time.  If a test returns logical true, cond evaluates and returns
;;   the value of the corresponding expr and doesn't evaluate any of the
;;   other tests or exprs. (cond) returns nil."
;;   [& clauses]
;;     (when clauses
;;       (list 'if (first clauses)
;;             (if (next clauses)
;;                 (second clauses)
;;                 (throw (IllegalArgumentException.
;;                          "cond requires an even number of forms")))
;;             (cons 'clojure.core/cond (next (next clauses))))))