;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты окружения."
       :author "Роман Захаров"}
  test.env
  (:use :reload aux match env)
  (:use clojure.test clojure.set [clojure.contrib seq-utils]))

(in-ns 'test.env)

(deftest env-test
  (let [a (default-agent {:name 'mate :tag 1})
        b (default-agent {:name 'feed :tag 1})
        c (default-agent {:name 'kill :tag 2})
        d (default-agent {:name 'repeat :tag 2})]
    (bind a b c d)

    (is (same env a b c d @a @b @c @d))
    (is (= 4 (count (env a))))
    (is (same self a @a))

    (is (and (alive? a)
             (alive? @a)
             (not (dead? a))
             (not (dead? @a))
             (not (fail? a))
             (not (fail? @a))))

    (is (same tag a b @a @b))
    (is (not (same tag b c)))

    (is (not (tag-locked? d)))
    (is (locking-tag c (tag-locked? d)))
    (is (not (tag-locked? d)))))

;; (deftest same-type-dispatch-test
;;   (are [x y] (= x y)
;;        (same-type-dispatch (agent {:type :a}) (agent {:type :b})) :env/different-types
;;        (same-type-dispatch (agent {:type :a}) (agent {:type :a})) :a))