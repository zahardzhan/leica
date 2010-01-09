;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Тесты окружения."
       :author "Роман Захаров"}
  test.env
  (:use :reload aux match env)
  (:use clojure.test clojure.set clojure.contrib.seq-utils))

(in-ns 'test.env)

(deftest env-test
  (let [a (default-agent :name 'mate :tag 1)
        b (default-agent :name 'feed :tag 1)
        c (default-agent :name 'kill :tag 2)
        d (default-agent :name 'rept :tag 2)]
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

    (is (and (not (tag-locked? c))
             (not (tag-locked? d))))
    (locking-tag c (is (and (tag-locked? c)
                            (not (tag-locked? d))
                            (tag-locked-in-env? d))))
    (is (and (not (tag-locked? c))
             (not (tag-locked? d))))))