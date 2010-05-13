;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns console-progress
  (:use agent aux clojure.set))

(in-ns 'console-progress)

(def *console-width* 80)
(def *console-progress-agent* nil)

(defn make-console-progress-agent []
  (make-agent :type ::console-progress-agent
              :environment nil
              :agents #{}))

(defn- show-console-progress [console-progress-agent a]
  (assoc console-progress-agent :agents
         (union (:agents console-progress-agent) (set a))))

(defn- hide-console-progress [console-progress-agent a]
  (.print System/out (str "\r" (apply str (repeat *console-width* \space)) "\r")) 
  (assoc console-progress-agent :agents
         (difference (:agents console-progress-agent) (set a))))

(defmacro with-show-progress-in-console
  "a is agent body."
  [a & body]
  (cond (or (nil? *console-progress-agent*)
            (not (env-agent-body? a)))
        (do ~@body)

        :else `(try (send-off *console-progress-agent* show-console-progress a)
                    (do ~@body)
                    (finally (send-off *console-progress-agent* hide-console-progress a)))))

(defmulti console-progress dispatch-by-type)

(defn- show-progress-of-all-agents [console-progress-agent]
  (with-return console-progress-agent
    (.print System/out "\r")
    (doseq [a (:agents console-progress-agent)]
      (.print System/out (console-progress a)))))

(defn update-console-progress []
  (when *console-progress-agent*
    (send-off *console-progress-agent* show-progress-of-all-agents)))
