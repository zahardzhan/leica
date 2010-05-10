;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns progress
  (:use agent aux clojure.contrib.seq-utils)) ;;; !!! seq-utils

(in-ns 'progress)

(def *console-width* 80)

(defn console-progress-agent [& state]
  (let [{:keys [tags]} (apply hash-map state)]
    (make-agent :aim ::progress
                :tags tags)))

(defn show-progress [ag message]
  (let [{:keys [tag name progress total time]} message
        percent (int (if-not (zero? total) (* 100 (/ progress total)) 0))
        new-state (assoc ag :tags
                         (assoc (ag :tags) tag 
                                {:name name :percent percent}))]
    (.print System/out "\r")
    (doseq [tag (keys (:tags new-state))]
      (let [name (((new-state :tags) tag) :name)
            percent (((new-state :tags) tag) :percent)
            first-part (apply str (take 5 name))
            last-part  (apply str (take-last 7 name))]
        (.print System/out (str \[ first-part ".." last-part \space percent \% \]))))
    new-state))

(defn hide-progress [ag tag]
  (.print System/out 
          (str "\r" (apply str (repeat *console-width* \space)) "\r"))
  (assoc ag :tags (dissoc (ag :tags) tag)))
