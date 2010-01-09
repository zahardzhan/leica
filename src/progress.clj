;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для отображения прогресса (состояния процесса)."
       :author "Роман Захаров"}
  progress
  (:use aux match [clojure.contrib seq-utils test-is]))

(in-ns 'progress)

(def *console-width* 80)

(defn console-progress-agent
  "Конструктор агента, отображающего прогресс в консоли."
  [] 
  (agent {:type ::console :tags {}}))

(defmulti show-progress
  "Показать прогресс загрузочного агента.
   Формат сообщения: {:tag :name :progress :total :time}"
  agent-or-type-dispatch)

(defmulti hide-progress
  "Спрятать показ прогресса загрузочного агента.
   Формат сообщения: tag"
  agent-or-type-dispatch)

(defmethod show-progress :agent [ag message]
  (send-off ag show-progress message))
(defmethod show-progress ::console [ag message]
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

(defmethod hide-progress :agent [ag tag] (send-off ag hide-progress tag))
(defmethod hide-progress ::console [ag tag]
  (.print System/out 
          (str "\r" (apply str (repeat *console-width* \space)) "\r"))
  (assoc ag :tags (dissoc (ag :tags) tag)))