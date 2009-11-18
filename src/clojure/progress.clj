;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для отображения прогресса (состояния процесса)."
       :author "Роман Захаров"}
  progress
  (:use match [clojure.contrib seq-utils test-is]))

(in-ns 'progress)

(def *console-width* 80)

(defn console-progress-agent
  "Конструктор агента, отображающего прогресс в консоли."
  [] 
  (agent {:type ::console :tags {}}))

(defmulti show-progress
  "Показать прогресс загрузочного агента.
   Формат сообщения: {:tag :name :progress :total :time}"
  (fn ([ag message] (:type ag))))

(defmulti hide-progress
  "Спрятать показ прогресса загрузочного агента.
   Формат сообщения: tag"
  (fn ([ag message] (:type ag))))  

(defmethod show-progress ::console [ag-state message]
  (let [{:keys [tag name progress total time]} message
        percent (int (if-not (zero? total) (* 100 (/ progress total)) 0))
        new-state (assoc ag-state :tags
                         (assoc (ag-state :tags) tag 
                                {:name name :percent percent}))]
    (.print System/out "\r")
    (doseq [tag (keys (:tags new-state))]
      (let [name (((new-state :tags) tag) :name)
            percent (((new-state :tags) tag) :percent)
            first-part (apply str (take 5 name))
            last-part  (apply str (take-last 7 name))]
        (.print System/out (str \[ first-part ".." last-part \space percent \% \]))))
    new-state))

(defmethod hide-progress ::console [ag-state tag]
  (.print System/out 
          (str "\r" (apply str (repeat *console-width* \space)) "\r"))
  (assoc ag-state :tags (dissoc (ag-state :tags) tag)))

(deftest console-progress-test
  (is (nil?
       (do (def cpa (console-progress-agent))
           (send cpa show-progress {:tag 1 :name "Boogie-bop phantom - 01.mkv" :progress 10 :total 20 :time nil})
           (send cpa show-progress {:tag 2 :name "Evangelion ReTake[03].rar"   :progress 10 :total 20 :time nil})
           (send cpa show-progress {:tag 2 :name "Evangelion ReTake[03].rar"   :progress 20 :total 20 :time nil})
           (send cpa hide-progress 2                                                                            )
           (send cpa show-progress {:tag 1 :name "Boogie-bop phantom - 01.mkv" :progress 20 :total 20 :time nil})
           ))))