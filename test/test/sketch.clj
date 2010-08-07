;;; -*- mode: clojure; coding: utf-8 -*-

(ns test.sketch
  (:use clojure.test hooks log)
  (:use :reload leica)
  (:require
   [clojure.contrib.http.agent :as http]
   [clojure.contrib.pprint :as pp])
  (:import java.io.File))

(defn make-env-and-ags []
  (def e1 (make-environment))
  ;; (def a1 (make-download-agent "http://dsv-region.data.cod.ru/73895" :environment e1
  ;;                              :path "/home/haru/Inbox"))

  (doseq [line
          ["http://dsv-region.data.cod.ru/73895"
           "http://dsv.data.cod.ru/883971"
           "http://dsv.data.cod.ru/883969"]]
    (make-download-agent line :environment e1
                         :path "/home/haru/Inbox")))

(comment
  (make-env-and-ags)
  e1
  (strategy a1)
  ((strategy a1) @a1)
  (run @a1)
  (send-off a1 run)

  (count (agents e1))
  (count (filter alive? (agents e1)))
  
  (doseq [a (succeeded (agents e1))]
    (send-off a run))
  
  (map (fn [a] {:serv (service a) :goal (goal a) :run (run? a) :alive (alive? a) :fail (failed? a) :error (agent-error a)})
       (agents e1))

  (doseq [a (agents e1)]
    (goal! a :stop))

  (def asdf 1)
  (binding [asdf 2]
    @(future asdf))
  leica/progress-agent*
    )
