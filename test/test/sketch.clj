;;; -*- mode: clojure; coding: utf-8 -*-

(ns test.sketch
  (:use clojure.test)
  (:use :reload leica)
  (:require [async.http.client :as HTTP]
            [clojure.contrib.http.agent :as http])
  (:import java.io.File))

(def link1 "http://files2.dsv-region.data.cod.ru/proxy/3/?WyJmYmI4NmNjZjBiYTQ4NTEyY2Q1NTBmMmU4ZDQ3ZWEzOCIsMTI4MDAxOTg5MywiU21lc2hueWVfdXNiX3N0aWtpLnJhciIsInIxSldoNWo3NERoXC94NTV6T3dJZ2ZJUDN0aU04MElIbU9vSlJoazM3dGxQZGt1OTNPNkt4RXUyXC84N2FlZ2ZsSzd2bWd5cGVcL0M2UW5xcTE4cjdBRmVkRDNoSzhIYXRBXC9tTUdRMXg3Y0dMUGZWVlEyWW5wNHNsOEJ0NFdEenlVdWZBVFRnOEJST0czd2NTQ0R5UWh6KzJEN1AwUk5wNXFBaE5KcFgxOEtpS1k9Il0%3D")

;; (defmethod console-progress ::download-agent
;;   [{:as a :keys [name length]}]
;;   (let [size       (size a)
;;         percent    #(int (if (and (pos? length) (pos? size))
;;                            (* 100 (/ size length))
;;                            0))
;;         first-part #(apply str (take 5 name))
;;         last-part  #(apply str (take-last 7 name))]
;;     (cond (and name length size)
;;           (str \[ (first-part) ".." (last-part) \space (percent) \% \])

;;           :else nil)))

(do
  (def e0 (make-environment))
  (doseq [line ["http://dsv.data.cod.ru/868417"
                "http://dsv.data.cod.ru/867518"
                "http://dsv.data.cod.ru/866756"
                "http://dsv.data.cod.ru/868967"]]
    (make-download-agent line :environment e0
                         :path "/home/haru/Inbox")))

(comment
  (first (agents e0))
  (count (agents e0))
  (doseq [a (agents e0)]
    (send-off a run))
  (map #(derefed % state) (agents e0))
  (map #(derefed % fail-reason) (agents e0))
  (select-from (agents e0)
               :order-by #(derefed % :precedence)
               :where #(and (derefed % alive?)
                            (= :simplify/files2-dsv-*-data-cod-ru
                               (derefed % :service))))
  )
