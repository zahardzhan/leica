;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  test.main
  (:use :reload aux match env
        [clojure.contrib command-line seq-utils test-is])
  (:require :reload env.download env.upload action program
            datacod.account datacod.action datacod.program
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:import (java.io File FileOutputStream InputStream)
           (java.util Date)
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)
           (org.apache.commons.httpclient URI HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'test.main)

(def *default-download-rule*
     {:get-link          action/get-link
      :get-name          action/get-name
      :get-tag           action/get-tag
      :get-file          action/get-file
      :get-length        action/get-length
      :move-to-done-path action/move-to-done-path
      :download          action/download
      :die               action/die
      :pass              action/pass})

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     *download-rules*
     [[#"http://dsv.data.cod.ru/\d{6}"
       (merge *default-download-rule*
              {:get-link   datacod.action/get-link-and-name
               :get-tag    (partial action/get-tag [#"files3?.dsv.data.cod.ru"
                                                    #"files2.dsv.data.cod.ru"])})]
      [#"http://[\w\.]*data.cod.ru/\d+"
       (merge *default-download-rule*
              {:get-link   datacod.action/get-link-and-name})]
      [#"http://77.35.112.8[1234]/.+" *default-download-rule*]
      [#"http://dsvload.net/ftpupload/.+" *default-download-rule*]])

(deftest main-test
  (def e (env.download/download-environment {:working-path (File. "/home/haru/inbox/dsv")}))
  (def a (env.download/download-agent "http://dsv.data.cod.ru/507882" *download-rules*))
  (add-agent e a)
  (run-env e)
  a
  e
  (agent-errors e)
  (agent-errors a)

  ;; ((@a :program) {:self @a :env @e})
  ;; (((@a :actions) :get-link) @a @e)
  ;; (def a3 (execute-action (execute-action (execute-action @a @e) @e) @e))
  ;; ((a3 :program) {:self a3 :env @e})
  ;; (def a4 (execute-action a3 @e))
  ;; ((a4 :program) {:self a4 :env @e})
  ;; (def a5 (execute-action a4 @e))
  ;; ((a5 :program) {:self a5 :env @e})
  ;; (def a6 (execute-action a5 @e))
  ;; ((a6 :program) {:self a6 :env @e})

  )

(comment 

(defn make-e [] (agent {:a nil}))
(defn make-a [] (agent {:e nil}))

(defn bindea [e a]
  (send a assoc :e (fn [] e))
  (send e assoc :a (fn [] a)))

(def e (make-e))
(def a (make-a))

(bindea e a)
e
a
((@a :e))
)

