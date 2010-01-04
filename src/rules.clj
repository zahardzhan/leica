;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Правила агентов."
       :author "Роман Захаров"}
  rules
  (:use :reload aux)
  (:require :reload action program datacod.action datacod.program))

(in-ns 'rules)

(def *default-upload-rule*
     {:program datacod.program/reflex-upload
      :actions {:upload datacod.action/upload
                :report datacod.action/report
                :pass   action/pass
                :die    action/die}})

(def #^{:doc "Таблица действий агентов для закачивания."}
     *upload-rules* *default-upload-rule*)

(def *default-download-rule*
     {:program program/reflex-download
      :actions {:get-link          action/get-link
                :get-name          action/get-name
                :get-tag           action/get-tag
                :get-file          action/get-file
                :get-length        action/get-length
                :move-to-done-path action/move-to-done-path
                :download          action/download
                :die               action/die
                :pass              action/pass}})

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     *download-rules*
     [[#"http://dsv.data.cod.ru/\d{6}"
       (merge *default-download-rule*
              {:actions 
               (merge (:actions *default-download-rule*)
                      {:get-link   datacod.action/get-link-and-name
                       :get-tag    (partial action/get-tag [#"files3?.dsv.data.cod.ru"
                                                            #"files2.dsv.data.cod.ru"])})})]
      [#"http://[\w\.]*data.cod.ru/\d+"
       (merge *default-download-rule*
              {:actions
               (merge (:actions *default-download-rule*)
                      {:get-link   datacod.action/get-link-and-name})})]
      [#"http://77.35.112.8[1234]/.+" *default-download-rule*]
      [#"http://dsvload.net/ftpupload/.+" *default-download-rule*]])