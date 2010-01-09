;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Правила агентов."
       :author "Роман Захаров"}
  rules
  (:use :reload aux)
  (:require :reload action datacod.action download.action download.env 
            program download.program))

(in-ns 'rules)

(def default-download-agent-control-part 
     {:program download.program/reflex
      :actions {:get-link          download.action/get-link
                :get-name          download.action/get-name
                :get-tag           download.action/get-tag
                :get-file          download.action/get-file
                :get-length        download.action/get-length
                :move-to-done-path download.action/move-to-done-path
                :download          download.action/download
                :die               action/die
                :pass              action/pass}})

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     download-rules
     [[#"http://dsv.data.cod.ru/\d{6}"
       (merge-with merge default-download-agent-control-part 
                   {:actions {:get-link   datacod.action/get-link-and-name
                              :get-tag    (partial action/get-tag 
                                                   [#"files3?.dsv.data.cod.ru"
                                                    #"files2.dsv.data.cod.ru"])}})]
      [#"http://[\w\.]*data.cod.ru/\d+"
       (merge-with merge default-download-agent-control-part
                   {:actions {:get-link   datacod.action/get-link-and-name}})]
      [#"http://77.35.112.8[1234]/.+"
       default-download-agent-control-part]
      [#"http://dsvload.net/ftpupload/.+" 
       default-download-agent-control-part]])