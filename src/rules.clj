;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Правила агентов."
       :author "Роман Захаров"}
  rules
  (:use :reload aux)
  (:require :reload 
            action download.action service.cod.data.download.action
            download.env download.program))

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
     [[#"http://[\w\.\-]*.data.cod.ru/\d+"
       (merge-with merge default-download-agent-control-part 
                   {:actions {:get-link service.cod.data.download.action/get-link-and-name
                              :get-tag  (partial download.action/get-tag [#"files3?.dsv.*.data.cod.ru"
                                                                          #"files2.dsv.*.data.cod.ru"])}})]
      [#"http://77.35.112.8[1234]/.+" default-download-agent-control-part]
      [#"http://dsvload.net/ftpupload/.+" default-download-agent-control-part]])