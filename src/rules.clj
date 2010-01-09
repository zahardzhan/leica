;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Правила агентов."
       :author "Роман Захаров"}
  rules
  (:use :reload aux)
  (:require :reload download.env action program datacod.action datacod.program))

(in-ns 'rules)

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     download-rules
     [[#"http://dsv.data.cod.ru/\d{6}"
       (merge-with merge download.env/default-download-agent-control-part 
                   {:actions {:get-link   datacod.action/get-link-and-name
                              :get-tag    (partial action/get-tag 
                                                   [#"files3?.dsv.data.cod.ru"
                                                    #"files2.dsv.data.cod.ru"])}})]
      [#"http://[\w\.]*data.cod.ru/\d+"
       (merge-with merge download.env/default-download-agent-control-part
                   {:actions {:get-link   datacod.action/get-link-and-name}})]
      [#"http://77.35.112.8[1234]/.+"
       download.env/default-download-agent-control-part]
      [#"http://dsvload.net/ftpupload/.+" 
       download.env/default-download-agent-control-part]])