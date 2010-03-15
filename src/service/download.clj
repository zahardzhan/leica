;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns service.download)

(in-ns 'service.download)

(def services
     {::cod.data
      {:address #"http://[\w\.\-]*.data.cod.ru/\d+"
       :tags #{#"files3?.dsv.*.data.cod.ru" #"files2.dsv.*.data.cod.ru"}}

      ::77.35.112.8*
      {:address #"http://77.35.112.8[1234]/.+"}

      ::dsvload.net.ftpupload
      {:address #"http://dsvload.net/ftpupload/.+"}})

(defn match-service [line services]
  (some (fn [[service {address-pattern :address}]]
          (when-let [address (re-find address-pattern line)]
            {:service service :address address}))
        services))