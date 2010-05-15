;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/

(ns #^{:author "Roman Zaharov"}
  leica
  (:gen-class)
  (:use :reload aux agent download hook console-progress clojure.contrib.command-line)
  (:require verified
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:import java.io.File
           java.util.Date
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)))

(in-ns 'leica)

(def *usage* 
     "Клиент для датакода.

Пишите о багах на zahardzhan@gmail.com.

Скачать файлы с датакода:
leica [ключи] [файл с адресами на скачивание] [директория для скачанных файлов]

Закачать файлы на датакод в домене [dsv|amur|khv|avangard|...]:
leica [ключи] -a домен:почтовый@адрес:пароль [файлы и директории для закачивания]
e")

(defn set-root-logger-log-level! [log-level]
  (let [root-logger (Logger/getLogger "")
        console-handler (first (.getHandlers root-logger))
        date-formatter (java.text.SimpleDateFormat. "HH:mm:ss")
        log-formatter (proxy [Formatter] []
                        (format 
                         [#^LogRecord record]
                         (str \return
                              (.format date-formatter (Date. (.getMillis record)))
                              \space (.getMessage record) \newline)))]
    (.setFormatter console-handler log-formatter)
    (.setLevel console-handler log-level)
    (.setLevel root-logger log-level)))

(defn -main [& args]
  (with-command-line args
    *usage*
    [[account a  "домен:имя@аккаунта:пароль для закачивания на датакод"]
     [move    m  "директория, в которую перемещать полностью скачанные файлы"]
     [report  r  "файл для отчета о закачанных"]
     [quiet?  q? "работать молча"]
     [debug?  d? "писать подробные сообщения для отлова багов"]
     remaining-args]

    (set-root-logger-log-level! (cond quiet? (Level/OFF)
                                      debug? (Level/FINE)
                                      :else  (Level/INFO)))

    (let [jobs-file    (some verified/jobs-file remaining-args)
          working-path (or (some verified/output-dir remaining-args)
                           (verified/output-dir (System/getProperty "user.dir")))
          done-path    (verified/output-dir move)]
      (when (and jobs-file working-path)
        (let [lines (duck/read-lines jobs-file)
              download-environment (make-download-env)]
          (doseq [line lines]
            (make-download-agent line
                                 :working-path working-path
                                 :done-path done-path
                                 :strategy reflex-with-transfer-of-control
                                 :environment download-environment))
          (turn-on-cli-for-all-download-environments)
          (add-hook download-environment-termination-hook :system-exit (fn [e] (System/exit 0)))
          (doseq [a (agents download-environment)]
            (send-off a run)))))))
    
    ;; (cond account
    ;;       (let [[domain login pass] (account-attribs account)
    ;;             acc (datacod.account/datacod-account domain login pass)
    ;;             files (files-for-upload remaining-args)
    ;;             report-file (verified-log-file report)]
    ;;         (when (and acc (seq files))
    ;;           (let [e (env.upload/upload-environment
    ;;                    acc {:report-file report-file
    ;;                         :termination (fn [env]
    ;;                                        (print-succesfully-uploaded (env :agents))
    ;;                                        (System/exit 0))})]
    ;;             (add-agents e (env.upload/upload-agents files *upload-rules*))
    ;;             (await e)
    ;;             (run-env e)))))
