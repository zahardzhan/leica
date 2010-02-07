;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

;;; октябрь, 2009

;;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:gen-class)
  (:use aux clojure.contrib.command-line)
  (:require env download.env progress rules verified
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
")

;; (defn account-attribs [line]
;;   (let [[_ domain login password]
;;         (re-find #"([^:]+):([^@]+@[^:]+):(.+)" line)]
;;     (when (and login password)
;;       [domain login password])))

;; (defn print-succesfully-uploaded [agents]
;;   (log/info
;;    (apply str "Загруженные файлы:" \newline
;;           (seq (map (fn [ag] (when-let [address (@ag :address)]
;;                                (format-link-for-forum (@ag :name) address)))
;;                     agents)))))

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
              progress-agent (progress/console-progress-agent)
              terminator (fn [_] (System/exit 0))
              agents (filter env/env-agent?
                             (for [line lines]
                               (download.env/download-agent 
                                rules/download-rules line
                                :working-path working-path
                                :done-path (when (not= working-path done-path) done-path)
                                :progress-agent progress-agent
                                :termination terminator)))]
          (apply env/bind agents)
          (dorun (map env/run agents)))))))
    
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