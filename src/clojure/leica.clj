;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

;;; октябрь, 2009

;;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:gen-class)
  (:use :reload aux match env
        [clojure.contrib command-line seq-utils test-is])
  (:require :reload action program
            env.download env.upload
            datacod.account datacod.action 
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:import (java.io File)
           (java.util Date)
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)))

(in-ns 'leica)

(def *usage* 
     "Клиент для датакода.

Пишите о багах на zahardzhan@gmail.com.

Скачать файлы с датакода:
leica [ключи] [файл с адресами на скачивание] [директория для скачанных файлов]

Закачать файлы на датакод:
leica [ключи] -a почтовый@адрес:пароль [файлы и директории для закачивания]
")

(defn user-agent [] ;; TODO: Сделать юзер-агента в соответствии со стандартом
  (str "Leica by Zahardzhan & GO1d ("
       (System/getProperty "os.name") " "
       (System/getProperty "os.version") " "
       (System/getProperty "os.arch") ")"))

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     *download-rules*
     [[#"http://dsv.data.cod.ru/\d{6}"
       {:get-link   datacod.action/get-link-and-name
        :get-tag    (partial action/get-tag [#"files3?.dsv.data.cod.ru"
                                         #"files2.dsv.data.cod.ru"])
        :get-file   action/get-file
        :get-length action/get-length
        :move-to-done-path action/move-to-done-path
        :download   action/download
        :die        action/die
        :pass       action/pass}]
      [#"http://[\w\.]*data.cod.ru/\d+"
       {:get-link   datacod.action/get-link-and-name
        :get-tag    (partial action/get-tag nil)
        :get-file   action/get-file
        :get-length action/get-length
        :move-to-done-path action/move-to-done-path
        :download   action/download
        :die        action/die
        :pass       action/pass}]
      [#"http://77.35.112.8[1234]/.+"
       {:get-link   action/get-link
        :get-name   action/get-name
        :get-tag    (partial action/get-tag nil)
        :get-file   action/get-file
        :get-length action/get-length
        :move-to-done-path action/move-to-done-path
        :download   action/download
        :die        action/die
        :pass       action/pass}]
      [#"http://dsvload.net/ftpupload/.+"
       {:get-link   action/get-link
        :get-name   action/get-name
        :get-tag    (partial action/get-tag nil)
        :get-file   action/get-file
        :get-length action/get-length
        :move-to-done-path action/move-to-done-path
        :download   action/download
        :die        action/die
        :pass       action/pass}]])

(defn verified-path [#^String path]
  (when (string? path)
    (let [#^File the-path (File. (.getCanonicalPath (File. path)))]
      (when (.exists the-path) the-path))))

(defn verified-jobs-file [#^String path]
  (when-let [#^File file (verified-path path)]
    (when (and (.isFile file) (.canRead file))
      file)))

(defn verified-output-dir [#^String path]
  (when-let [#^File dir (verified-path path)]
    (when (and (.isDirectory dir) (.canWrite dir))
      dir)))

(defn verified-upload-file [#^String path]
  (when-let [#^File file (verified-path path)]
    (when (and (.isFile file) (.canRead file)
               (> (file-length file) 0))
      file)))

(defn verified-upload-dir [#^String path]
  (when-let [#^File dir (verified-path path)]
    (when (and (.isDirectory dir) (.canRead dir))
      dir)))

(defn files-for-upload [paths]
  (loop [up '()
         ps (flatten (map #(or (verified-upload-file %)
                               (sort (seq (.listFiles (verified-upload-dir %)))))
                          paths))]
    (if-not (seq ps) up
            (let [p (first ps)]
              (recur (if (includes? up p) up (push up p))
                     (rest ps))))))

(defn account-attribs [line]
  (let [[_ domain login password]
        (re-find #"([^:]+):([^@]+@[^:]+):(.+)" line)]
    (when (and login password)
      [domain login password])))

(defn -main [& args]
  (with-command-line args
      *usage*
      [[account a "домен:имя@аккаунта:пароль для закачивания на датакод"]
       [move-done-to m "директория, в которую перемещать полностью загруженные файлы"]
       [quiet? q? "работать молча"]
       [debug? d? "писать подробные сообщения для отлова багов"]
       remaining-args]

    (let [root-logger (Logger/getLogger "")
          console-handler (first (.getHandlers root-logger))
          date-formatter (java.text.SimpleDateFormat. "HH:mm:ss")
          log-formatter (proxy [Formatter] []
                          (format 
                           [#^LogRecord record]
                           (str (.format date-formatter (Date. (.getMillis record))) " "
                                (.getMessage record) "\n")))
          log-level (cond quiet? (Level/OFF)
                          debug? (Level/FINE)
                          :else  (Level/INFO))]
      (.setFormatter console-handler log-formatter)
      (.setLevel console-handler log-level)
      (.setLevel root-logger log-level))

    (cond account
          (let [[domain login pass] (account-attribs account)
                acc (datacod.account/datacod-account domain login pass)
                files (files-for-upload remaining-args)]
            (when (and acc files)
              (let [e (env.upload/upload-environment acc {:termination #(System/exit 0)})]
                (add-agents e (env.upload/upload-agents files))
                (await e)
                (run-env e))))
          :else
          (let [jobs-file (some verified-jobs-file remaining-args)
                working-path (or (some verified-output-dir remaining-args)
                                 (verified-output-dir (System/getProperty "user.dir")))
                done-path (verified-output-dir move-done-to)]
            (when (and jobs-file working-path)
              (let [lines (duck/read-lines jobs-file)
                    e (env.download/download-environment
                       {:working-path working-path 
                        :done-path (when (not= working-path done-path) done-path)
                        :termination #(System/exit 0)})]
                (add-agents e (env.download/download-agents lines *download-rules*))
                (await e)
                (run-env e)))))))