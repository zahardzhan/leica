;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

;;; октябрь, 2009

;;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:gen-class)
  (:use :reload aux match env rules
        [clojure.contrib command-line seq-utils test-is])
  (:require :reload env.download env.upload datacod.account progress
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

Закачать файлы на датакод в домене [dsv|amur|khv|avangard|...]:
leica [ключи] -a домен:почтовый@адрес:пароль [файлы и директории для закачивания]
")

(defn user-agent [] ;; TODO: Сделать юзер-агента в соответствии со стандартом
  (str "Leica by Zahardzhan & GO1d ("
       (System/getProperty "os.name") \space
       (System/getProperty "os.version") \space
       (System/getProperty "os.arch") \)))

(defn verified-path [path]
  (cond (string? path)
        (let [#^File the-path (File. (.getCanonicalPath (File. path)))]
          (when (.exists the-path) the-path))

        (file? path)
        (when (.exists path) path)))

(defn verified-jobs-file [path]
  (when-let [#^File file (verified-path path)]
    (when (and (.isFile file) (.canRead file))
      file)))

(defn verified-log-file [path]
  (when (string? path)
    (let [#^File file (File. (.getCanonicalPath (File. path)))
          #^File dir (File. (.getParent file))]
      (when (and (.exists dir) (.canWrite dir))
        (when-not (.exists file) (.createNewFile file))
        file))))

(defn verified-output-dir [path]
  (when-let [#^File dir (verified-path path)]
    (when (and (.isDirectory dir) (.canWrite dir))
      dir)))

(defn verified-upload-file [path]
  (when-let [#^File file (verified-path path)]
    (when (and (.isFile file) (.canRead file)
               (> (file-length file) 0))
      file)))

(defn verified-upload-dir [path]
  (when-let [#^File dir (verified-path path)]
    (when (and (.isDirectory dir) (.canRead dir))
      dir)))

(defn files-for-upload [upload-paths]
 ;; (filter verified-upload-file
 ;;         (distinct (flatten (map file-seq 
 ;;                                 (filter verified-upload-dir
 ;;                                         upload-paths))))))
  (loop [unique '()
         paths  (flatten
                 (filter identity
                         (map (fn [path]
                                (or (verified-upload-file path)
                                    (when-let [dir (verified-upload-dir path)]
                                      (filter verified-upload-file
                                              (sort (seq (.listFiles dir)))))))
                              upload-paths)))]
    (if-not (seq paths) unique
            (let [path (first paths)]
              (recur (if (includes? unique path) unique (push unique path))
                     (rest paths))))))

(defn account-attribs [line]
  (let [[_ domain login password]
        (re-find #"([^:]+):([^@]+@[^:]+):(.+)" line)]
    (when (and login password)
      [domain login password])))

(defn print-succesfully-uploaded [agents]
  (log/info
   (apply str "Загруженные файлы:" \newline
          (seq (map (fn [ag] (when-let [address (@ag :address)]
                               (format-link-for-forum (@ag :name) address)))
                    agents)))))

(defn -main [& args]
  (with-command-line args
      *usage*
      [[account a  "домен:имя@аккаунта:пароль для закачивания на датакод"]
       [move    m  "директория, в которую перемещать полностью скачанные файлы"]
       [report  r  "файл для отчета о закачанных"]
       [quiet?  q? "работать молча"]
       [debug?  d? "писать подробные сообщения для отлова багов"]
       remaining-args]

    (let [root-logger (Logger/getLogger "")
          console-handler (first (.getHandlers root-logger))
          date-formatter (java.text.SimpleDateFormat. "HH:mm:ss")
          log-formatter (proxy [Formatter] []
                          (format 
                           [#^LogRecord record]
                           (str \return
                                (.format date-formatter (Date. (.getMillis record)))
                                \space (.getMessage record) \newline)))
          log-level (cond quiet? (Level/OFF)
                          debug? (Level/FINE)
                          :else  (Level/INFO))]
      (.setFormatter console-handler log-formatter)
      (.setLevel console-handler log-level)
      (.setLevel root-logger log-level))
    
    (cond account
          (let [[domain login pass] (account-attribs account)
                acc (datacod.account/datacod-account domain login pass)
                files (files-for-upload remaining-args)
                report-file (verified-log-file report)]
            (when (and acc (seq files))
              (let [e (env.upload/upload-environment
                       acc {:report-file report-file
                            :termination (fn [env]
                                           (print-succesfully-uploaded (env :agents))
                                           (System/exit 0))})]
                (add-agents e (env.upload/upload-agents files *upload-rules*))
                (await e)
                (run-env e))))
          :else
          (let [jobs-file (some verified-jobs-file remaining-args)
                working-path (or (some verified-output-dir remaining-args)
                                 (verified-output-dir (System/getProperty "user.dir")))
                done-path (verified-output-dir move)]
            (when (and jobs-file working-path)
              (let [lines (duck/read-lines jobs-file)
                    e (env.download/download-environment
                       {:working-path working-path 
                        :done-path (when (not= working-path done-path) done-path)
                        :progress-agent (progress/console-progress-agent)
                        :termination (fn [env] (System/exit 0))})]
                (add-agents e (env.download/download-agents lines *download-rules*))
                (await e)
                (run-env e)))))))