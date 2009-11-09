;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

;;; leica.clj: многопоточная качалка для data.cod.ru и dsvload.net.

;;; октябрь, 2009

;;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров, Александр Золотов"}
  leica
  (:gen-class)
  (:require :reload env env.download env.upload action datacod.action
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use aux match
        [clojure.contrib command-line seq-utils test-is])
  (:import (java.io File)
           (java.util Date)
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)))

(def *usage* 
     "Клиент для датакода.

Пишите о багах на zahardzhan@gmail.com.

Скачать файлы с датакода:
leica [Ключи] [Файл с адресами на скачивание] [Директория для скачанных файлов]

Закачать файлы на датакод:
leica [Ключи] -a почтовый@адрес:пароль [Файлы и директории для закачивания]
")

(defn user-agent [] ;; TODO: Сделать юзер-агента в соответствии со стандартом
  (str "Leica by Zahardzhan & GO1d ("
       (System/getProperty "os.name") " "
       (System/getProperty "os.version") " "
       (System/getProperty "os.arch") ")"))

(defn datacod-account
  "Аккаунт на датакоде."
  [login password]
  (when (and login password)
    {:login login :password password}))

(def #^{:doc "Таблица действий агентов для скачивания для конкретных адресов.
  Хосты упорядочены от частного к общему."}
     *download-rules*
     [[#"http://dsv.data.cod.ru/\d{6}"
       {:get-link   datacod.action/get-link-name
        :get-tag    (partial action/get-tag [#"files3?.dsv.data.cod.ru"
                                         #"files2.dsv.data.cod.ru"])
        :get-file   action/get-file
        :get-length action/get-length
        :download   action/download
        :die        action/die}]
      [#"http://[\w\.]*data.cod.ru/\d+"
       {:get-link   datacod.action/get-link-name
        :get-tag    (partial action/get-tag nil)
        :get-file   action/get-file
        :get-length action/get-length
        :download   action/download
        :die        action/die}]
      [#"http://77.35.112.8[1234]/.+"
       {:get-link   action/get-link
        :get-name   action/get-name
        :get-tag    (partial action/get-tag nil)
        :get-file   action/get-file
        :get-length action/get-length
        :download   action/download
        :die        action/die}]
      [#"http://dsvload.net/ftpupload/.+"
       {:get-link   action/get-link
        :get-name   action/get-name
        :get-tag    (partial action/get-tag nil)
        :get-file   action/get-file
        :get-length action/get-length
        :download   action/download
        :die        action/die}]])

;;;; COMMAND-LINE

(defn valid-path [#^String path]
  (let [#^File the-path (File. (.getCanonicalPath (File. path)))]
    (when (.exists the-path) the-path)))

(defn valid-jobs-file [#^String path]
  (when-let [#^File file (valid-path path)]
    (when (and (.isFile file) (.canRead file))
      file)))

(defn valid-output-dir [#^String path]
  (when-let [#^File dir (valid-path path)]
    (when (and (.isDirectory dir) (.canWrite dir))
      dir)))

(defn valid-upload-file [#^String path]
  (when-let [#^File file (valid-path path)]
    (when (and (.isFile file) (.canRead file)
               (> (file-length file) 0))
      file)))

(defn valid-upload-dir [#^String path]
  (when-let [#^File dir (valid-path path)]
    (when (and (.isDirectory dir) (.canRead dir))
      dir)))

(defn files-for-upload [paths]
  (loop [up '()
         ps (flatten (map #(or (valid-upload-file %)
                               (sort (seq (.listFiles (valid-upload-dir %)))))
                          paths))]
    (if-not (seq ps) up
            (let [p (first ps)]
              (recur (if (includes? up p) up (push up p))
                     (rest ps))))))

(defn login-and-password [line]
  (let [[_ login password]
        (re-find #"([^@]+@[^:]+):(.+)" line)]
    (when (and login password)
      [login password])))

(defn -main [& args]
  (with-command-line args
      *usage*
      [[account a "почтовый@адрес:пароль аккаунта на датакоде для закачивания"]
       [quiet? q? "работать молча"]
       [debug? d? "писать подробные сообщения для отлова багов"]
       remaining-args]

    (let [root-logger (Logger/getLogger "")
          console-handler (first (.getHandlers root-logger))
          basic-formatter (proxy [Formatter] []
                            (format
                             [#^LogRecord record]
                             (let [time (new Date (.getMillis record))
                                   hour (.getHours time)
                                   min  (.getMinutes time)
                                   sec  (.getSeconds time)]
                               (str hour ":" min ":" sec " " 
                                    (.getMessage record) "\n"))))
          log-level (cond quiet? (Level/OFF)
                          debug? (Level/FINE)
                          :else  (Level/INFO))]
      (.setFormatter console-handler basic-formatter)
      (.setLevel console-handler log-level)
      (.setLevel root-logger log-level))

    (cond account
          (let [[login pass] (login-and-password account)
                acc (datacod-account login pass)
                files (files-for-upload remaining-args)]
            (when (and acc files)
              (let [e (env.upload/upload-environment acc {:termination #(System/exit 0)})]
                (send e env/add-agents (env.upload/upload-agents files))
                (await e)
                (send e env/run-env))))
          :else
          (let [jobs-file (some valid-jobs-file remaining-args)
                working-path (or (some valid-output-dir remaining-args)
                                 (valid-output-dir (System/getProperty "user.dir")))]
            (when (and jobs-file working-path)
              (let [lines (duck/read-lines jobs-file)
                    e (env.download/download-environment {:working-path working-path 
                                                          :termination #(System/exit 0)})]
                (send e env/add-agents (env.download/download-agents lines *download-rules*))
                (await e)
                (send e env/run-env)))))))

;;;; TESTS

;; (let [jj {:link (URI. "http://files3.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
;;           :name "Hayate_the_combat_butler.mkv"
;;           :address (URI. "http://dsv.data.cod.ru/433148")}
;;       jk {:link (URI. "http://files4.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
;;           :name "Hayate_the_combat_butler.mkv"
;;           :address (URI. "http://dsv.data.cod.ru/433148")}
;;       j8 {:link (URI. "http://77.35.112.82/upload/Personal_Folders/Peshehod/Chelovek-Slon.mpg")
;;           :name "Chelovek-Slon.mpg"
;;           :address (URI. "http://77.35.112.82/upload/Personal_Folders/Peshehod/Chelovek-Slon.mpg")}]
;;   (deftest test-tag
;;     (is (= (:tag (leica/job-tag nil jj nil))
;;            "files3.dsv.data.cod.ru"))
;;     (is (= (:tag (leica/job-tag #"files3?.dsv.data.cod.ru" jj nil))
;;            "files3?.dsv.data.cod.ru"))
;;     (is (= (:tag (leica/job-tag [#"files3?.dsv.data.cod.ru"
;;                                  #"files2.dsv.data.cod.ru"] jj nil))
;;            "files3?.dsv.data.cod.ru"))
;;     (is (= (:tag (job-tag [#"files3?.dsv.data.cod.ru"
;;                            #"files2.dsv.data.cod.ru"] jk nil))
;;            "files4.dsv.data.cod.ru"))
;;     (is (= (:tag (job-tag nil j8 nil))
;;            "77.35.112.82"))))

;; (deftest test-match
;;   (is (= (match "http://dsv.data.cod.ru/433148"
;;                 '((#"http://dsv.data.cod.ru/\d{6}" :MATCH))
;;                 {:rule-response rest})
;;          '(:MATCH))))

;; (deftest test-login-and-password
;;   (is (= ["zahardzhan@gmail.com" "zxcvbn"]
;;          (login-and-password "zahardzhan@gmail.com:zxcvbn")))
;;   (is (= ["mail@gmail.com" "password"]
;;          (login-and-password "mail@gmail.com:password"))))

;; (deftest test-run
;;   (is (nil?
;;        (and nil
;;             (do
;;               (def e (environment {:working-path (File. "/home/haru/inbox/dsv")}))
;;               (send e add-agents (download-agents
;;                                   ["http://dsv.data.cod.ru/458692"
;;                                    "http://77.35.112.82/upload/Personal_Folders/Peshehod/Chelovek-Slon.mpg"]))
;;               (await e)
;;               (send e run-environment))))))

;; (deftest test-run-step
;;   (is (nil?
;;        (and nil
;;             (do
;;               (def e (environment {:working-path (File. "/home/haru/inbox/dsv")}))
;;               (def a (download-agent "Goal.Icon.rar http://dsv.data.cod.ru/456136"))
;;               (def b (download-agent ""))
;;               (send e add-agent a)
;;               (send e add-agent b)
;;               (await e)
;;               (send-off a act e)
;;               (send-off b act e)
;;               )))))

;; (deftest test-upload
;;   (is (nil?
;;        (and nil
;;             (do
;;               (def e (upload-environment
;;                       (datacod-account "zahardzhan@gmail.com" "zscxadw")))
;;               (def b (upload-agent (File. "/home/haru/inbox/issue27-ru.pdf")))
;;               (def d (upload-agent (File. "/home/haru/inbox/sicp.pdf")))
;;               (def f (upload-agent (File. "/home/haru/inbox/pcl.pdf")))
;;               (send e add-agent b)
;;               (send e add-agent d)
;;               (send e add-agent f)

;;               ;;(send-off b act e)
;;               (send e run-environment)

;;               (termination? e)
;;               )))))