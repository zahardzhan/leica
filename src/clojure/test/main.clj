;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  test.main
  (:use :reload aux match env
        [clojure.contrib command-line seq-utils test-is])
  (:require :reload env.download env.upload action program
            datacod.account datacod.action datacod.program
            [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:import (java.io File FileOutputStream InputStream)
           (java.util Date)
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)
           (org.apache.commons.httpclient URI HttpClient HttpStatus)
           (org.apache.commons.httpclient.methods GetMethod HeadMethod)
           (org.apache.commons.httpclient.params.HttpMethodParams)
           (org.apache.commons.httpclient.util EncodingUtil)))

(in-ns 'test.main)

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

(deftest main-test
  (is 
   (nil?
    (def e (env.download/download-environment {:working-path (File. "/home/haru/inbox/dsv")}))
    (def a (env.download/download-agent "http://dsv.data.cod.ru/479745" *download-rules*))
    (add-agents e [a])
    (run-agent a e)

    (run-env e) e
    
    ((@a :program) {:self @a :env @e})
    (((@a :actions) :get-link) @a @e)
    (def a3 (execute-action (execute-action (execute-action @a @e) @e) @e))
    ((a3 :program) {:self a3 :env @e})
    (def a4 (execute-action a3 @e))
    ((a4 :program) {:self a4 :env @e})
    (def a5 (execute-action a4 @e))
    ((a5 :program) {:self a5 :env @e})
    (def a6 (execute-action a5 @e))
    ((a6 :program) {:self a6 :env @e})

    )))

;; (deftest upload-test
;;   (is (nil?
;;        (do
;;          (def a (env.upload/upload-agent (File. "/home/haru/inbox/dsv/.jobs")))
;;          (def e (env.upload/upload-environment
;;                  (datacod.account/datacod-account "dsv" "zahardzhan@gmail.com" "zscxadw")
;;                  {:report-file (leica/verified-log-file "/home/haru/share/src/leica/log")}))
;;          (add-agent e a)
;;          (await e))

;;        ;;(env/type-dispatch a)
       
;;        (run-env e) e
       
;;        (run-agent a e) e ((@a :program) {:self @a :env @e})
;;        (env/dead? a)

;;        (env/dead?- (env/execute-action @a @e))

       
;;        (run-env e) e
;;        (termination? e)
;;        (agent-errors e)
;;        (clear-agent-errors e)

;;        ((@a :program) {:self @a :env @e})
       
;;        (def a1 (execute-action @a @e)) a1
;;        ((a1 :program) {:self a1 :env @e})
;;        (def a2 (execute-action a1 @e)) a2
;;        ((a2 :program) {:self a2 :env @e})
;;        (def a3 (execute-action a2 @e)) a3
;;        ((a3 :program) {:self a3 :env @e})
;;        )))

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
