;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

(ns #^{:doc "Агент и окружение для закачивания."
       :author "Роман Захаров"}
  env.upload
  (:use env aux match)
  (:require :reload action datacod.account)
  (:import (java.io File)
           (org.apache.commons.httpclient HttpClient)))

(def *slogan* "uploaded with secret alien technology")

(derive ::upload-agent :env/default-agent)
(derive ::upload-env   :env/default-env)

;;;; Агент

(defn upload-agent 
  "Агент для закачивания.

  :file     файл для закачки на сервер
  :address  ссылка на файл на сервере
  :length   размер файла, что нужно закачать
  :password пароль на файл
  :description описание файла"
  
  [#^File file rules]
  (when (and (.isFile file) (.canRead file) (> (file-length file) 0))
    (agent {:type ::upload-agent
            :env empty-fn
            :name (.getName file) :file file :length (file-length file)
            :address nil
            :password nil :description *slogan*
            :actions (:actions rules)
            :program (:program rules)
            :alive true :fail false :percept nil :action :create})))

(defmethod same? ::upload-agent [ag1 ag2]
  (or (= ag1 ag2)
      (= (:file @ag1) (:file @ag2))))

(defn upload-agents [files]
  (remove (comp not agent?) (map upload-agent files)))

(defmethod run-agent [::upload-agent :state] [ag-state]
  (let [env (related-env ag-state)]
    (cond (dead? ag-state) ag-state
 
          :else (let [new-state (action/percept-and-execute ag-state {:self ag-state})]
                  (cond (dead? new-state) (done env *agent*)
                        (fail? new-state) (done env *agent*)
                        :else (when-not (debug? *agent*) (run-agent *agent*)))
                  new-state))))

;;;; Окружение

(defn upload-environment [account & [{:keys [report-file
                                             debug
                                             termination]
                                      :or   {report-file nil
                                             debug false
                                             termination empty-fn}}]]
  (agent {:type ::upload-env :agents '() :account account
          :report-file report-file
          :debug debug
          :termination termination}))

(defmethod run-env [::upload-env :state] [env-state]
  (when-let [alive-ag (some #(when (alive? %) %) (:agents env-state))]
    (run-agent alive-ag))
  env-state)

(defmethod done [::upload-env :state] [env-state ag]
  (let [alive-unfailed
        (some #(when (and (alive? %) (not (fail? %))) %)
              (:agents env-state))
        next-alive
        (next-after-when #(and (alive? %)) ag (:agents env-state))]
    
    (cond alive-unfailed
          (run-agent alive-unfailed)

          next-alive
          (run-agent next-alive)

          (termination? env-state) ((env-state :termination) env-state)))
  env-state)

(defmulti out-of-space-on-account? type-agent-dispatch)

(defmethod out-of-space-on-account? [::upload-agent :agent] [ag]
  (out-of-space-on-account? (deref ag)))

(defmethod out-of-space-on-account? [::upload-agent :state] [ag]
  (let [client (new HttpClient)
        account ((deref (related-env ag)) :account)]
    (datacod.account/with-auth client account
      (let [free-space (datacod.account/free-space client account)]
        (< free-space (ag :length))))))