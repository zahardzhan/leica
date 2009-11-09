;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

(ns #^{:doc "doc"
       :author "Роман Захаров"}
  env.upload
  (:require :reload action env program datacod.action datacod.program)
  (:use :reload env aux match)
  (:import [java.io File]))

;;;; Агент

(defn upload-agent 
  "Агент для закачивания.

  :file     файл для закачки на сервер
  :address  ссылка на файл на сервере
  :length   размер файла, что нужно закачать
  :password пароль на файл
  :description описание файла"
  
  [#^File file]
  (when (and (.isFile file) (.canRead file) (> (file-length file) 0))
    (agent {:type ::upload
            :name (.getName file) :file file :length (file-length file)
            :address nil
            :password "" :description "uploaded with secret alien technology"
            :actions {:upload datacod.action/upload
                      :die    action/die}
            :program datacod.program/reflex-upload
            :alive true :fail false :percept nil :action nil})))

(defn upload-agents [files]
  (remove (comp not agent?) (map upload-agent files)))

(defmethod run-agent ::upload [ag env]
  (cond (dead? *agent*) ag

        :else (do (let [result (execute-action *agent* env)]
                    (cond (not (:alive result)) (send env done *agent*)
                          (:fail result) (send env done *agent*))
                    result))))

;;;; Окружение

(defn upload-environment [account & [{:keys [termination]
                                      :or   {termination #()}}]]
  (agent {:type ::upload :agents '() :account account
          :termination termination}))

(defmethod run-env ::upload [env]
  (when-let [alive-ag (some #(when (alive? %) %) (:agents env))]
    (send-off alive-ag run-agent *agent*))
  env)

(defmethod done ::upload [env ag]
  (let [alive-unfailed
        (some #(when (and (alive? %) (not (fail? %))) %)
              (agents *agent*))
        next-alive
        (next-after-when #(and (alive? %)) ag (agents *agent*))]
    
    (cond alive-unfailed
          (send-off alive-unfailed run-agent *agent*)

          next-alive
          (send-off next-alive run-agent *agent*)

          (termination? *agent*) ((env :termination))))
  env)