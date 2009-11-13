;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov zahardzhan@gmail.com

(ns #^{:doc "doc"
       :author "Роман Захаров"}
  env.upload
  (:use env aux match)
  (:require :reload action program datacod.action datacod.program)
  (:import [java.io File]))

(def *slogan* "uploaded with secret alien technology")

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
    (agent {:type :upload
            :name (.getName file) :file file :length (file-length file)
            :address nil
            :password nil :description *slogan*
            :actions {:upload datacod.action/upload
                      :die    action/die}
            :program datacod.program/reflex-upload
            :alive true :fail false :percept nil :action nil})))

(defn upload-agents [files]
  (remove (comp not agent?) (map upload-agent files)))

(defmethod run-agent- :upload [ag-state env]
  (cond (dead?- ag-state) ag-state

        :else (let [new-state (execute-action ag-state @env)]
                (cond (dead?- new-state) (done env *agent*)
                      (fail?- new-state) (done env *agent*))
                new-state)))

;;;; Окружение

(defn upload-environment [account & [{:keys [termination]
                                      :or   {termination #()}}]]
  (agent {:type :upload :agents '() :account account
          :termination termination}))

(defmethod run-env- :upload [env-state]
  (when-let [alive-ag (some #(when (alive? %) %) (:agents env-state))]
    (run-agent alive-ag *agent*))
  env-state)

(defmethod done- :upload [env-state ag]
  (let [alive-unfailed
        (some #(when (and (alive? %) (not (fail? %))) %)
              (:agents env-state))
        next-alive
        (next-after-when #(and (alive? %)) ag (:agents env-state))]
    
    (cond alive-unfailed
          (run-agent alive-unfailed *agent*)

          next-alive
          (run-agent next-alive *agent*)

          (termination?- env-state) ((env-state :termination) env-state)))
  env-state)