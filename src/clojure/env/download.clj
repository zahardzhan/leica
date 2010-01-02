;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Агент для скачивания."
       :author "Роман Захаров"}
  env.download
  (:use env aux match)
  (:require :reload [action :only 'percept-and-execute])
  (:import (java.io File)
           (org.apache.commons.httpclient URI)))

(in-ns 'env.download)

(derive ::download-agent :env/default-agent)

(def *timeout-after-fail* 3000)

(defn download-environment [& [{:keys [working-path done-path progress-agent 
                                       debug termination]
                                :or   {working-path nil
                                       done-path nil
                                       progress-agent nil
                                       debug false
                                       termination empty-fn}}]]
  (agent {:type ::download-env :agents '() :tags {}
          :working-path working-path
          :done-path done-path
          :progress-agent progress-agent
          :debug debug
          :termination termination}))

(defn download-agent 
  "Агент для скачивания."  
  [rules line]
  (let [[address {actions :actions program :program}]
        (match line rules {:action list})]
    (when (and address actions program)
      (send (default-agent {:type ::download-agent
                            :actions actions
                            :program program})
            assoc
            :address (URI. address) ; адрес задания
            :link nil ; прямая ссылка на файл, который нужно скачать
            :file nil ; файл в который сохраняется скачанное
            :length nil ; размер файла, что нужно скачать
            ))))

(defn address [ag] (-> ag self deref :address))

(def same-download-agents (fn-or (partial same identity)
                                 (partial same address)))

(defn download-agents [rules lines]
  (remove (comp not agent?) 
          (map (partial download-agent rules) lines)))

(defmethod run-agent [::download-agent :state] [ag]
  (let [tag (:tag ag)
        env (related-env ag)]
    (cond (dead? ag) ag

          (not tag) (let [new-state (action/percept-and-execute ag {:self ag})]
                      (cond (dead? new-state) (done env *agent*)
                            (fail? new-state) (do (sleep *agent* *timeout-after-fail*)
                                                  (when-not (debug? *agent*)
                                                    (run-agent *agent*)))
                            (:tag new-state) (do (add-tag env (:tag new-state))
                                                 (received-tag env *agent*))
                            :else (when-not (debug? *agent*) (run-agent *agent*)))
                      new-state)

          (tag-locked? env tag) ag

          :else (let [new-state (with-lock-env-tag env tag
                                  (action/percept-and-execute ag {:self ag}))]
                  (cond (dead? new-state) (done env *agent*)
                        (fail? new-state) (do (sleep *agent* *timeout-after-fail*)
                                              (done env *agent*))
                        :else (when-not (debug? *agent*) (run-agent *agent*)))
                  new-state))))

;;;; Окружение

(defmethod received-tag [::download-env :state] [env ag]
  (when-let [next-alive-untagged (next-after-when (fn-and alive? (complement tag))
                                                  ag (agents env))]
    (run-agent next-alive-untagged))
  (run-agent ag)
  env)

(defmethod done [::download-env :state] [env ag]
  (let [alive-unfailed-with-same-tag (some (fn-and alive? 
                                                   (complement fail?) 
                                                   (partial same-tag? ag)
                                                   identity)
                                           (agents env))

        next-alive-with-same-tag (next-after-when (fn-and alive?
                                                          (partial same-tag? ag))
                                                  ag (agents env))]
    
    (cond alive-unfailed-with-same-tag (run-agent alive-unfailed-with-same-tag)
          next-alive-with-same-tag (run-agent next-alive-with-same-tag)
          (termination? env) ((env :termination) env)))
  env)

(defmulti done-path                  type-agent-dispatch)
(defmulti working-path               type-agent-dispatch)
(defmulti out-of-space-on-work-path? type-agent-dispatch)
(defmulti out-of-space-on-done-path? type-agent-dispatch)
(defmulti fully-loaded?              type-agent-dispatch)
(defmulti already-on-done-path?      type-agent-dispatch)

(defmethod out-of-space-on-work-path? [::download-agent :agent] [ag]
  (out-of-space-on-work-path? (deref ag)))

(defmethod out-of-space-on-work-path? [::download-agent :state] [ag]
  (< (.getUsableSpace (working-path ag))
     (- (ag :length) (file-length (ag :file)))))

(defmethod out-of-space-on-done-path? [::download-agent :agent] [ag]
  (out-of-space-on-done-path? (deref ag)))

(defmethod out-of-space-on-done-path? [::download-agent :state] [ag]
  (< (.getUsableSpace (done-path ag)) (ag :length)))

(defmethod fully-loaded? [::download-agent :agent] [ag]
  (fully-loaded? (deref ag)))

(defmethod fully-loaded? [::download-agent :state] [ag]
  (<= (ag :length) (file-length (ag :file))))

(defmethod already-on-done-path? [::download-agent :agent] [ag]
  (already-on-done-path? (deref ag)))

(defmethod already-on-done-path? [::download-agent :state] [ag]
  (.exists (File. (done-path ag) (.getName (ag :file)))))

(defmethod working-path [::download-agent :agent] [ag] (:working-path (deref (related-env ag))))
(defmethod working-path [::download-agent :state] [ag] (:working-path (deref (related-env ag))))
(defmethod working-path [::download-env :agent]  [env] (:working-path (deref env)))
(defmethod working-path [::download-env :state]  [env] (:working-path env))

(defmethod done-path [::download-agent :agent] [ag] (:done-path (deref (related-env ag))))
(defmethod done-path [::download-agent :state] [ag] (:done-path (deref (related-env ag))))
(defmethod done-path [::download-env :agent]  [env] (:done-path (deref env)))
(defmethod done-path [::download-env :state]  [env] (:done-path env))