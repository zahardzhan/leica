;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns log
  (:use
   clojure.contrib.def
   [clojure.set :only [difference union]]))

(import 'java.text.SimpleDateFormat)
(import 'java.util.Date)

(defvar *log-levels* #{:trace :debug :info :warn :error :fatal})

(defn log-level? [maybe-level]
  (boolean (*log-levels* maybe-level)))

(defn- make-logging-agent
  "The default agent used for performing logging durng a transaction."
  []
  (agent {:log-levels #{:info :warn :error :fatal}
          :format-message
          (fn [message]
            (str \return
                 (. (new SimpleDateFormat "HH:mm:ss") format (new Date))
                 \space message \newline))}))

(defn- setup-logging-agent [namespace ag]
  (alter-meta! namespace assoc :logging-agent ag))

(defn- logging-agent-set? [namespace]
  (boolean (:logging-agent (meta namespace))))

(defn- logging-agent [namespace]
  (:logging-agent (meta namespace)))

(defn- log-level-enabled? [namespace & levels]
  (every? (fn [lvl] (lvl (:log-levels @(logging-agent namespace))))
          levels))

(defn- enable-log-level! [namespace & levels]
  (let [logging-agent (logging-agent namespace)]
    (send logging-agent assoc :log-levels
          (union (@logging-agent :log-levels) (set levels)))))

(defn- disable-log-level! [namespace & levels]
  (let [logging-agent (logging-agent namespace)]
    (send logging-agent assoc :log-levels
          (difference (@logging-agent :log-levels) (set levels)))))

(defn- set-format-message! [namespace format-message]
  (send (logging-agent namespace) assoc :format-message format-message))

(defn log "Write log message."
  [level message & {:keys [namespace] :or {namespace *ns*}}]
  (when-not (logging-agent-set? namespace)
    (setup-logging-agent namespace (make-logging-agent)))
  (when (log-level-enabled? namespace level)
    (let [log-ag (logging-agent namespace)
          formatted-message ((@log-ag :format-message) message)
          write-log-message (fn [message] (. System/err println message))
          write-log-message-to-ag (fn [ag message] (write-log-message message) ag)]
      (if-not (clojure.lang.LockingTransaction/isRunning)
        (write-log-message formatted-message)
        (send-off log-ag write-log-message-to-ag formatted-message)))))

(defn trace [message] (log :trace message))
(defn debug [message] (log :debug message))
(defn info  [message] (log :info  message))
(defn warn  [message] (log :warn  message))
(defn error [message] (log :error message))
(defn fatal [message] (log :fatal message))

(defn set-log [& {:keys [enable-levels disable-levels format]}]
  (let [namespace *ns*]
    (when-not (logging-agent-set? namespace)
      (setup-logging-agent namespace (make-logging-agent)))
    (when (seq enable-levels)
      (apply enable-log-level! namespace enable-levels))
    (when (seq disable-levels)
      (apply disable-log-level! namespace disable-levels))
    (when (ifn? format)
      (set-format-message! namespace format))))

(comment
  (setup-logging-agent *ns* (make-logging-agent))
  (logging-agent *ns*)
  (agent-error (logging-agent *ns*))
  (enable-log-level! *ns* :trace :warn :debug)
  (disable-log-level!  *ns* :trace :warn :debug)
  (log-level-enabled? *ns* :trace)

  (set-log :disable-levels '(:trace :debug))

  (let [r (ref 1)]
    (dosync (ref-set r 2) (log :info "Info message1.")))
  (log :info "Info message 3.")
  )
