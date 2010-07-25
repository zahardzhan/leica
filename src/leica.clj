;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns leica
  (:gen-class)
  (:use
   [clojure.set :only [difference union]]
   clojure.test
   clojure.contrib.def
   clojure.contrib.command-line)
  
  (:require
   [clojure.contrib.http.agent :as http]
   [clojure.contrib.logging :as log]
   [clojure.contrib.duck-streams :as duck]
   [async.http.client :as HTTP]
   fn)
  
  (:import
   java.util.Date
   java.net.URLDecoder
   (java.io File
            FileOutputStream
            InputStream)))

(import '(java.util.logging Logger
                            Level
                            Formatter
                            LogRecord
                            StreamHandler))

(import 'java.net.ConnectException)

(import '(org.apache.commons.httpclient URI
                                        HttpClient
                                        HttpStatus
                                        ConnectTimeoutException 
                                        NoHttpResponseException))

(import '(org.apache.commons.httpclient.methods GetMethod HeadMethod))
(import '(org.apache.commons.httpclient.params HttpMethodParams))
(import '(org.apache.commons.httpclient.util EncodingUtil))

(defmethod print-method clojure.lang.Delay
  [x w] ;; Don't print/eval delays
  ((get-method print-method Object) x w))

(defn set-root-logger-log-level [log-level]
  (let [root-logger (Logger/getLogger "")
        console-handler (first (.getHandlers root-logger))
        date-formatter (java.text.SimpleDateFormat. "HH:mm:ss")
        log-formatter
        (proxy [Formatter] []
          (format [#^LogRecord record]
                  (str \return
                       (.format date-formatter (Date. (.getMillis record)))
                       \space
                       (.getMessage record)
                       \newline)))]
    (.setFormatter console-handler log-formatter)
    (.setLevel console-handler log-level)
    (.setLevel root-logger log-level)))

(set-root-logger-log-level (Level/FINE)) ;; Log to console DEBUG messages

(defn same [f & xs]
  (apply = (map f xs)))

(defmacro with-return
  [expr & body]
  `(do (do ~@body)
       ~expr))

(defmacro let-return 
  [[form val] & body]
  `(let [~form ~val]
     (with-return ~form (do ~@body))))

(defn derefed
  ([] nil)
  ([x] (if (instance? clojure.lang.IDeref x) (deref x) x))
  ([x f] (f (derefed x)))
  ([x f & fs] ((apply comp (reverse fs)) (derefed x f))))

(defmacro with-deref 
  [[ref & refs] & body]
  (if ref
    `(let [~ref (derefed ~ref)]
       (with-deref ~refs ~@body))
    `(do ~@body)))

(defmacro when-supplied [& clauses]
  (if (not clauses) true
      `(and (or (nil? ~(first clauses))
                (do ~(second clauses)))
            (when-supplied ~@(next (next clauses))))))

(defalias supplied and)

(def nothing nil)

(defn as-file
  [arg & {:as args :keys [exists create readable writeable directory]}]
  (let [argtype (type arg)]
    (cond (= argtype File)
          (let [maybe-create
                (fn [f] (when f (cond (and (= create true) (not (.exists f)))
                                     (let [dir (File. (.getParent f))]
                                       (if-not (.exists dir)
                                         (throw (new Exception
                                                     "Cannot create file in nonexistant directory."))
                                         (if-not (.canWrite dir)
                                           (throw (new Exception
                                                       "Cannot create file in nonwriteable directory."))
                                           (do (.createNewFile f) f))))
                                     :else f)))
                maybe-exists
                (fn [f] (when f (cond (= exists true)  (when (.exists f) f)
                                     (= exists false) (when-not (.exists f) f)
                                     (not exists)     f)))
                maybe-directory
                (fn [f] (when f (cond (= directory true)  (when (.isDirectory f) f)
                                     (= directory false) (when-not (.isDirectory f) f)
                                     (not directory)  f)))
                maybe-readable
                (fn [f] (when f (cond (= readable true)  (when (.canRead f) f)
                                     (= readable false) (when-not (.canRead f) f)
                                     (not readable)     f)))
                maybe-writeable
                (fn [f] (when f (cond (= writeable true)  (when (.canWrite f) f)
                                     (= writeable false) (when-not (.canWrite f) f)
                                     (not writeable)     f)))]
            
            (-> arg maybe-create maybe-exists maybe-directory maybe-readable maybe-writeable))

          (= argtype String) (if args
                               (apply as-file (new File arg) (flatten (seq args)))
                               (as-file (new File arg))))))

(defn list-directory-files [dir] 
  (seq (.listFiles (as-file dir :readable true :directory true))))

(defn select [collection & {:keys [where order-by before after entirely-after]}]
  (let [take-after  (fn [item] (rest (drop-while (partial not= item) collection)))
        take-before (fn [item] (take-while (partial not= item) collection))
        take-entirely-after (fn [item] (concat (take-after item)
                                              (take-before item)
                                              (list item)))
        maybe-take-after  #(if after  (take-after  after) %)
        maybe-take-before #(if before (take-before before) %)
        maybe-take-entirely-after #(if entirely-after
                                     (take-entirely-after entirely-after) %)
        maybe-filter-where #(if where (filter where %) %)
        maybe-order-by #(if order-by (sort order-by %) %)]
    (-> (seq collection)
        maybe-take-entirely-after
        maybe-take-after
        maybe-take-before
        maybe-filter-where
        maybe-order-by)))

(defn make-environment []
  {:agents-ref (ref #{})})

(defn agents "The agents belonging to this environment." [e]
  (when e (deref (:agents-ref e))))

(declare bind unbind)

(defn make-agent [{:as opts :keys [environment]}]
  (let [a (agent (merge (dissoc opts :environment)
                        {:env-ref (delay (ref environment))}))]
    (with-return a
      (when environment (bind a environment)))))

(defn env "The environment belonging to this agent." [a]
  (when a (derefed a :env-ref force deref)))

(defn surrounding "The agents belonging to environment this agent belonging to."
  [a] (when-let [e (env a)] (difference (agents e) #{a})))

(defn binded? "Agent is binded to environment and vice-versa?"
  ([a]   (boolean (when-let [ags (agents (env a))] (ags a))))
  ([a e] (and (identical? e (env a)) (binded? a))))

(defn bind [a e]
  (with-return e
    (when-not (binded? a e)
      (when (binded? a) (unbind a))
      (dosync (alter (:agents-ref e) union #{a})
              (ref-set (derefed a :env-ref force) e)))))

(defn unbind [a]
  (with-return a
    (when (binded? a)
      (dosync (alter (:agents-ref (env a)) difference #{a})
              (ref-set (derefed a :env-ref force) nil)))))

(deftest agent-bind-test
  (let [e1 (make-environment)
        a1 (make-agent {:environment e1})
        a2 (make-agent {:environment e1})]
    (is (and (binded? a1 e1)
             (binded? a1)
             (binded? a2 e1)
             (binded? a2)))
    (unbind a1)
    (unbind a2)
    (is (not (or (binded? a1 e1)
                 (binded? a1)
                 (binded? a2 e1)
                 (binded? a2))))
    (bind a1 e1)
    (is (and (binded? a1 e1)
             (binded? a1)
             (not (binded? a2 e1))
             (not (binded? a2))))
    (bind a2 e1)
    (is (and (binded? a1 e1)
             (binded? a1)
             (binded? a2 e1)
             (binded? a2)))))

(defvar- services* (atom {}))

(defmacro defservice [service & opts]
  `(let [service-keyword# (keyword (str *ns*) (str (quote ~service)))]
     (swap! services* assoc service-keyword#
            (merge (hash-map ~@opts)
                   {:service service-keyword#}))))

(defn extract-url [line]
  (first (re-find #"((https?|ftp|gopher|telnet|file|notes|ms-help):((//)|(\\\\))+[\w\d:#@%/;$()~_?\+-=\\\.&]*)"
                  line)))

(defn match-service [line]
  (when-let [url (extract-url line)]
    (first (for [[service-name {:keys [address-pattern]}] @services*
                 :let [address (re-find address-pattern url)]
                 :when address]
             [service-name address]))))

(defn make-download-agent-body [line]
  (when-let [[service-name address] (match-service line)]
    (let [{:as service :keys [body]} (service-name @services*)]
      (dissoc (merge service
                     (when body (body))
                     {:address address})
              :body))))

(deftest make-download-agent-body-test
  (is (= "http://files.dsv.data.cod.ru/asdf"
         (:address (make-download-agent-body
                    "asdasd http://files.dsv.data.cod.ru/asdf ghjk")))))

(def *precedence* (atom 0))

(defn make-download-agent
  [line & {:as opts :keys [environment strategy precedence path name]}]
  {:pre [(when-supplied strategy     (instance? clojure.lang.IFn strategy)
                        precedence   (number? precedence)
                        environment  (map? environment)
                        path         (as-file path :directory true :writeable true)
                        name         (string? name))]}
  (when-let [body (make-download-agent-body line)]
    (make-agent (merge body
                       {:state-atom (atom :idle)
                        :fail-reason-atom (atom nil)
                        :precedence (or precedence (swap! *precedence* inc))}
                       (when environment {:environment environment})
                       (when strategy {:strategy strategy})
                       (when path {:path (as-file path)})
                       (when name {:name name})))))

(deftest make-download-agent-test
  (is (= "http://files.dsv.data.cod.ru/asdg"
         (:address @(make-download-agent "a http://files.dsv.data.cod.ru/asdg g")))))

(defn state [{:as a state-atom :state-atom}]
  (deref state-atom))

(defn state! [{:as a state-atom :state-atom} new-state]
  {:pre [(({:idle     #{:running}
            :failed   #{:running}
            :dead     #{}
            :running  #{:stopping :idle :failed :dead}
            :stopping #{:idle :failed :dead}}
           (state a)) new-state)]}
  (with-return a (reset! state-atom new-state)))

(defn state? [a in-state]
  (cond (keyword? in-state) (= in-state (state a))
        (set? in-state)     (keyword? (in-state (state a)))))

(defn fail-reason [{:as a fail-reason-atom :fail-reason-atom}]
  (deref fail-reason-atom))

(defn fail-reason! [{:as a fail-reason-atom :fail-reason-atom} reason]
  (with-return a (reset! fail-reason-atom reason)))

(defn dead?   [a] (state? a :dead))
(defn alive?  [a] (not (dead? a)))
(defn active? [a] (state? a #{:running :stopping}))
(defn idle?   [a] (state? a :idle))
(defn fail?   [a] (state? a :failed))

(defn pass    [a] a)

(defn idle [a]
  (with-return a
    (state! a :idle)
    (fail-reason! a nil)))

(defn fail [a & {:as opts :keys [reason]}]
  (with-return a
    (state! a :failed)
    (when reason (fail-reason! a reason))))

(defn die     [a] (state! a :dead))

(defn sleep   [a millis]
  (with-return a (Thread/sleep millis)))

(defn get-action [{:as a strategy :strategy}]
  (strategy a))

(defn execute [a action]
  (if (dead? a) a
      (try (state! a :running)
           (try (let [a1 (action a)]
                  (cond (active? a1) (idle a1)
                        :else        a1))
                (catch AssertionError err
                  (fail a :reason err))
                (catch Exception err
                  (fail a :reason err)))
           (catch AssertionError err
             (fail a :reason err)))))

(defn run [a]
  (execute a (get-action a)))

(defn file-size [{:as a file-size-atom :file-size-atom}]
  (deref file-size-atom))

(defn actual-file-length [file]
  (if (.exists file) (.length file) 0))

(defn out-of-space-on-path? [{:as a :keys [path file total-size]}]
  {:pre [(supplied path file total-size)]}
  (if (.exists file)
    (< (.getUsableSpace path) (- total-size (.length file)))
    (= (.getUsableSpace path) 0)))

(defn fully-loaded? [{:as a :keys [file total-size]}]
  {:pre [(supplied file total-size)]}
  (boolean (and (.exists file) (<= total-size (.length file)))))

(defn get-file [{:as a :keys [name path]}]
  {:pre [(supplied name path)]}
  (idle (assoc a :file (new File path name))))

(defvar- timeout-after-fail*        3000)
(defvar- connection-timeout*       15000)
(defvar- get-request-timeout*      30000)
(defvar- head-request-timeout*     10000)
(defvar- buffer-size*              65536)

(defn download
  [{:as a :keys [name address file total-size file-size-atom]}]
  {:pre [(supplied name address file total-size file-size-atom)]}
  (let [client (new HttpClient)
        get    (new GetMethod address)]
    (when (and address file)
      (try
        (.. client getHttpConnectionManager getParams 
            (setConnectionTimeout connection-timeout*))
        (.. get getParams (setSoTimeout get-request-timeout*))
        (.setRequestHeader get "Range" (str "bytes=" (actual-file-length file) \-))
        (.executeMethod client get)
        (let [content-length (.getResponseContentLength get)]
          (cond (not content-length)
                (fail a :reason "Cannot check file before download.")

                (not= content-length (- total-size (actual-file-length file)))
                (fail a :reason "Downloading file size mismatch.")

                :else
                (with-open [#^InputStream input (.getResponseBodyAsStream get)
                            #^FileOutputStream output (FileOutputStream. file true)]
                  (log/info (str "Begin downloading " name))
                  (let [buffer (make-array Byte/TYPE buffer-size*)]
                    (loop [file-size (actual-file-length file)]
                      (let [read-size (.read input buffer)]
                        (when (pos? read-size)
                          (let [new-size (+ file-size read-size)]
                            (.write output buffer 0 read-size)
                            (reset! file-size-atom new-size)
                            (recur new-size)))))
                    (.flush output))
                  (log/info (str "End downloading " name))
                  (idle a))))
        (finally (do (.releaseConnection get)))))))

(defn files-dsv-*-data-cod-ru-get-head [{:as a :keys [address name]}]
  {:pre [(supplied address)]}
  (let [client (new HttpClient)
        head (new HeadMethod address)]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout*))
    (.. head getParams (setSoTimeout head-request-timeout*))
    (try (let [status (.executeMethod client head)]
           (if (= status HttpStatus/SC_OK)
             (let [content-length (Integer/parseInt
                                   (.. head (getResponseHeader "Content-Length") (getValue)))
                   content-disposition (.. head (getResponseHeader "Content-Disposition") (getValue))
                   filename (URLDecoder/decode (second (re-find #"; filename=\"(.*)\"" content-disposition)))]
               (if-not (and content-length filename) (die a)
                       (idle (merge a {:total-size content-length}
                                    (when (not name) {:name filename})))))
             (fail a :reason "HEAD request failed.")))
         (finally (.releaseConnection head)))))

(defn files-dsv-*-data-cod-ru-reflex-strategy
  [{:as a :keys [address name file path total-size]}]
  (cond (not address)                 die
        (not (and name total-size))   files-dsv-*-data-cod-ru-get-head
        (not file)                    get-file
        (or (out-of-space-on-path? a) (fully-loaded? a)) die
        :else                         download))

(defn data-cod-ru-parse-page [{:as a :keys [address]}]
  {:pre [(supplied address)]}
  (let [client (new HttpClient)
        get (new GetMethod address)]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout*))
    (.. get getParams (setSoTimeout get-request-timeout*))
    (try (let [status (.executeMethod client get)]
           (if (= status HttpStatus/SC_OK)
             (let [link (re-find #"http://files[-\d\w\.]*data.cod.ru/[^\"]+"
                                 (duck/slurp* (.getResponseBodyAsStream get)))]
               (if link (idle (assoc a :link link))
                   (die a)))
             (fail a)))
         (catch Exception e (fail a))
         (finally (.releaseConnection get)))))

(defn data-cod-ru-make-child-agent [{:as a :keys [link path precedence]}]
  {:pre [(supplied link path precedence (env a))]}
  (let [child (make-download-agent link :environment (env a)
                                   :precedence precedence
                                   :path path)]
    (if-not child (die a)
            (do (send-off child run)
                (idle (assoc a :child child))))))

(defn data-cod-ru-reflex-strategy [{:as a :keys [address link child]}]
  (cond (not (and address (env a))) die
        (not link)     data-cod-ru-parse-page
        (not child)    data-cod-ru-make-child-agent
        :else          die))

(defn download-environment-is-done? [e]
  (every? #(derefed % dead?) (agents e)))

(defn terminate-download-environment [e] nil)

(defn try-to-terminate-environment [e]
  (when (download-environment-is-done? e)
    (terminate-download-environment e)))

(defn data-cod-ru-schedule-strategy [strategy]
  (fn scheduled-strategy [{:as a-stgy}]
    (cond (not *agent*) (strategy a-stgy)

          :else
          (fn scheduled-action [a]
            (let-return
             [new-a ((strategy a) a)]
             (cond (dead? new-a) (do nothing)
                   (fail? new-a) (do (send-off *agent* sleep timeout-after-fail*)
                                     (send-off *agent* run))
                   :else         (do (send-off *agent* run))))))))

(defn idle-service-successors [a]
  (select (agents (env a)) :order-by #(derefed % :precedence)
          :where #(and (derefed % idle?) (same :service (deref a) (deref %)))))

(defn failed-service-successors-after [a]
  (select (agents (env a)) :entirely-after a :order-by #(derefed % :precedence)
          :where #(and (derefed % fail?) (same :service (deref a) (deref %)))))

(defn alive-service-successors-after [a]
  (select (agents (env a)) :entirely-after a :order-by #(derefed % :precedence)
          :where #(and (derefed % alive?) (same :service (deref a) (deref %)))))

(defn active-service-neighbours [a]
  (select (surrounding a) :where #(and (same :service (deref %) (deref a))
                                       (derefed % active?))))

(defn files-dsv-*-data-cod-ru-schedule-strategy [strategy]
  (fn scheduled-strategy [{:as a-stgy :keys [max-active-agents service]}]
    (cond (not *agent*) (strategy a-stgy)

          (> (count (active-service-neighbours *agent*)) max-active-agents) idle

          :else
          (fn scheduled-action [a]
            (let [new-a ((strategy a) a)
                  successor (or (first (idle-service-successors *agent*))
                                (first (failed-service-successors-after *agent*))
                                (first (alive-service-successors-after *agent*)))]
              (with-return new-a
                (cond (dead? new-a) (do (when successor (send-off successor run)))
                      (fail? new-a) (do (send-off *agent* sleep timeout-after-fail*)
                                        (when successor (send-off successor run)))
                      :else         (do (send-off *agent* run)))))))))

(defservice data-cod-ru
  :address-pattern #"http://[\w\-]*.data.cod.ru/\d+"
  :strategy (data-cod-ru-schedule-strategy data-cod-ru-reflex-strategy)
  :body #(hash-map :address nil :link nil :child nil))

(defservice files3?-dsv-*-data-cod-ru
  :address-pattern #"http://files3?.dsv.*.data.cod.ru/.+"
  :strategy (files-dsv-*-data-cod-ru-schedule-strategy files-dsv-*-data-cod-ru-reflex-strategy)
  ;; :strategy files-dsv-*-data-cod-ru-reflex-strategy
  :max-active-agents 1
  :body #(hash-map :address nil :name nil :file nil :path nil
                   :total-size nil :file-size-atom (atom nil)))

(defservice files2-dsv-*-data-cod-ru
  :address-pattern #"http://files2.dsv.*.data.cod.ru/.+"
  :strategy (files-dsv-*-data-cod-ru-schedule-strategy files-dsv-*-data-cod-ru-reflex-strategy)
  ;; :strategy files-dsv-*-data-cod-ru-reflex-strategy
  :max-active-agents 1
  :body #(hash-map :address nil :name nil :file nil :path nil
                   :total-size nil :file-size-atom (atom nil)))

(defn -main [& args]
  (with-command-line args
    "Use it!"
    [[quiet?  q? "Work quietly."]
     [debug?  d? "Print debug messages."]
     remaining-args]

    (set-root-logger-log-level (cond quiet? (Level/OFF)
                                     debug? (Level/FINE)
                                     :else  (Level/INFO)))

    (let [jobs-file    (some #(as-file % :directory false :readable true)
                             remaining-args)
          working-path (or (some #(as-file % :directory true :writeable true)
                                 remaining-args)
                           (as-file (System/getProperty "user.dir") :writeable true))]
      (when (and jobs-file working-path)
        (let [lines (duck/read-lines jobs-file)
              download-environment (make-environment)]
          (doseq [line lines]
            (make-download-agent line :environment download-environment
                                 :path working-path))
          ;; (turn-on-cli-for-all-download-environments)
          ;; (add-hook download-environment-termination-hook :system-exit (fn [e] (System/exit 0)))
          (doseq [a (agents download-environment)]
            (send-off a run)))))))
