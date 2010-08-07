;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns leica
  (:gen-class)
  (:use
   [clojure.set :only [difference union]]
   clojure.contrib.command-line
   clojure.contrib.def
   clojure.test
   hooks)
  
  (:require
   [clojure.contrib.http.agent :as http]
   [clojure.contrib.duck-streams :as duck]
   fn
   log)
  
  (:import
   (java.util Date)

   (java.net URLDecoder
             ConnectException)   

   (java.io File
            FileOutputStream
            InputStream)

   (org.apache.commons.httpclient URI
                                  HttpClient
                                  HttpStatus
                                  ConnectTimeoutException 
                                  NoHttpResponseException
                                  methods.GetMethod
                                  methods.HeadMethod
                                  params.HttpMethodParams
                                  util.EncodingUtil)))

(defmethod print-method clojure.lang.Delay
  [x w] ((get-method print-method Object) x w)) ;; Don't print/eval delays

(defn same [f & xs]
  (apply = (map f xs)))

(defmacro with-return [expr & body]
  `(do (do ~@body)
       ~expr))

(defmacro with-let-ret [[form val] & body]
  `(let [~form ~val]
     (with-return ~form (do ~@body))))

(defmacro defhook [name qualifier target args & body]
  (let [key# (keyword name)]
    `(add-hook ~qualifier ~target ~key# (fn ~args ~@body))))

(defn derefed
  ([] nil)
  ([x] (if (instance? clojure.lang.IDeref x) (deref x) x))
  ([x f] (f (derefed x)))
  ([x f & fs] ((apply comp (reverse fs)) (derefed x f))))

(defalias dref derefed)

(defmacro with-deref [[ref & refs] & body]
  (if ref
    `(let [~ref (derefed ~ref)]
       (with-deref ~refs ~@body))
    `(do ~@body)))

(defmacro when-supplied [& clauses]
  (if (not clauses) true
      `(and (or (nil? ~(first clauses))
                (do ~(second clauses)))
            (when-supplied ~@(next (next clauses))))))

(defn agent? [x]
  (instance? clojure.lang.Agent x))

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
        maybe-order-by #(if order-by (sort-by order-by %) %)]
    (when (seq collection)
      (-> (seq collection)
          maybe-order-by
          maybe-take-entirely-after
          maybe-take-after
          maybe-take-before
          maybe-filter-where))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Download-Environment-Protocol
  (agents [e]))

(defrecord Download-Environment [agents-ref]
  Download-Environment-Protocol
  (agents [e] @agents-ref))

(declare operating?)

(defn operating [ags]
  (filter operating? ags))

(defn make-download-environment []
  (new Download-Environment (ref #{})))

(make-download-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Download-Agent-Protocol
  (ask     [a & opts])
  (body    [a] [a key] [a key value] [a key value & kvs])
  (state   [a] [a key] [a key value] [a key value & kvs])
  (precedence [a])
  (env     [a])
  (binded? [a] [a e])
  (surround [a])
  (run?    [a])
  (stop?   [a])
  (alive?  [a])
  (dead?   [a])
  (fail?   [a])
  (failure [a])
  (operating? [a])
  (program [a]))

(defrecord Download-Agent
  [agent-precedence ;; number
   agent-controller ;; (agent)
   state-delay      ;; (delay (ref {k v, ...}))
   body-ref         ;; (ref {k v, ...})
   ]

  Object
  (toString [this] (str "Download Agent: " @body-ref))

  Download-Agent-Protocol
  (ask     [a & opts])
  (body    [a] @body-ref)
  (body    [a key] (@body-ref key))
  (body    [a key value] (dosync (alter body-ref key value)))
  (body    [a key value & kvs]
           (dosync (apply (partial alter body-ref assoc key value) (seq kvs))))
  (state   [a] @state-delay)
  (state   [a key] (@@state-delay key))
  (state   [a key value] (dosync (alter @state-delay assoc key value)))
  (state   [a key value & kvs] ;; TODO: Test
           (dosync (apply (partial alter @state-delay assoc key value) (seq kvs))))
  (precedence [a] agent-precedence)
  (env     [a] (state a :env))
  (binded? [a] (boolean (when-let [ags (agents (env a))] (ags a)))) ;; TODO: Test
  (binded? [a e] (and (identical? e (env a)) (binded? a))) ;; TODO: Test
  (surround [a] (when-let [e (env a)] (difference (agents e) #{a})))
  (run?    [a] (state a :run))
  (stop?   [a] (state a :stop))
  (alive?  [a] (state a :alive))
  (dead?   [a] (not (alive? a)))
  (fail?   [a] (boolean (state a :fail)))
  (failure [a] (state a :fail))
  (operating? [a] (and (alive? a) (not (stop? a))))
  (program [a] (state a :program)))

(declare bind unbind)

(defvar make-download-agent
  (let [precedence-counter (atom 0)]
    (fn [& {:as opts :keys [environment precedence program]}]
      (with-let-ret
        [a (new Download-Agent
                (or precedence (swap! precedence-counter inc))
                (agent nil :error-mode :fail)
                (delay (ref {:alive   true
                             :run     false
                             :stop    false
                             :fail    nil
                             :env     nil
                             :program program
                             :pending-actions clojure.lang.PersistentQueue/EMPTY
                             :running-actions #{}}))
                (ref (dissoc opts :environment :precedence :program)))]
        (when-supplied environment (bind a environment))))))

(let [a (make-download-agent)]
  [(alive? a)
   (state a :alive false :run true)
   (alive? a) (run? a)])

(let [r (ref {:a 1 :b 2})]
  (dosync (apply (partial alter r assoc :a 2) (seq [:b 3]))))

((fn [x & xs] xs) 1 2 3 4)

(deftest make-download-agent-test
  (is (= "http://files.dsv.data.cod.ru/asdg"
         (:address @(make-download-agent
                     "a http://files.dsv.data.cod.ru/asdg g")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (first (re-find #"((https?|ftp|file):((//)|(\\\\))+[\w\d:#@%/;$()~_?\+-=\\\.&]*)"
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

(defn make-download-agent
  [line & {:as opts :keys [environment strategy precedence path name]}]
  {:pre [(when-supplied strategy     (instance? clojure.lang.IFn strategy)
                        precedence   (number? precedence)
                        environment  (map? environment)
                        path         (as-file path :directory true :writeable true)
                        name         (string? name))]}
  (when-let [body (make-download-agent-body line)]
    (make-agent (merge body
                       {:precedence (or precedence (swap! *precedence* inc))
                        :run-atom  (atom false)
                        :stop-atom (atom false)
                        :alive true
                        :fail-atom (atom nil)
                        :goal-atom (atom nil)}
                       (when environment {:environment environment})
                       (when strategy {:strategy strategy})
                       (when path {:path (as-file path)})
                       (when name {:name name})))))

(defn run [{:as a :keys [strategy run-atom fail-atom]}]
  (when (and (not *agent*) (run? a))
    (throw (Exception. "Cannot run agent while it is running.")))
  
  (debug (str "Run " (download-agent-to-str a)))

  (if (or (dead? a) (stop? a)) a
      (try (reset! run-atom true)
           (let [action (strategy a)]
             (with-return (action a)
               (reset! fail-atom nil)))
           (catch Error e
             (error (str "Error:" \newline
                         (download-agent-to-str a) \newline
                         e \newline))
             (reset! fail-atom nil)
             (throw e))
           (catch RuntimeException e
             (error (str "Runtime Exception:" \newline
                         (download-agent-to-str a) \newline
                         e \newline))
             (reset! fail-atom nil)
             (throw e))
           (catch Throwable e
             (with-return a
               (error (str "Fail:" \newline
                           (download-agent-to-str a) \newline
                           e \newline))
               (reset! fail-atom e)
               (release-pending-sends)))
           (finally (reset! run-atom false)))))

(defn file-length   [a] (body a :file-length))
(defn total-length  [a] (body a :total-length))
(defn aname         [a] (body a :name))
(defn service       [a] (body a :service))

(defn actual-file-length [file]
  (if (.exists file) (.length file) 0))

(defn out-of-space-on-path? [{:as a :keys [path file total-length]}]
  {:pre [(supplied path file total-length)]}
  (if (.exists file)
    (< (.getUsableSpace path) (- total-length (.length file)))
    (= (.getUsableSpace path) 0)))

(defn fully-loaded? [{:as a :keys [file total-length]}]
  {:pre [(supplied file total-length)]}
  (boolean (and (.exists file) (<= total-length (.length file)))))

(defalias pass identity)

(defn die [a]
  (assoc a :alive false))

(defn sleep [a millis]
  (with-return a (Thread/sleep millis)))

(defn get-file [{:as a :keys [name path]}]
  {:pre [(supplied name path)]}
  (assoc a :file (new File path name)))

(defvar- timeout-after-fail*        3000)
(defvar- connection-timeout*       15000)
(defvar- get-request-timeout*      30000)
(defvar- head-request-timeout*     10000)
(defvar- buffer-size*              65536)

(defn download-agent-performance [a]
  (let [total-length (total-length a)
        file-length  (file-length a)]
    {:percent (if (and (number? total-length) (number? file-length)
                       (pos? total-length) (pos? file-length))
                (/ file-length total-length)
                0)}))

(defvar progress-agent* (agent {:agents #{}}))

(defn begin-monitor-progress [a]
  {:pre (agent? a)}
  (send-off progress-agent* assoc :agents
            (union (@progress-agent* :agents) #{a})))

(defn cease-monitor-progress [a]
  {:pre (agent? a)}
  (send-off progress-agent* assoc :agents
            (difference (@progress-agent* :agents) #{a})))

(defn update-progress []
  (send-off progress-agent*
            (fn [progress-agent]
              (with-return progress-agent
                (let [progress-strings
                  (for [a (@progress-agent* :agents)
                        :let [name (agent-name a)]
                        :let [performance (download-agent-performance a)]
                        :let [percent (:percent performance)]]
                    (str \[
                         (cond (not name) \-
                               (< (count name) 12) name
                               :longer (let [name-head (apply str (take 5 name))
                                             name-tail (apply str (take-last 7 name))]
                                         (str name-head \. \. name-tail)))
                         \space percent \% \]))]
                  (.println System/out (apply str \return progress-strings))
                  (.flush System/out))))))

(defmacro with-monitoring-progress [a & body]
  `(try (when (agent? ~a)
          (begin-monitor-progress ~a))
        (do ~@body)
        (finally (cease-monitor-progress ~a))))

(defn download
  [{:as a :keys [name address file total-length file-length-atom]}]
  {:pre [(supplied name address file total-length file-length-atom)]}
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
                (throw (Exception. "Cannot check file before download."))

                (not= content-length (- total-length (actual-file-length file)))
                (throw (Exception. "Downloading file size mismatch."))

                :else
                (with-return a
                  (with-open [#^InputStream input (.getResponseBodyAsStream get)
                              #^FileOutputStream output (FileOutputStream. file true)]
                    (with-monitoring-progress *agent*
                      (info (str "Begin download " name))
                      (let [buffer (make-array Byte/TYPE buffer-size*)]
                        (loop [file-size (actual-file-length file)]
                          (let [read-size (.read input buffer)]
                            (when (pos? read-size)
                              (let [new-size (+ file-size read-size)]
                                (.write output buffer 0 read-size)
                                (reset! file-length-atom new-size)
                                (when (not (stop? a))
                                  (recur new-size))))))
                        (.flush output)))
                    (info (str "End download " name))))))
        (finally (.releaseConnection get))))))

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
                       (merge a {:total-length content-length}
                              (when (not name) {:name filename}))))
             (throw (Exception. "HEAD request failed."))))
         (finally (.releaseConnection head)))))

(defn download-environment-is-done? [e]
  (every? dead? (agents e)))

(defn terminate-download-environment [e] nil)

(defn try-to-terminate-download-environment [e]
  (when (download-environment-is-done? e)
    (terminate-download-environment e)))

(defn idle-service-successors [a]
  (select (succeeded (agents (env a))) :order-by precedence
          :where (fn/and idle? (partial same service a))))

(defn failed-service-successors-after [a]
  (select (succeeded (agents (env a))) :entirely-after a :order-by precedence
          :where (fn/and failed? (partial same service a))))

(defn alive-service-successors-after [a]
  (select (succeeded (agents (env a))) :entirely-after a :order-by precedence
          :where (fn/and alive? (partial same service a))))

(defn running-service-agents [service environment]
  (select (succeeded (agents environment))
          :where #(and (= service (service %)) (run? %))))

(defn files-dsv-*-data-cod-ru-strategy
  [{:as a :keys [address name file path total-length service max-active-agents]}]
  (let [get-successor
        (fn [] (when *agent* (or (first (idle-service-successors *agent*))
                                (first (failed-service-successors-after *agent*))
                                (first (alive-service-successors-after *agent*)))))
        schedule
        (fn [action body]
          (try
            (let [new-body (action body)]
              (with-return new-body
                (when *agent*
                  (let [successor (get-successor)]
                    (if (dead? new-body)
                      (when successor (send-off successor run))
                      (send-off *agent* run))))))
            (catch Throwable t
              (when *agent*
                (let [successor (get-successor)]
                  (send-off *agent* sleep timeout-after-fail*)
                  (when successor (send-off successor run))))
              (throw t))))]
    (cond (not address) (add-hook die schedule)
          
          (> (count (running-service-agents service (env a))) max-active-agents)
          pass

          (not (and name total-length))
          (add-hook files-dsv-*-data-cod-ru-get-head schedule)
          
          (not file) (add-hook get-file schedule)
          
          (or (out-of-space-on-path? a) (fully-loaded? a))
          (add-hook die schedule)
          
          :else (add-hook download schedule))))

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
               (if link (assoc a :link link)
                   (die a)))
             (throw (Exception. "Fail to parse page."))))
         (finally (.releaseConnection get)))))

(defn data-cod-ru-make-child-agent [{:as a :keys [link path precedence]}]
  {:pre [(supplied link path precedence (env a))]}
  (let [child (make-download-agent link :environment (env a)
                                   :precedence precedence
                                   :path path)]
    (if-not child (die a)
            (do (send-off child run)
                (assoc a :child child)))))

(defn data-cod-ru-strategy [{:as a :keys [address link child]}]
  (let [schedule
        (fn [action body]
          (try
            (let [new-body (action body)]
              (with-return new-body
                (when *agent*
                  (when (alive? new-body) (send-off *agent* run)))))
            (catch Throwable t
              (when *agent*
                (send-off *agent* sleep timeout-after-fail*)
                (send-off *agent* run)))))]
    (cond (not (and address (env a))) die
          (not link)     (add-hook data-cod-ru-parse-page schedule)
          (not child)    (add-hook data-cod-ru-make-child-agent schedule)
          :else          die)))

(defservice data-cod-ru
  :address-pattern #"http://[\w\-]*.data.cod.ru/\d+"
  :strategy data-cod-ru-strategy
  :body #(hash-map :address nil :link nil :child nil))

(defservice files3?-dsv-*-data-cod-ru
  :address-pattern #"http://files3?.dsv.*.data.cod.ru/.+"
  :strategy files-dsv-*-data-cod-ru-strategy
  :max-active-agents 1
  :body #(hash-map :address nil :name nil :file nil :path nil
                   :total-length nil :file-length-atom (atom nil)))

(defservice files2-dsv-*-data-cod-ru
  :address-pattern #"http://files2.dsv.*.data.cod.ru/.+"
  :strategy files-dsv-*-data-cod-ru-strategy
  :max-active-agents 1
  :body #(hash-map :address nil :name nil :file nil :path nil
                   :total-length nil :file-length-atom (atom nil)))

(set-log :enable-levels [:debug])

(defn -main [& args]
  (with-command-line args
    "Use it!"
    [[quiet?  q? "Work quietly."]
     [debug?  d? "Print debug messages."]
     remaining-args]

    (set-log :levels (cond quiet? ()
                           debug? (list :debug :info :warn :error :fatal)
                           :else  (list :info :error :fatal)))
    
    (let [jobs-file (some #(as-file % :directory false :readable true) remaining-args)
          working-path (or (some #(as-file % :directory true :writeable true) remaining-args)
                           (as-file (System/getProperty "user.dir") :writeable true))]
      (when (and jobs-file working-path)
        (let [lines (duck/read-lines jobs-file)
              download-environment (make-environment)]
          (doseq [line lines]
            (make-download-agent line
                                 :environment download-environment
                                 :path working-path))
          
          (add-hook :after terminate-download-environment
                    :system-exit (fn [e] (System/exit 0)))
          
          (doseq [a (agents download-environment)]
            (send-off a run)))))))
