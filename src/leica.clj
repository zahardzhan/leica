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
   [com.twinql.clojure.http :as http]
   [clojure.contrib.duck-streams :as duck]
   [clojure.contrib.io :as io]
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

(defvar timeout-after-fail* 3000)
(defvar connection-timeout* 15000)
(defvar get-request-timeout* 30000)
(defvar head-request-timeout* 30000)
(defvar buffer-size* 65536)

;; Даже несколько удивлен тем, что функция высшего порядка same
;; практически нигде не используется. Что может быть логичнее чем код
;; типа (same parents alice bob).

(defn same [f & xs]
  (apply = (map f xs)))

;; Выполняет блок кода body, при этом возвращает значение выражения
;; expr.

(defmacro with-return [expr & body]
  `(do (do ~@body)
       ~expr))

;; Связывает значение val с именем form, выполняет блок кода body и в
;; конце возвращает значение form.

(defmacro let-return [[form val] & body]
  `(let [~form ~val]
     (with-return ~form (do ~@body))))

(defn dref
  ([] nil)
  ([x] (if (instance? clojure.lang.IDeref x) (deref x) x))
  ([x & fs] ((apply comp (reverse fs)) (dref x))))

(defmacro with-deref [[ref & refs] & body]
  (if ref
    `(let [~ref (dref ~ref)]
       (with-deref ~refs ~@body))
    `(do ~@body)))

(defmacro when-supplied [& clauses]
  (if-not clauses true
          `(and (or (nil? ~(first clauses))
                    (do ~(second clauses)))
                (when-supplied ~@(next (next clauses))))))

(defalias supplied and)

(defn agent? [x]
  (instance? clojure.lang.Agent x))

(def nothing nil)

(defn take-after [item coll]
  (rest (drop-while (partial not= item) coll)))

(defn take-before [item coll]
  (take-while (partial not= item) coll))

(defn take-entirely-after [item coll]
  (let [after (take-after item coll)
        before (take-before item coll)]
    (concat after before
            (when-not (= (count before) (count coll))
              (list item)))))

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

(defn list-files [directory]
  (seq (.listFiles (as-file directory :readable true :directory true))))

(defn actual-file-length [file]
  (if (.exists file) (.length file) 0))

(defn extract-url [line]
  (first (re-find #"((https?|ftp|file):((//)|(\\\\))+[\w\d:#@%/;$()~_?\+-=\\\.&]*)"
                  line)))

(def downloads* (ref #{}))

(defn add-to-downloads [dload]
  (dosync (alter downloads* union (hash-set dload))))

(defn remove-from-downloads [dload]
  (dosync (alter downloads* difference (hash-set dload))))

(defn surround [dload]
  (difference downloads* (hash-set dload)))

(def download-scheduler*
     (agent {:done-hook nil ;; run when there are no more scheduling job
             :schedule-with-callback false}))

(def download-types* (atom {}))

(defmacro def-download-type [name body]
  `(let [name-keyword# (keyword (str *ns*) (str (quote ~name)))]
     (def ~name (with-meta ~body {:type name-keyword#}))
     (derive name-keyword# ::download)
     (swap! download-types* assoc name-keyword# ~name)
     ~name))

(defn download-type-matching-address [line]
  (when-let [url (extract-url line)]
    (first (for [[type-keyword download-type] @download-types*
                 :when (:link-pattern download-type)
                 :let [link (re-find (:link-pattern download-type) url)]
                 :when link]
             (merge download-type {:link link})))))

(def downloads-precedence-counter (atom 0))

(defn make-download [line & {:as opts :keys [program path name]}]
  (when-let [download-type (download-type-matching-address line)]
    (let-return
     [dload (agent
             (merge
              download-type
              {:precedence (swap! downloads-precedence-counter inc)
               :alive true
               :failed false
               :fail-reason nil
               :run-atom (atom false)
               :stop-atom (atom false)}
              (dissoc opts :program :path :name)
              (when (supplied program)
                (if-not (ifn? program)
                  (throw (Exception. "Program must be invokable."))
                  {:program program}))
              (when (supplied path)
                (if-let [valid-path (as-file path :directory true :writeable true)]
                  {:path valid-path}
                  (throw (Exception. "Path must be writeable directory."))))
              (when (supplied name)
                (if-not (string? name)
                  (throw (Exception. "Name must be string."))
                  {:name name}))))]
     (add-to-downloads dload))))

(defn download-repr [{:as dload :keys [name link]}]
  (str "<Download " (apply str (take 30 (or name link))) \>))

(defn alive? [dload]
  (:alive dload))

(defn dead? [dload]
  (not (alive? dload)))

(defn failed? [dload]
  (:failed dload))

(defn fail-reason [dload]
  (:fail-reason dload))

(defn fail [dload & {reason :reason}]
  (log/error (str "Error:" \space (download-repr dload) \newline reason \newline))
  (assoc dload :failed true :fail-reason reason))

(defn die [dload]
  (assoc dload :alive false))

(defalias pass identity)

(defn sleep [dload millis]
  (with-return dload (Thread/sleep millis)))

(defn stopped? [dload]
  (deref (:stop-atom dload)))

(defn stop-download [dload])

(defn running? [dload]
  (deref (:run-atom dload)))

(defn idle? [dload]
  (and (alive? dload)
       (not (running? dload))
       (not (stopped? dload))
       (not (failed? dload))))

(defn idle [dload]
  (assoc dload :failed false :fail-reason nil))

(defn file-length [{:as dload file-length-atom :file-length-atom}]
  (when file-length-atom
    (deref file-length-atom)))

(defn run-download [{:as dload :keys [program run-atom stop-atom failed]} & {action :action}]
  (when (and (not *agent*) (running? dload))
    (throw (Exception. "Cannot run download while it is running.")))
  
  (log/debug (str "Run download " (download-repr dload)))

  (if (or (dead? dload) (stopped? dload)) dload
      (try (reset! run-atom true)
           (let [action (or action (program dload))]
             (idle (action dload)))
           ;; (catch Error RuntimeException ...)
           (catch Throwable e (fail dload :reason e))
           (finally (reset! run-atom false)))))

(defn files*-dsv-*-data-cod-ru-get-head [{:as dload :keys [link name]}]
  {:pre [(supplied link)]}
  (let [client (new HttpClient)
        head (new HeadMethod link)]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout*))
    (.. head getParams (setSoTimeout head-request-timeout*))
    (try (let [status (.executeMethod client head)]
           (if (= status HttpStatus/SC_OK)
             (let [length (Integer/parseInt
                           (.. head (getResponseHeader "Content-Length") (getValue)))
                   disposition (.. head (getResponseHeader "Content-Disposition") (getValue))
                   filename (URLDecoder/decode (second (re-find #"; filename=\"(.*)\"" disposition)))]
               (if-not (and length filename) (die dload)
                       (merge dload
                              {:total-file-length length
                               :file-length-atom (atom 0)}
                              (when (not name) {:name filename}))))
             (throw (Exception. "HEAD request failed."))))
         (finally (.releaseConnection head)))))

(defn get-file [{:as dload :keys [name path]}]
  {:pre [(supplied name path)]}
  (assoc dload :file (new File path name)))

(defn out-of-space-on-path? [{:as dload :keys [path file total-file-length]}]
  {:pre [(supplied path file total-file-length)]}
  (if (.exists file)
    (< (.getUsableSpace path) (- total-file-length (.length file)))
    (= (.getUsableSpace path) 0)))

(defn fully-loaded? [{:as dload :keys [file total-file-length]}]
  {:pre [(supplied file total-file-length)]}
  (boolean (and (.exists file) (<= total-file-length (.length file)))))

(defn begin-download
  [{:as dload :keys [name link file total-file-length file-length-atom]}]
  {:pre [(supplied name link file total-file-length)]}
  (let [client (HttpClient.)
        get (GetMethod. link)]
    (try
      (.. client getHttpConnectionManager getParams 
          (setConnectionTimeout connection-timeout*))
      (.. get getParams (setSoTimeout get-request-timeout*))
      (.setRequestHeader get "Range" (str "bytes=" (actual-file-length file) \-))
      (.executeMethod client get)
      (let [content-length (.getResponseContentLength get)]
        (cond (not content-length)
              (throw (Exception. "Cannot check file length before download."))

              (not= content-length (- total-file-length (actual-file-length file)))
              (throw (Exception. "Downloading file size mismatch."))

              :else
              (with-return dload
                (with-open [#^InputStream input (.getResponseBodyAsStream get)
                            #^FileOutputStream output (FileOutputStream. file true)]
                  (log/info (str "Begin download " name))
                  (let [buffer (make-array Byte/TYPE buffer-size*)]
                    (loop [file-size (actual-file-length file)]
                      (let [read-size (.read input buffer)]
                        (when (pos? read-size)
                          (let [new-size (+ file-size read-size)]
                            (.write output buffer 0 read-size)
                            (reset! file-length-atom new-size)
                            (when-not (stopped? dload)
                              (recur new-size)))))))
                  (.flush output)
                  (log/info (str "End download " name))))))
      (finally (.releaseConnection get)))))

(defn files*-dsv-*-data-cod-ru-download-program
  [{:as dload :keys [link name file path total-file-length]}]
  (cond (not link) die
        (not (and name total-file-length)) files*-dsv-*-data-cod-ru-get-head
        (not file) get-file
        (or (out-of-space-on-path? dload) (fully-loaded? dload)) die
        :requirements-ok begin-download))

(def files*-dsv-*-data-cod-ru
     {:program files*-dsv-*-data-cod-ru-download-program
      :max-running-downloads 1
      :link nil
      :name nil
      :path nil
      :file nil
      :total-file-length nil})

(def-download-type files3?-dsv-*-data-cod-ru
  (merge files*-dsv-*-data-cod-ru
         {:link-pattern #"http://files3?.dsv.*.data.cod.ru/.+"}))

(def-download-type files2-dsv-*-data-cod-ru
  (merge files*-dsv-*-data-cod-ru
         {:link-pattern #"http://files2.dsv.*.data.cod.ru/.+"}))

(def-download-type files-dsv-region-data-cod-ru
  (merge files*-dsv-*-data-cod-ru
         {:link-pattern #"http://files.dsv-region.data.cod.ru/.+"}))

(def-download-type files2-dsv-region-data-cod-ru
  (merge files*-dsv-*-data-cod-ru
         {:link-pattern #"http://files2.dsv-region.data.cod.ru/.+"}))

(defn data-cod-ru-parse-page [{:as dload :keys [link]}]
  {:pre [(supplied link)]}
  (let [client (new HttpClient)
        get (new GetMethod link)]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout*))
    (.. get getParams (setSoTimeout get-request-timeout*))
    (try (let [status (.executeMethod client get)]
           (if (= status HttpStatus/SC_OK)
             (let [child-link (re-find #"http://files[-\d\w\.]*data.cod.ru/[^\"]+"
                                       (duck/slurp* (.getResponseBodyAsStream get)))]
               (if child-link
                 (assoc dload :child-link child-link)
                 (die dload)))
             (throw (Exception. "Fail to parse page."))))
         (finally (.releaseConnection get)))))

(defn data-cod-ru-make-child-download [{:as dload :keys [link child-link path]}]
  {:pre [(supplied link child-link path)]}
  (let [child (or (first (for [dl @downloads* :when (= child-link (:link @dl))] dl))
                  (make-download child-link :path path))]
    (if-not child
      (die dload)
      dload)))

(defn data-cod-ru-download-program [{:as dload :keys [link child-link child]}]
  (cond (not link) data-cod-ru-parse-page
        (not child) data-cod-ru-make-child-download
        :finally die))

(def-download-type data-cod-ru
  {:link-pattern #"http://[\w\-]*.data.cod.ru/\d+"
   :program data-cod-ru-download-program
   :link nil
   :path nil
   :child-link nil
   :child nil})

(defn exit-program []
  (log/debug "Leica is done. Bye."))

(defn callback-download-scheduler [dload]
  (when *agent*
    (send-off download-scheduler* schedule-downloads :callback *agent*)))

(defn schedule-downloads [{:as scheduler :keys [done-hook schedule-with-callback]}
                          & {:keys [callback]}]
  (cond
   ;; if there are no downloads to schedule or all downloads is dead
   ;; then leave
   (or (not (seq @downloads))
       (every? (comp dead? deref) @downloads))
   (with-return scheduler
     (when donehook (done-hook)))
   
   :shedule
   (let [groups ;; hash map of downloads grouped by type, each group
                ;; sorted by precedence
         (into {} (for [[type dloads] (group-by (comp type deref) @downloads)]
                    {type (sort-by (comp :precedence deref) dloads)}))

         successors ;; list of download successors
         (flatten
          (for [[dloads-type dloads] groups
                :let [max-running-dloads ;; maximum amount of running
                      ;; downloads for this type of downloads
                      (:max-running-downloads (dloads-type @download-types*))
                      running-dloads (filter (comp running? deref) dloads)
                      count-of-running-dloads (count running-dloads)
                      idle-dloads (filter (comp idle? deref) dloads)
                      failed-dloads (filter (comp failed? deref) dloads)
                      count-of-dloads-to-launch (- max-running-dloads count-of-running-dloads)]]
            (cond
             ;; if no downloads in group then leave
             (not (seq dloads)) ()

             ;; if all downloads in group is dead then leave
             (every? (comp dead? deref) dloads) ()

             ;; if there are more running downloads then it might be for
             ;; that group then leave
             (<= count-of-dloads-to-launch 0) ()

             ;; schedule downloads from scratch
             (not callback)
             (take count-of-dloads-to-launch (concat idle-dloads failed-dloads))

             callback
             (take count-of-dloads-to-launch
                   (concat idle-dloads (take-entirely-after callback failed-dloads))))))]
     
     (with-return scheduler
       (doseq [successor successors]
         (send-off successor run-download)
         (when schedule-with-callback
           (send-off successor callback-download-scheduler)))))))

(defmulti performance type)

(defmethod performance ::download
  [{:as dload :keys [total-file-length]}]
  (let [file-length (file-length dload)
        load-percent (when (and (number? total-file-length) (number? file-length)
                                (pos? total-file-length) (pos? file-length))
                       (/ file-length total-file-length))]
    (merge {} (when load-percent {:load-percent load-percent}))))

(performance @(first @downloads*))

(def progress-monitor* (agent {:agents #{}}))

(defn begin-monitor-progress [{:as progress-monitor agents :agents} agnt]
  {:pre (agent? agnt)}
  (assoc progress-monitor :agents (union agents (hash-set agnt))))

(defn cease-monitor-progress [{:as progress-monitor agents :agents} agnt]
  {:pre (agent? agnt)}
  (assoc progress-monitor :agents (difference agents (hash-set agnt))))

(defmacro with-progress-monitoring [agnt & body]
  (if-not (agent? agnt)
    `(do ~@body)
    `(try (send-off progress-monitor* begin-monitor-progress ~agnt)
          (do ~@body)
          (finally (send-off progress-monitor* cease-monitor-progress ~agnt)))))

(defn show-progress [{:as progress-monitor agents :agents}]
  (with-return progress-monitor
    (.print System/out \return)
    (doseq [abody (map deref agents)
            :let [name (:name abody)
                  name-length (if (string? name) (count name) nil)
                  perf (performance abody)
                  load-percent (:load-percent perf)]]
      (.print
       System/out
       (str \[
            (cond (not name) \-
                  (< name-length 12) name
                  :longer (str (.substring name 0 5) \. \.
                               (.substring name (- name-length 7) name-length)))
            \space (or load-percent \0) \% \])))
    (.println System/out)
    (.flush System/out)))

(comment
  (show-progress {:agents @downloads*})
  (def d1 (make-download "http://files2.dsv-region.data.cod.ru/proxy/3/?WyI0NjcyZTBhYTJiNzc1ZTNjMjRlYWFhMTc4MDgxMzk4MCIsIjEyODgzNDUzMzEiLCJwcmlrb2xfcmFzdGlzaGthLmZsdiIsIm5UbE1obFF2ZVVKNXZRUFpkMlVxVnQyakdxUDFyOXZPN0NpNlwvYTBLMUd1RWRzT1NoMnA5OU5EWllLaGhjaXptSzIrN09HU2w3VkdNTUVNcTd1RjdWTEZzUkI5K1liZFFsb2hiQWRNcjdTNUh6QXpIcmRtREQxaG5hVTVtaXJzR3k3WFRkWCt0MnFXc2xNc0ZcLzdGakRXT3FEV1wvSWZHNmQwM2oyTlwvUlpUUU09Il0%3D"
                         :path "/home/haru/Inbox/"))
  (def d2 (make-download "http://files.dsv-region.data.cod.ru/?WyJjMjMyOWMwZWZhNzdjOGFiMWUwNzMwYmI5YTM1OWNmOSIsMTI4ODUwNzAzNywid3d3LmRpc2x5Lm5ldC5ydV9cdTA0MzhcdTA0NDJcdTA0MzBcdTA0M2JcdTA0NGNcdTA0NGZcdTA0M2RcdTA0M2UucmFyIiwiSlJwOFR5aGUwQjNJWWFmUU5FZjF6bmxiZkQ0anIzOXR5cGxCRFE1a1Y5T3ZwcmRnTDhKUlJFVGMyangrTkw5a0UrTFM2NlVRUlVtRUlxazZITFRJMmZoZUVnUWcyWUpwbFRVM3VtSzIxa3FBcm13TzgzRitJOXp3amR1eExsZjF2Y3dwQzdONGRrdDNZaVBvdTZGbkhKdm1LY3g3QnU4QVlzd016bktsU3dVPSJd"
                         :path "/home/haru/Inbox/"))
  (schedule-downloads @download-scheduler*)
  d1
  ((@d1 :program) @d1)
  (send-off d1 ((@d1 :program) @d1))
  (((@d1 :program) @d1) @d1)

  d2
  (send-off d2 ((@d2 :program) @d2))
  (((@d2 :program) @d2) @d2)
  )
