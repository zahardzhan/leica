;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns leica
  (:gen-class)
  (:use
   [clojure.set :only [difference union]]
   [clojure.contrib.duck-streams :only [slurp* read-lines]]
   clojure.contrib.command-line
   clojure.contrib.def
   clojure.test
   [log :only [info debug error]]
   hooks)

  (:require
   [clojure.contrib.io :as io])
  
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

(def timeout-after-fail* 3000)
(def connection-timeout* 15000)
(def get-request-timeout* 30000)
(def head-request-timeout* 30000)
(def buffer-size* 65536)

(defmacro with-return [expr & body]
  `(do (do ~@body)
       ~expr))

(defmacro let-return [[form val] & body]
  `(let [~form ~val]
     (with-return ~form
       (do ~@body))))

(defmacro when-supplied [& clauses]
  (if-not clauses true
          `(and (or (nil? ~(first clauses))
                    (do ~(second clauses)))
                (when-supplied ~@(next (next clauses))))))

(defalias supplied and)

(defn agent? [x]
  (instance? clojure.lang.Agent x))

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

(declare monitor-progress)

(def downloads* (ref #{}))

(defn add-to-downloads [dload]
  (dosync (alter downloads* union (hash-set dload))))

(defn remove-from-downloads [dload]
  (dosync (alter downloads* difference (hash-set dload))))

(defn surround [dload]
  (difference downloads* (hash-set dload)))

(def download-scheduler*
     (agent {:active true
             :done-hook nil ;; run when there are no more scheduling job
             :schedule-with-callback true
             :last-scheduled ()}))

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
             (assoc download-type :link link)))))

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

(defn type-dispatch
  ([x] (type x))
  ([x & xs] (type x)))

(defmulti alive? type-dispatch)
(defmulti idle? type-dispatch)
(defmulti running? type-dispatch)
(defmulti stopped? type-dispatch)
(defmulti failed? type-dispatch)
(defmulti dead? type-dispatch)

(defmulti run type-dispatch)
(defmulti stop type-dispatch)
(defmulti idle type-dispatch)
(defmulti sleep type-dispatch)
(defmulti fail type-dispatch)
(defmulti die type-dispatch)
(defalias pass identity)

(defmulti represent type-dispatch)
(defmulti performance type-dispatch)
(defmulti fail-reason type-dispatch)
(defmulti file-length type-dispatch)

(declare show-progress)

(defmethod represent ::download [{:as dload :keys [name link]}]
  (str "<Download " (apply str (take 30 (or name link))) \>))

(defmethod alive? ::download [dload]
  (:alive dload))

(defmethod dead? ::download [dload]
  (not (alive? dload)))

(defmethod failed? ::download [dload]
  (:failed dload))

(defmethod fail-reason ::download [dload]
  (:fail-reason dload))

(defmethod fail ::download [dload & {reason :reason}]
  (error "Error " (represent dload) " reason: " reason)
  (assoc dload :failed true :fail-reason reason))

(defmethod die ::download [dload]
  (assoc dload :alive false))

(defmethod sleep ::download [dload millis]
  (with-return dload (Thread/sleep millis)))

(defmethod stopped? ::download [dload]
  (deref (:stop-atom dload)))

(defmethod running? ::download [dload]
  (deref (:run-atom dload)))

(defmethod idle? ::download [dload]
  (and (alive? dload)
       (not (running? dload))
       (not (stopped? dload))
       (not (failed? dload))))

(defmethod idle ::download [dload]
  (assoc dload :failed false :fail-reason nil))

(defmethod stop ::download [dload])

(defmethod run ::download
  [{:as dload :keys [program run-atom stop-atom failed]} & {action :action}]

  (when (and (not *agent*) (running? dload))
    (throw (Exception. "Cannot run download while it is running.")))
  
  (debug "Run download " (represent dload))

  (if (or (dead? dload) (stopped? dload)) dload
      (try (reset! run-atom true)
           (let [action (or action (program dload))]
             (idle (action dload)))
           ;; (catch Error RuntimeException ...)
           (catch Throwable e (fail dload :reason e))
           (finally (reset! run-atom false)))))

(defmethod file-length ::download
  [{:as dload file-length-atom :file-length-atom}]
  (when file-length-atom
    (deref file-length-atom)))

(defmethod performance ::download
  [{:as dload :keys [total-file-length]}]
  (let [file-length (file-length dload)
        load-percent (when (and (number? total-file-length) (number? file-length)
                                (pos? total-file-length) (pos? file-length))
                       (int (Math/floor (* 100 (/ file-length total-file-length)))))]
    (merge {} (when load-percent {:load-percent load-percent}))))

(def eighty-spaces "                                                                                ")

(def progress-monitor* (agent {:agents #{}}))

(defn begin-monitor-progress [{:as progress-monitor agents :agents} agnt]
  {:pre (agent? agnt)}
  (assoc progress-monitor :agents (union agents (hash-set agnt))))

(defn cease-monitor-progress [{:as progress-monitor agents :agents} agnt]
  {:pre (agent? agnt)}
  (.print System/out (str "\r" eighty-spaces "\r"))
  (assoc progress-monitor :agents (difference agents (hash-set agnt))))

(defn monitor-progress []
  (send-off progress-monitor* show-progress)
  (release-pending-sends))

(defmacro with-progress-monitoring [agnt & body]
  `(let [agnt?# (agent? ~agnt)]
     (try (when agnt?# (send-off progress-monitor* begin-monitor-progress ~agnt))
          (do ~@body)
          (finally (when agnt?# (send-off progress-monitor* cease-monitor-progress ~agnt))))))

(defn show-progress [{:as progress-monitor agents :agents}]
  (with-return progress-monitor
    (.print System/out \return)
    (doseq [abody (map deref agents)
            :let [name (:name abody)
                  name-length (if (string? name) (count name) nil)
                  perf (performance abody)
                  load-percent (:load-percent perf)]]
      (.print System/out (str \[ (cond (not name) \-
                                       (< name-length 12) name
                                       :longer (str (.substring name 0 5) \. \.
                                                    (.substring name (- name-length 7) name-length)))
                              \space (or load-percent \0) \% \])))
    (.print System/out \return)
    ;; (.flush System/out)
    ))

(defn files*-dsv-*-data-cod-ru-get-head [{:as dload :keys [link name]}]
  {:pre [(supplied link)]}
  (let [client (new HttpClient)
        head (new HeadMethod link)]
    (.. client getHttpConnectionManager getParams 
        (setConnectionTimeout connection-timeout*))
    (.. head getParams (setSoTimeout head-request-timeout*))
    (try (let [status (.executeMethod client head)]
           (if (= status HttpStatus/SC_OK)
             (let [content-length (.. head (getResponseHeader "Content-Length"))
                   content-disposition (.. head (getResponseHeader "Content-Disposition"))]
               (if-not (or content-length content-disposition) (die dload)
                 (let [length (Integer/parseInt (.getValue content-length))
                       filename (URLDecoder/decode (second (re-find #"; filename=\"(.*)\"" (.getValue content-disposition))))]
                   (if-not (and length filename) (die dload)
                     (assoc dload
                       :total-file-length length
                       :file-length-atom (atom 0)
                       :name (or name filename))))))
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
                  (info "Begin download " name)
                  (with-progress-monitoring *agent*
                    (let [buffer (make-array Byte/TYPE buffer-size*)]
                      (loop [file-size (actual-file-length file)]
                        (let [read-size (.read input buffer)]
                          (when (pos? read-size)
                            (let [new-size (+ file-size read-size)]
                              (.write output buffer 0 read-size)
                              (reset! file-length-atom new-size)
                              (when *agent* (monitor-progress))
                              (when-not (stopped? dload)
                                (recur new-size))))))))
                  (.flush output)
                  (info "End download " name)))))
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
  (assoc files*-dsv-*-data-cod-ru :link-pattern #"http://files3?.dsv.*.data.cod.ru/.+"))

(def-download-type files2-dsv-*-data-cod-ru
  (assoc files*-dsv-*-data-cod-ru :link-pattern #"http://files2.dsv.*.data.cod.ru/.+"))

(def-download-type files3?-dsv-region-data-cod-ru
  (assoc files*-dsv-*-data-cod-ru :link-pattern #"http://files3?.dsv-region.data.cod.ru/.+"))

(def-download-type files2-dsv-region-data-cod-ru
  (assoc files*-dsv-*-data-cod-ru :link-pattern #"http://files2.dsv-region.data.cod.ru/.+"))

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
                                       (slurp* (.getResponseBodyAsStream get)))]
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

(declare schedule-downloads callback-download-scheduler)

(defn callback-download-scheduler-from [dload dload-agent]
  (with-return dload
    (send-off download-scheduler* schedule-downloads :callback dload-agent)))

(defn schedule-downloads [{:as scheduler :keys [active done-hook schedule-with-callback last-scheduled]}
                          & {:keys [callback]}]
  {:pre [(agent? callback)]}
  (cond
   ;; when inactive then leave
   (not active)
   (assoc scheduler :last-scheduled ())
   
   ;; if there are no downloads to schedule or all downloads is dead
   ;; then leave
   (or (not (seq @downloads*))
       (every? (comp dead? deref) @downloads*))
   (with-return (assoc scheduler :last-scheduled ())
     (when done-hook (done-hook)))
   
   :shedule
   (let [groups ;; hash map of downloads grouped by type, each group
                ;; sorted by precedence
         (into {} (for [[dloads-type dloads] (group-by (comp type deref) @downloads*)]
                    {dloads-type (sort-by (comp :precedence deref) dloads)}))

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
     
     (with-return (assoc scheduler :last-scheduled successors)
       (doseq [successor successors]
         (send-off successor run)
         (when schedule-with-callback
           (send-off successor callback-download-scheduler-from successor)))))))

(def help* 
     "Leica -- downloader written in lisp.

Download files:

leica [keys] [file with links] [directory]")

(defn exit-program []
  (debug "Leica is done. Bye.")
  (System/exit 0))

(defn -main [& args]
  (with-command-line args help*
    [[quiet?  q? "work quietly"]
     [debug?  d? "write debug messages"]
     remaining-args]

    (let [lines-with-links
          (read-lines (some #(as-file % :readable true :directory false) remaining-args))

          workpath
          (or (some #(as-file % :writeable true :directory true) remaining-args)
              (as-file (System/getProperty "user.dir") :writeable true :directory true))]

      (when-not lines-with-links
        (info "You must specify file with links to download.")
        (exit-program))

      (when-not workpath
        (info "You must specify directory in which files will be downloaded.")
        (exit-program))

      (doseq [line lines-with-links]
        (make-download line :path workpath))

      (send-off download-scheduler* assoc :done-hook exit-program)
      (send-off download-scheduler* schedule-downloads))))

(comment
  (do
    (def d1 (make-download "http://files.dsv-region.data.cod.ru/?WyJjMjMyOWMwZWZhNzdjOGFiMWUwNzMwYmI5YTM1OWNmOSIsMTI4ODUwNzAzNywid3d3LmRpc2x5Lm5ldC5ydV9cdTA0MzhcdTA0NDJcdTA0MzBcdTA0M2JcdTA0NGNcdTA0NGZcdTA0M2RcdTA0M2UucmFyIiwiSlJwOFR5aGUwQjNJWWFmUU5FZjF6bmxiZkQ0anIzOXR5cGxCRFE1a1Y5T3ZwcmRnTDhKUlJFVGMyangrTkw5a0UrTFM2NlVRUlVtRUlxazZITFRJMmZoZUVnUWcyWUpwbFRVM3VtSzIxa3FBcm13TzgzRitJOXp3amR1eExsZjF2Y3dwQzdONGRrdDNZaVBvdTZGbkhKdm1LY3g3QnU4QVlzd016bktsU3dVPSJd"
                           :path "/home/haru/Inbox/"))
    
    (def d2 (make-download "http://files2.dsv-region.data.cod.ru/proxy/1/?WyJjNDc5MTg1ZjkyNTZkNWJmZWM3Mzc1NmY5ZDAzYjRhMCIsIjEyODg3NDUyODUiLCJcdTA0MWFcdTA0M2RcdTA0MzhcdTA0MzNcdTA0MzAgJydcdTA0MjBcdTA0NDNcdTA0NDFcdTA0NGMgXHUwNDM4XHUwNDM3XHUwNDNkXHUwNDMwXHUwNDQ3XHUwNDMwXHUwNDNiXHUwNDRjXHUwNDNkXHUwNDMwXHUwNDRmJycucmFyIiwiaVNBSXRcL041Q0tYRXNCY0FIZ01ld2NHMVhXYlZHZFREbjdUbk4xMnR3UEllU0tYaXNHNHh4Z3FXc2swdk52ZW1cL3hQWlFCYlRWdUhNK0pXQVwvRTl0ck12bUp0bCtQV2Y3N25UVE1ncDRwTkhtdlhWMUp5QlNGdVZnWFVDTThpQSs3d3h0ZVB6VG9DN1ZBT1N5N01peHVnTE5jeFJiaFFjeHcwNzlWTGRLcllVPSJd"
                           :path "/home/haru/Inbox/"))

    (def d3 (make-download "http://files2.dsv-region.data.cod.ru/proxy/3/?WyIwMmM4ZmJiMGQxMDMzOWM3MjY3NmQzNDUyNDUyYjA1ZSIsIjEyODg3NDQwNzMiLCJrYXRyaWcyLmF2aSIsImMyUkNNNEphSVBkWnMyZWJZWW4rSTZsck9JamZPYytzdlwvU0lxcnUzcFFFWXdoYjFFRmZ1ZnNcL1FmTFBMZ1lFOGFcL0lTSmREZmFmNmVGd2sxaUo0V3d4cG1rUVFHUXdhcHI3VWowdUhuUTdDcW52d256UW1DaVhcL2RZWTFTakF6RjlmT0pzWkNOa1FiN214RE9YSnhUZXZ5RWpCc0dVNHNqaDEzMHByNlRQenc9Il0%3D"
                           :path "/home/haru/Inbox/"))

    (def d4 (make-download "http://files2.dsv-region.data.cod.ru/proxy/3/?WyJhMmFmMjlhMTFiZmM0MmQ2Yjg3OTg5NDAzODNjYTExNyIsIjEyODg3MDc5ODgiLCJ3d3cuZGlzbHkubmV0LnJ1X1x1MDQ0MVx1MDQzZlx1MDQzZVx1MDQ0MFx1MDQ0Mlx1MDQzOFx1MDQzMlx1MDQzZFx1MDQ0Ylx1MDQzNSBcdTA0M2NcdTA0MzBcdTA0NDhcdTA0MzhcdTA0M2RcdTA0NGIucmFyIiwieXpWQTVYcTBZNzNXdWpZY0JHbXBvR3g4NG55VHR4YlJBdEhDOE1iYU4xMEthTlphdENtT2FQQ25nSkttSG5kaHkrRTlFcmtKYWdvN0ViQnJwNG5Rd0cyUUhpUG5sV3RNdTBWdzBYV3RNZVNvd3JHYTYyNytGNFFETzNIZG0yaXpDWTNnZjU4cHY4Z2tNN2d6eHpHR2RBd1ZDbTdNQ0NmaHduSEsyYnFrdWdjPSJd"
                           :path "/home/haru/Inbox/"))

    (def d5 (make-download "http://files2.dsv-region.data.cod.ru/proxy/1/?WyIwOGNhY2MyYjRlYjIxZWM4ZjU4ZWQ5Yzc5ZGU1YWEwNiIsIjEyODg3MDI0MzYiLCJ3d3cuZGlzbHkubmV0LnJ1X1x1MDQyNVx1MDQ0ZFx1MDQzYlx1MDQ0M1x1MDQzOFx1MDQzZC5yYXIiLCJQYnNjV21WZWJReUoxbkZtNDBNcStCNzBQRnlkUVVTVDlaNE81cVpyXC9EYVMzVUw0empOQW01RE9UNUdiZ09Da2xsMXBzT0lQSTZKdGJlOWpcL2xKb3A1dHhBQUxRMnorWEZFWWFjcmlaKzY5YllneTBtdzBcL1pHXC9XTEFaQVpQc25rMnU4TGV1MFUzQjBYaHAzZ2phVXlUOSs2bTNoa0loeEJRaTZXcElwQmZNPSJd"
                           :path "/home/haru/Inbox/"))

    (def d6 (make-download "http://files.dsv-region.data.cod.ru/?WyJhY2NhOTZhYTY0ODdmOTZkY2YyZmZmMTgwNGFiNTg0ZSIsMTI4ODkxOTYxOSwid3d3LmRpc2x5Lm5ldC5ydV9cdTA0MjVcdTA0NGRcdTA0M2JcdTA0NDNcdTA0MzhcdTA0M2QucmFyIiwiZlwvZG45dCtlK2hNQmJxb3VnZ0pIQ0FqXC9IK1pZYUpDTWN1SUxRMGxrbTdGRG9Fa3RyTkVjSEpCTUhWb25zSHVITERoaHpTaTNydHg4WnpXRVRGS3M2VnRDdVhvblV2NGxId0xCUnpmWmp4TWx6aWRxVm5sWFg1UjE5SEZaSmRPUVQ0eG01XC9VVzNaVkE5ZFkrb1pcLzA2WnhrOURXcncwYXN5VUlneE16RnVCdz0iXQ%3D%3D"
                           :path "/home/haru/Inbox/")))

  (send-off download-scheduler* assoc :schedule-with-callback false)

  {:dloads (count @downloads*)
   :dead (count (filter (comp dead? deref) @downloads*))
   :idle (count (filter (comp idle? deref) @downloads*))
   :failed (count (filter (comp failed? deref) @downloads*))
   :run (count (filter (comp running? deref) @downloads*))
   :scheduled (count (:last-scheduled @download-scheduler*))
   :progress (count (:agents @progress-monitor*))}
  
  (send-off download-scheduler* schedule-downloads)
  download-scheduler*
  downloads*)
