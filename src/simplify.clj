;;; -*- mode: clojure; coding: utf-8 -*-

;; Copyright (C) 2010 Roman Zaharov <zahardzhan@gmail.com>

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(ns simplify
  (:use
   clojure.set
   clojure.test
   clojure.contrib.def)
  
  (:require
   [clojure.contrib.logging :as log]
   [clojure.contrib.duck-streams :as duck]
   [async.http.client :as HTTP]
   fn)
  
  (:import
   (java.io File
            FileOutputStream
            InputStream
            InterruptedIOException)
   (java.net ConnectException)))

;; Don't print delays
(defmethod print-method clojure.lang.Delay [x w]
  ((get-method print-method Object) x w))

(defmacro with-return
  [expr & body]
  `(do (do ~@body)
       ~expr))

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

(defn as-file [x]
  (let [type (type x)]
    (cond (= type File) x
          (= type String) (new File x))))

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

(defn surrounding "The agents belonging to environment this agent belonging to." [a]
  (when-let [e (env a)] (difference (agents e) #{a})))

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

(defn can-write-to-directory? [dir]
  (and (.exists dir) (.isDirectory dir) (.canWrite dir)))

(def *precedence* (atom 0))

(defn make-download-agent
  [line & {:as opts :keys [environment strategy precedence path name]}]
  {:pre [(when-supplied strategy     (instance? clojure.lang.IFn strategy)
                        precedence   (number? precedence)
                        environment  (map? environment)
                        path         (can-write-to-directory? (as-file path))
                        name         (string? name))]}
  (when-let [body (make-download-agent-body line)]
    (make-agent (merge body
                       {:state-atom (atom :idle)
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
  (when-not (({:idle     #{:running}
               :failed   #{:running}
               :dead     #{}
               :running  #{:stopping :idle :failed :dead}
               :stopping #{:idle :failed :dead}}
              (state a)) new-state)
    (throw (new AssertionError
                "Download agent cannot change state.")))
  (with-return a (reset! state-atom new-state)))

(defn state? [a in-state]
  (cond (keyword? in-state) (= in-state (state a))
        (set? in-state)     (keyword? (in-state (state a)))))

(defn dead?   [a] (state? a :dead))
(defn alive?  [a] (not (dead? a)))
(defn active? [a] (state? a #{:running :stopping}))
(defn idle?   [a] (state? a :idle))
(defn fail?   [a] (state? a :failed))

(defn pass    [a] a)
(defn idle    [a] (state! a :idle))
(defn fail    [a] (state! a :failed))
(defn die     [a] (state! a :dead))

(defn sleep   [a millis]
  (with-return a (Thread/sleep millis)))

(defn get-action [{:as a strategy :strategy}]
  (strategy a))

(defn execute [a action]
  (if (dead? a) a
      (try (state! a :running)
           (try (let [a1 (action a)]
                  (cond (active? a1) (state! a1 :idle)
                        :else a1))
                (catch Exception _ (state! a :failed)))
           (catch AssertionError _ a))))

(defn run [a]
  (execute a (get-action a)))

(defn file-size [{:as a file-size-atom :file-size-atom}]
  (deref file-size-atom))

(defn out-of-space-on-path? [{:as a :keys [path file total-size]}]
  (when (and path file total-size)
    (if (.exists file)
      (< (.getUsableSpace path) (- total-size (.length file)))
      (= (.getUsableSpace path) 0))))

(defn fully-loaded? [{:as a :keys [file total-size]}]
  (when (and file (.exists file) total-size)
    (<= total-size (.length file))))

(defn get-file [{:as a :keys [name path]}]
  (when (and name path)
    (idle (assoc a :file (new File path name)))))

(defn download [{:as a :keys []}])

(defn files-dsv-*-data-cod-ru-get-head
  [{:as a :keys [address name]}]
  (let [response (HTTP/HEAD address)]
    (cond (= (derefed response :status :code) 200)
          (let [content-length (derefed response :headers :content-length)
                content-disposition (derefed response :headers :content-disposition)
                filename (second (re-find #"; filename=\"(.*)\"" content-disposition))]
            (if-not (and content-length filename) (die a)
                    (idle (merge a
                                 {:total-size content-length}
                                 (when (not name) {:name filename})))))
          :else (fail a))))

(defn files-dsv-*-data-cod-ru-reflex-strategy
  [{:as a :keys [address name file path total-size]}]
  (cond (not address)                 die
        (not (and name total-size))   files-dsv-*-data-cod-ru-get-head
        (not file)                    get-file
        (or (out-of-space-on-path? a) (fully-loaded? a)) die
        :else                         download))

(defn data-cod-ru-parse-page [{:as a :keys [address]}])

(defn data-cod-ru-make-child-agent [{:as a :keys [link]}]
  (when (and link (env a))
    (let [child (make-download-agent link :environment (env a))]
      (if-not child (die a)
              (do (send-off child run)
                  (idle (assoc a :child child)))))))

(defn data-cod-ru-reflex-strategy
  [{:as a :keys [address link child]}]
  (cond (not (and address (env a))) die
        (not link)     data-cod-ru-parse-page
        (not child)    data-cod-ru-make-child-agent
        :else          die))

(defservice data-cod-ru
  :address-pattern #"http://[\w\-]*.data.cod.ru/\d+"
  :strategy data-cod-ru-reflex-strategy
  :body #(hash-map :address nil :link nil :child nil))

(defservice files3?-dsv-*-data-cod-ru
  :address-pattern #"http://files3?.dsv.*.data.cod.ru/.+"
  :strategy files-dsv-*-data-cod-ru-reflex-strategy
  :max-active-agents 1
  :body #(hash-map :address nil :name nil :file nil :path nil
                   :total-size nil :file-size-atom (atom nil)))

(defservice files2-dsv-*-data-cod-ru
  :address-pattern #"http://files2.dsv.*.data.cod.ru/.+"
  :strategy files-dsv-*-data-cod-ru-reflex-strategy
  :max-active-agents 1
  :body #(hash-map :address nil :name nil :file nil :path nil
                   :total-size nil :file-size-atom (atom nil)))
