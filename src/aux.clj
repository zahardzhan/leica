;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns aux
  (:require fn)
  (:import java.io.File))

(in-ns 'aux)

;; Задержаные вычисления не печатаются
(defmethod print-method clojure.lang.Delay [x w]
  ((get-method print-method Object) x w))

(def agent? (partial instance? clojure.lang.Agent))

(def ref? (partial instance? clojure.lang.Ref))

(defn multimethod? [x]
  (= (type x) clojure.lang.MultiFn))

(defn invocable? [x]
  (instance? clojure.lang.IFn x))

(defn swap-args [f]
  (fn [x y] ( f y x)))

(defn same [f & xs]
  (apply = (map f xs)))

(defn before [x xs]
  (take-while (partial not= x) xs))

(defn after [x xs]
  (next (drop-while (partial not= x) xs)))

(defmacro when-supplied [& clauses]
  (if (not clauses) true
      `(and (or (nil? ~(first clauses))
                (do ~(second clauses)))
            (when-supplied ~@(next (next clauses))))))

(defn select-items [& {:as opts
                       :keys [from order-by where entirely-after after before]
                       :or {where identity}}]

  {:pre  [(when-supplied from (coll? from)
                         order-by (or (keyword? order-by) (fn? order-by) (multimethod? order-by))
                         where (or (multimethod? where) (fn? where)))]}
  
  (let [maybe-sort (fn [xs] (if order-by (sort-by order-by xs) xs))
        maybe-take-part-of-seq (fn [xs]
                                 (cond (not (or entirely-after after before)) xs
                                       entirely-after (concat (aux/after entirely-after xs)
                                                              (aux/before entirely-after xs)
                                                              (list entirely-after))
                                       after (aux/after after xs)
                                       before (aux/before before xs)))
        maybe-filter (fn [xs] (if where (filter where xs) xs))]
    (when from
      (->> from
           maybe-sort
           maybe-take-part-of-seq
           maybe-filter))))

(defn rule-based-translator
  "Find the first rule that matches input, and apply the
  action to the result of substituting the match result
  into the rule's response. If no rule matches, apply
  otherwise to the input."
  [input rules &
   {:as opts
    :keys [matcher rule-pattern rule-response fail?
           action otherwise]
    :or {matcher =
         rule-pattern first
         rule-response rest
         fail? (fn/or nil? false?)
         action list
         otherwise identity}}]

  (or (first (for [rule rules
                   :let [result (matcher (rule-pattern rule) input)]
                   :when (not (fail? result))]
               (action result (rule-response rule))))
      (otherwise input)))

(defn extract-url [line]
  (first (re-find #"((https?|ftp|gopher|telnet|file|notes|ms-help):((//)|(\\\\))+[\w\d:#@%/;$()~_?\+-=\\\.&]*)"
                  line)))

(defn derefed
  "Разыменовывает аргумент."
  ([] nil)
  ([x] (if (instance? clojure.lang.IDeref x) (deref x) x))
  ([x f] (f (derefed x)))
  ([x f & fs] ((apply comp (reverse fs)) (derefed x f))))

(defn deref-seq [coll]
  (map derefed coll))

(defmacro with-deref 
  "Разыменовывает и захватывает идентификаторы ссылок/агентов/санок/обещаний, и
  выполняет код тела макроса."
  [[ref & refs] & body]
  (if ref
    `(let [~ref (derefed ~ref)]
       (with-deref ~refs ~@body))
    `(do ~@body)))

(defmacro with-return
  [expr & body]
  `(do (do ~@body)
       ~expr))

(defmacro let-return 
  [[form val] & body]
  `(let [~form ~val]
     (with-return ~form (do ~@body))))

(defn dispatch-by-type
    ([x] (type x))
    ([x & args] (type x)))

(defn dispatch-by-derefed-type
  ([x] (derefed x type))
  ([x & args] (derefed x type)))

(defn dispatch-by-type-of-2-args
  ([x y] [(type x) (type y)])
  ([x y & args] [(type x) (type y)]))

(defn dispatch-by-derefed-type-of-2-args
  ([x y] [(derefed x type) (derefed y type)])
  ([x y & args] [(derefed x type) (derefed y type)]))

(defn file? [x]
  (and (instance? java.io.File x) (.isFile x)))

(defn directory? [x] 
  (and (instance? java.io.File x) (.isDirectory x)))

(defn files-in-directory
  "Возвращает содержимое директории - список файлов."
  [x] 
  (and (directory? x) (seq (.listFiles x))))

(defn file-length [#^File file]
  (if (.exists file) (.length file) 0))

(defmulti join-paths
  "Склеивает пути файлов."
  {:arglists '([file-path-1 file-path-2])}
  (fn [p1 p2] [(class p1) (class p2)]))

(defmethod join-paths [String String] [p1 p2] (File. (File. p1) p2))
(defmethod join-paths [File String] [p1 p2] (File. p1 p2))
(defmethod join-paths [File File] [p1 p2] (File. p1 (.getName p2)))

(defn move-file
  "Перемещает файл в указанную директорию."
  [#^File source-file #^File dest-dir]
  (let [#^File dest-file (join-paths dest-dir source-file)]
    (when (. source-file renameTo dest-file)
      dest-file)))

(defn transliterate 
  "Транслитерация строки с русского на английский."
  [line]
  (let [table {"а" "a"  "б" "b"  "в" "v"  "г" "g"  "д" "d"
               "е" "e"  "ё" "yo"  "ж" "zh"  "з" "z"  "и" "i"
               "й" "y"  "к" "k"  "л" "l"  "м" "m"  "н" "n"
               "о" "o"  "п" "p"  "р" "r"  "с" "s"  "т" "t"
               "у" "u"  "ф" "f"  "х" "h"  "ц" "c"  "ч" "ch"
               "ш" "sh"  "щ" "sh"  "ъ" "'"  "ы" "y"  "ь" "'"
               "э" "e"  "ю" "yu"  "я" " ya"  "А" "A"  "Б" "B"
               "В" "V"  "Г" "G"  "Д" "D"  "Е" "E"  "Ё" "YO"
               "Ж" "ZH"  "З" "Z"  "И" "I"  "Й" "Y"  "К" "K"
               "Л" "L"  "М" "M"  "Н" "N"  "О" "O"  "П" "P"
               "Р" "R"  "С" "S"  "Т" "T"  "У" "U"  "Ф" "F"
               "Х" "H"  "Ц" "C"  "Ч" "CH"  "Ш" "SH"  "Щ" "SH"
               "Ъ" "'"  "Ы" "Y"  "Ь" "'"  "Э" "E"  "Ю" "YU" "Я" " YA"}]
    (apply str (map (fn [char] (or (table (str char)) char)) line))))

(defn format-link-for-forum [name address]
  (str "[b]" name "[/b]: [url=" address "]" address "[/url]" \newline))
