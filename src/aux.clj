;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns aux
  (:use clojure.contrib.seq-utils)
  (:import java.io.File))

(in-ns 'aux)

;; Задержаные вычисления не печатаются
(defmethod print-method clojure.lang.Delay [x w]
  ((get-method print-method Object) x w))

(def agent? (partial instance? clojure.lang.Agent))

(def ref? (partial instance? clojure.lang.Ref))

(defn same [f & xs]
  (apply = (map f xs)))

(defn next-after-when
  "Для коллекции уникальных элементов xs находит следующий за x
  элемент, соответствующий предикату pred?. Если после х таких нет,
  то берется первый соответствующий элемент с начала коллекции."
  [pred? x xs]
  (when (some pred? xs)
    (let [[before after] (split-with (partial not= x) xs)]
      (some #(when (pred? %) %) (rest (cycle (concat after before)))))))

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

(defmacro when-supplied [arg & body]
  `(or (nil? ~arg) (do ~@body)))

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
