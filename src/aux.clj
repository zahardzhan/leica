;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Вспомогательные функции."
       :author "Роман Захаров"}
  aux
  (:use match clojure.contrib.seq-utils)
  (:import (java.io File)))

;; Задержаные вычисления не печатаются
(defmethod print-method clojure.lang.Delay [x w]
  ((get-method print-method Object) x w))

(in-ns 'aux)

(defn empty-fn 
  ([] nil)
  ([x] nil)
  ([x y] nil)
  ([x y & z] nil))

(defn fn-and
  "Пересечение функций возвращает функцию, эквивалентную
  (fn [x] (and (f1 x) (f2 x) (f3 x) ...)"
  [f & fs]
  (if-not fs
    f
    (let [chain (apply fn-and fs)]
      (fn [x] (and (f x) (chain x))))))

(defn fn-or
  "Объединение функций, возвращает функцию, эквивалентную
  (fn [x] (or (f1 x) (f2 x) (f3 x) ...)"
  [f & fs]
  (if-not fs
    f
    (let [chain (apply fn-or fs)]
      (fn [x] (or (f x) (chain x))))))

(defn agent? [x]
  (instance? clojure.lang.Agent x))

(defn derefed
  "Разыменовывает аргумент, если его можно разыменовать."
  [x]
  (if (instance? clojure.lang.IDeref x) 
    (deref x)
    x))

(defmacro with-deref 
  "Код с разыменоваными идентификаторами ссылок/агентов/санок/обещаний внутри."
  [[ref & refs] & body] 
     (cond (not ref)        `(do ~@body)
           (not (seq refs)) `(let [~ref (derefed ~ref)]
                               (do ~@body))
           (seq refs)       `(let [~ref (derefed ~ref)]
                               (with-deref [~@refs] ~@body))))

(defn type-dispatch
  "Диспетчер по типу агента."
  [x & xs] (:type (derefed x)))

(defn agent-or-type-dispatch
  "Диспетчер по агенту (:agent) или по типу тела агента (:state)."
  [x & xs] (when x (if (agent? x) :agent (type-dispatch x))))

(defn same [f & xs]
  (apply = (map f xs)))

(defn file? [x]
  (instance? java.io.File x))

(defn push [coll x]
  (concat coll (list x)))

(defn next-after-when [pred? x xs]
  "Для коллекции уникальных элементов xs находит следующий за x
  элемент, соответствующий предикату pred?. Если после х таких нет,
  то берется первый соответствующий элемент с начала коллекции."
  (when (some pred? xs)
    (let [[before after] (split-with (partial not= x) xs)]
      (some #(when (pred? %) %) (rest (cycle (concat after before)))))))

(defn file-length [#^File file]
  (if (.exists file) (.length file) 0))

(defmulti join-paths (fn [p1 p2] [(class p1) (class p2)]))

(defmethod join-paths [String String] [p1 p2]
  (str (File. (File. p1) p2)))

(defmethod join-paths [File String] [p1 p2]
  (str (File. p1 p2)))

(defn move-file [#^File source-file #^File dest-dir]
  (let [#^File dest-file (File. dest-dir (.getName source-file))]
    (when (. source-file renameTo dest-file)
      dest-file)))

(defn transliterate [line]
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

(defn http-error-status-handler [status fatal not-fatal]
  (match status
         [[#{400  ;; Bad Request - неправильный запрос
             403  ;; Forbidden - нет доступа
             404  ;; Not Found - документ не найден
             405  ;; Method Not Allowed
             406  ;; None Acceptable
             409  ;; Conflict
             410} ;; Gone
           fatal]
          [#{408  ;; Request Timeout
             500  ;; Internal Server Error - внутренняя ошибка скрипта
             501  ;; Not Implemented
             502  ;; Bad Gateway
             503  ;; Service Unavailable
             504} ;; Gateway Timeout
           not-fatal]]))
