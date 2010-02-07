;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Вспомогательные функции."
       :author "Роман Захаров"}
  aux
  (:use clojure.contrib.seq-utils)
  (:import java.io.File))

;; Задержаные вычисления не печатаются
(defmethod print-method clojure.lang.Delay [x w]
  ((get-method print-method Object) x w))

(in-ns 'aux)

(defn empty-fn
  "Пустая функция, всегда возвращает nil."
  ([] nil)
  ([x] nil)
  ([x y] nil)
  ([x y & z] nil))

(defn no
  "Более функциональная альтернатива функции complement.

  Например: (no f) => (complement f)
            (no f x y ...) => ((complement f) x y ...)"
  ([f] (complement f))
  ([f & xs] (apply (no f) xs)))

(defn fn-and
  "Пересечение функций возвращает функцию эквивалентную 
  (fn [x ...] (and (f x ...) (g x ...) ...))"
  [f & fs]
  (if-not fs
    f
    (let [chain (apply fn-and fs)]
      (fn [& xs] (and (apply f xs) (apply chain xs))))))

(defn fn-or
  "Объединение функций возвращает функцию эквивалентную
  (fn [x ...] (or (f x ...) (g x ...) ...))"
  [f & fs]
  (if-not fs
    f
    (let [chain (apply fn-or fs)]
      (fn [& xs] (or (apply f xs) (apply chain xs))))))

(defn agent? "Является ли аргумент агентом языка Clojure."
  [x] (instance? clojure.lang.Agent x))

(defn derefed "Разыменовывает аргумент."
  [x] (if (instance? clojure.lang.IDeref x) 
        (deref x)
        x))

(defmacro with-deref 
  "Разыменовывает и захватывает идентификаторы ссылок/агентов/санок/обещаний, и
  выполняет код тела макроса."
  [[ref & refs] & body]
  (if ref
    `(let [~ref (derefed ~ref)]
       (with-deref ~refs ~@body))
    `(do ~@body)))

(defn agent-dispatch
  "Диспетчер возвращает ключ :agent, если первый аргумент - агент."
  ([] nil)
  ([a & xs] (when (agent? a) :agent)))

(defn type-dispatch
  "Диспетчер возвращает тип первого аргумента.
  Для хэша тип - значение для ключа :type. Если аргумент ссылка - разыменовывает."
  ([] nil)
  ([x & xs] (:type (derefed x))))

(defn same
  "Возвращает true, если функция f для всех аргументов xs возвращает
  одинаковое значение.

  Например: (same even? [2 4 6])
            > true"
  [f & xs]
  (apply = (map f xs)))

(defn file? "Является ли аргумент файлом (не директорией)?"
  [x] (and (instance? java.io.File x) (.isFile x)))

(defn directory? "Является ли аргумент директорией?"
  [x] (and (instance? java.io.File x) (.isDirectory x)))

(defn list-files "Возвращает содержимое директории - список файлов."
  [x] (and (directory? x) (seq (.listFiles x))))

(defn push "Запихивает аргумент в конец списка."
  [coll x] (concat coll (list x)))

(defn next-after-when
  "Для коллекции уникальных элементов xs находит следующий за x
  элемент, соответствующий предикату pred?. Если после х таких нет,
  то берется первый соответствующий элемент с начала коллекции."
  [pred? x xs]
  (when (some pred? xs)
    (let [[before after] (split-with (partial not= x) xs)]
      (some #(when (pred? %) %) (rest (cycle (concat after before)))))))

(defn file-length "Длина файла."
  [#^File file] (if (.exists file) (.length file) 0))

(defmulti join-paths "Склеивает пути файлов."
  {:arglists '([file-path-1 file-path-2])}
  (fn [p1 p2] [(class p1) (class p2)]))

(defmethod join-paths [String String] [p1 p2] (File. (File. p1) p2))
(defmethod join-paths [File String] [p1 p2] (File. p1 p2))
(defmethod join-paths [File File] [p1 p2] (File. p1 (.getName p2)))

(defn move-file "Перемещает файл в указанную директорию."
  [#^File source-file #^File dest-dir]
  (let [#^File dest-file (join-paths dest-dir source-file)]
    (when (. source-file renameTo dest-file)
      dest-file)))

(defn transliterate "Транслитерация строки с русского на английский."
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

(defn http-error-status-handler [status fatal not-fatal]
  (case status
        400 fatal     ;; Bad Request - неправильный запрос
        403 fatal     ;; Forbidden - нет доступа
        404 fatal     ;; Not Found - документ не найден
        405 fatal     ;; Method Not Allowed
        406 fatal     ;; None Acceptable
        409 fatal     ;; Conflict
        410 fatal     ;; Gone
        408 not-fatal ;; Request Timeout
        500 not-fatal ;; Internal Server Error - внутренняя ошибка скрипта
        501 not-fatal ;; Not Implemented
        502 not-fatal ;; Bad Gateway
        503 not-fatal ;; Service Unavailable
        504 not-fatal ;; Gateway Timeout
        ))