;;; -*- mode: clojure; coding: utf-8 -*-
;;; authors: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc "Базовые действия агента."
       :author "Роман Захаров"}
  action
  (:require [clojure.contrib.logging :as log])
  (:use aux match env))

(in-ns 'action)

(defn execute 
  "Возвращает новое тело агента - результат выполнения агентом действия.
  Действие выполняется в отдельном потоке, сам агент не изменяется."
  [ag action]
  (with-deref [ag]
    (let [fag (future (assoc (((ag :actions) action) ag)
                        :action (if (#{:die :fail :pass} action)
                                  (ag :action)
                                  action)))]
      (log/debug (str (or (ag :name) (ag :address)) \space action))
      (with-deref [fag]
        (log/debug (str (or (fag :name) (fag :address)) \space action \space 
                        (cond (dead? fag) "агент умер"
                              (fail? fag) "агент провалился"
                              :else       "успешно")))
        fag))))

(defn percept
  "Возвращает действие - результат восприятия программой агента тела агента,
  окружения и самого `восприятия'. Восприятие передается программе агента в 
  виде хэша."
  ([ag] (percept ag {}))
  ([ag perception] (with-deref [ag] ((ag :program) ag perception))))

(defn percept-and-execute
  "Агент выполняет действие, основываясь на восприятии."
  ([ag] (percept-and-execute ag {}))
  ([ag perception] (with-deref [ag]
                     (let [action (percept ag perception)]
                       (execute ag action)))))

(defn after
  "Возвращает true, если последнее действие агента совпадает с аргументом.
  В качестве статуса `status' можно указать ключи:
  :successful - удачное завершение последнего действия и
  :failed - неудачное. "
  ([action ag] (with-deref [ag] (= action (:action ag))))
  ([status action ag] 
     (with-deref [ag]
       (and (after action ag)
            (case status
                  :successful ((no fail?) ag)
                  :failed     (fail? ag))))))

(defn pass "Пустое действие - возвращается неизменное тело агента."
  [ag] ag)
 
(defn fail "Неудачное действие - возвращается тело агента с маркером неудачи."
  [ag] (assoc ag :fail true))
 
(defn die "Смерть агента - возвращается тело агента с маркером смерти."
  [ag] (assoc ag :alive false))