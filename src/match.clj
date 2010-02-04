;;; -*- mode: clojure; coding: utf-8 -*-
;;; author: Roman Zaharov <zahardzhan@gmail.com>

(ns #^{:doc
       "Сопоставление с образцом.
  Подробнее можно глянуть в книге 'Paradigms of the artificial intelligence
  programming: case studies in common lisp'"
       :author "Роман Захаров"}
  match)

(in-ns 'match)

(defn default-matcher 
  "Дефолтный сопоставитель с образцом."
  [rule sample]
  (cond (fn? rule) (rule sample)
        (set? rule) (rule sample)
        (keyword? rule) (when (= rule sample) rule)
        (string? rule) (when (= rule sample) rule)
        (instance? java.util.regex.Pattern rule) (re-find rule sample)))

(defn match 
  "Находит среди правил первое правило, которому соответствует образец, и
   возвращает результат действия над этим правилом.
   Сопоставитель (matcher) сравнивает паттерн с образцом, и если
   в результате этого хоть что-то получается, то сравнение считается удачным.
        
   Обычно правила представлены в виде списка ((паттерн соответствие), ...)"

  [sample rules &
   [{:keys [matcher rule-pattern rule-response action]
     :or   {matcher default-matcher
            rule-pattern first
            rule-response second
            action #(if %1 %2)}}]]

  (some (fn [rule]
          (when-let [result (matcher (rule-pattern rule) sample)]
            (action result (rule-response rule))))
        rules))