;;; leica.clj: многопоточная качалка для data.cod.ru и dsvload.net.

;; Роман Захаров
;; октябрь, 2009

;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:require [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck])
  (:use clojure.contrib.test-is clojure.contrib.seq-utils)
  (:import (java.io File FileOutputStream InputStream
                    ByteArrayOutputStream ByteArrayInputStream)
           (java.net HttpURLConnection InetAddress URI URL URLEncoder)
           (org.htmlparser Parser)
           (org.htmlparser.util ParserException)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags LinkTag)
           (org.htmlparser.nodes TagNode)))

(in-ns 'leica)

(def *ping-timeout* 3000)

;;; RULE

(defn default-matcher 
  "Дефолтный сопоставитель с образцом."
  [rule sample]
  (cond (fn? rule) (rule sample)
        (set? rule) (rule sample)
        (string? rule) (if (= rule sample) rule)
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

;;; AUX

(defn push [coll x] (concat coll (list x)))

(defn file-length [#^File file]
  (if (.exists file) (.length file) 0))

(defn join-paths [path1 path2]
  (str (File. (File. path1) path2)))

(defn http-error-status-handler [http-agent fatal not-fatal]
  (match (ha/status http-agent)
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

(defn parse-datacodru-page
  "Парсит текст страницы файла на датакоде."
  [page]
  ;; TODO: нужно разобраться с парсером, ибо он необъятный
  ;; spacep = re.compile('Вам доступно ([\d\.]+) (.+)')
  ;; value, unit = (match.group(1), match.group(2))
  ;; self.space = int(float(value) * \
  ;;                       rule.match(unit, {'ГБ': 1073741824,
  ;;                                         'МБ': 1048576,
  ;;                                         'КБ': 1024}))
  (let [name (atom nil)
        link (atom nil)      
        parser (Parser/createParser page nil)
        visitor
        (proxy [NodeVisitor] []
          (visitTag
           [tag]
           (cond (instance? LinkTag tag)
                 (when-let [the-link (re-find #"http://files[\d\w\.]*data.cod.ru/.+"
                                              (.extractLink tag))]
                   (reset! link the-link))
                 (and (instance? TagNode tag) (= "b" (.getRawTagName tag)))
                 (when-let [the-name (.getAttribute tag "title")]
                   (reset! name the-name)))))]
    (try (.visitAllNodesWith parser visitor)
         {:name @name :link (when @link (new URI @link))}
         (catch ParserException _ nil))))

(defn job-die [job]
  (assoc job :alive false))

(defn job-datacodru-link-name [job]
  (when-let [#^URI address (job :address)]
    (let [page-request (ha/http-agent address)]
      (ha/result page-request)
      (if (and (ha/done? page-request) (ha/success? page-request))
        (let [parsed (parse-datacodru-page (ha/string page-request))]
          (if (and (parsed :name) (parsed :link))
            (assoc job :name (parsed :name) :link (parsed :link))
            (job-die job)))
        ((http-error-status-handler page-request
                                    job-die identity) job)))))

(defn job-link [job]
  (when-let [#^URI address (job :address)]
    (assoc job :link (.toASCIIString address))))

(defn job-name [job]
  (when-let [#^URI link (job :link)]
    (assoc job :name (second (re-find #"/([^/]+)$" (.getPath link))))))

(defn job-tag [pattern job]
  (when-let [#^URI link (job :link)]
    (when-let [tag (or (if pattern 
                         (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                               (if (sequential? pattern) pattern [pattern])))
                       (.getHost link))]
      (assoc job :tag tag))))

(defn job-length [job]
  (when-let [#^URI link (job :link)]
    (let [length-request (ha/http-agent link :method "HEAD")]
      (ha/result length-request)
      (if (and (ha/done? length-request) (ha/success? length-request))
        (assoc job :length (Integer/parseInt
                            (:content-length (ha/headers length-request))))
        ((http-error-status-handler length-request
                                    job-die identity) job)))))

(defn job-file [dir job]
  (when-let [name (job :name)]
    (assoc job :file (new File (join-paths dir name)))))

(defn download [job]
  (when-let [#^URI link (job :link)]
    (when-let [#^File file (job :file)]
      (let [headers {"Range" (str "bytes=" (file-length file) "-")}
            loader (ha/http-agent link
                                  :headers headers
                                  :handler
                                  (fn [remote]
                                    (with-open [local (FileOutputStream. file)]
                                      (duck/copy (ha/stream remote) local))))]
        (ha/result loader)
        (if (and (ha/done? loader) (ha/success? loader))
          (job-die job)
          ((http-error-status-handler loader
                                      job-die identity) job))))))

;;; AGENCY

(defn reflex-download-program
  "Простая рефлексная программа агента для скачивания."
  [percept]
  (letfn [(out-of-space
           [percept]
           (when-let [#^File file ((percept :self) :file)]
             (< (.getUsableSpace file) (file-length file))))
          (fully-loaded
           [percept]
           (<= ((percept :self) :length) (file-length ((percept :self) :file))))
          (missing [key] (fn [percept] (not ((percept :self) key))))
          (otherwise [_] true)]
  (match percept
         [[(missing :address) :die]
          [(missing :actions) :die]
          [(missing :link)    :obtain-link]
          [(missing :tag)     :obtain-tag]
          [(missing :name)    :obtain-name]
          [(missing :file)    :obtain-file]
          [(missing :length)  :obtain-length]
          [fully-loaded       :die]
          [out-of-space       :die]
          [otherwise          :download]])))

(defn download-agent 
  "Агент для скачивания.

  Агент это что-то, что воспринимает свое окружение и действует.
  У агента есть тело, которое хранит состояние, и программа,
  которая выбирает действие, основываясь на состоянии агента и 
  восприятии окружения. Действие агента возвращается обратно в окружение
  для его выполнения. Представление восприятия и действия зависит от конкретного
  окружения и агента.

  :name    имя агента
  :program слот программы агента, содержит функцию одного аргумента,
           результата восприятия окружения, и возвращает действие
  :actions набор функций-действий агента
  :percept последнее восприятие
  :action  последнее действие
  :alive   определяет жив агент или умер

  :address адрес задания
  :link    прямая ссылка на файл, который нужно скачать
  :tag     идентификатор по которому разделяются потоки загрузок
  :file    файл в который сохраняется скачанное
  :length  размер файла, что нужно скачать"
  
  [line rules]
  (let [[address actions] (match line rules {:action list})]
    (when (and address actions)
      (agent {:address (URI. address)
              :link nil :name nil :tag nil :file nil :length nil
              :actions actions
              :program reflex-download-program
              :alive true :percept nil :action nil}))))

(defn alive?
  "Жив ли агент?"
  [ag] (:alive @ag))

(defn dead?
  "Мертв ли агент?"
  [ag] (not (:alive @ag)))

(defn tag [ag] (:tag @ag))

(defn environment []
  (ref {:agents '() :tags {}}))

(defn add-agent [env ag]
  (assoc env :agents (push (env :agents) ag)))

(defn add-tag [env tag]
  (if (contains? (env :tags) tag) env
      (assoc env :tags (assoc (env :tags) tag (atom false)))))

(defn tag-locked? [env tag]
  (when (contains? (@env :tags) tag)
    @((@env :tags) tag)))

(defn tag-lock [env tag]
  (when (contains? (@env :tags) tag)
    (reset! ((@env :tags) tag) true)))

(defn tag-unlock [env tag]
  (when (contains? (@env :tags) tag)
    (reset! ((@env :tags) tag) false)))

(defn act
  "Агент воспринимает окружение и действует."
  [ag env]
  (let [tag (:tag ag)]
    (cond (dead? *agent*) ag
          
          (not tag)
          (do (let [result (atom nil)
                    thread (Thread.
                            #(let [percept {:self ag}
                                   action  ((ag :program) percept)
                                   new-ag-state (((ag :actions) action) ag)]
                               (reset! result new-ag-state)))]
                (.start thread)
                (.join thread)
                (when-let [tag (:tag @result)]
                  (when-not (contains? (@env :tags) tag)
                    (dosync (alter env add-tag tag))))
                (send-off *agent* act env)
                @result))

          (and tag (tag-locked? env tag))
          (do (Thread/sleep 1000)
              (send-off *agent* act env)
              ag)

          (and tag (not (tag-locked? env tag)))
          (do (tag-lock env tag)
              (let [result (atom nil)
                    thread (Thread.
                            #(let [percept {:self ag} ;; :obtain-tag
                                   action  ((ag :program) percept)
                                   new-ag-state (((ag :actions) action) ag)]
                               (reset! result new-ag-state)))]
                (.start thread)
                (.join thread)
                (tag-unlock env tag)
                (send-off *agent* act env)
                @result)))))

(def #^{:doc "Хосты упорядочены от частного к общему."}
     job-rules
     [[#"http://dsv.data.cod.ru/\d{6}"
       {:obtain-link job-datacodru-link-name
        :obtain-tag  (partial job-tag [#"files3?.dsv.data.cod.ru"
                                       #"files2.dsv.data.cod.ru"])
        :obtain-file (partial job-file "/home/haru/inbox/dsv")
        :obtain-length job-length
        :download    download
        :die job-die}]
      [#"http://[\w\.]*data.cod.ru/\d+"
       {:obtain-link job-datacodru-link-name
        :obtain-tag  (partial job-tag nil)
        :obtain-file (partial job-file "/home/haru/inbox/dsv")
        :obtain-length job-length
        :download    download}]
      [#"http://77.35.112.8[1234]/.+"
       {:obtain-link job-link
        :obtain-name job-name
        :obtain-tag  (partial job-tag nil)
        :obtain-file (partial job-file "/home/haru/inbox/dsv")
        :obtain-length job-length
        :download    download}]
      [#"http://dsvload.net/ftpupload/.+"
       {:obtain-link job-link
        :obtain-name job-name
        :obtain-tag  (partial job-tag nil)
        :obtain-file (partial job-file "/home/haru/inbox/dsv")
        :obtain-length job-length
        :download    download}]])

;;; TESTS

(let [jj {:link (URI. "http://files3.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
          :name "Hayate_the_combat_butler.mkv"
          :address (URI. "http://dsv.data.cod.ru/433148")}
      jk {:link (URI. "http://files4.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
          :name "Hayate_the_combat_butler.mkv"
          :address (URI. "http://dsv.data.cod.ru/433148")}]
  (deftest test-tag
    (is (= (:tag (leica/job-tag nil jj))
           "files3.dsv.data.cod.ru"))
    (is (= (:tag (leica/job-tag #"files3?.dsv.data.cod.ru" jj))
           "files3?.dsv.data.cod.ru"))
    (is (= (:tag (leica/job-tag [#"files3?.dsv.data.cod.ru"
                                 #"files2.dsv.data.cod.ru"] jj))
           "files3?.dsv.data.cod.ru"))
    (is (= (:tag (job-tag [#"files3?.dsv.data.cod.ru"
                           #"files2.dsv.data.cod.ru"] jk))
           "files4.dsv.data.cod.ru"))))

(deftest test-match
  (is (= (match "http://dsv.data.cod.ru/433148"
                '((#"http://dsv.data.cod.ru/\d{6}" :MATCH))
                {:rule-response rest})
         '(:MATCH))))

(let [jobs [(download-agent "http://dsv.data.cod.ru/425812" job-rules)
            (download-agent "gold http://dsv.data.cod.ru/443824" job-rules)
            (download-agent "http://dsv.data.cod.ru/451185" job-rules)]]
  (def j0 (jobs 0))
  (def j1 (jobs 1))
  (def j2 (jobs 2))
  (def e (environment))
  (dosync (alter e add-agent j0)
          (alter e add-agent j1)
          (alter e add-agent j2)))

;; (do (send-off j0 act e)
;;     (send-off j1 act e)
;;     (send-off j2 act e))