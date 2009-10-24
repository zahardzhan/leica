;;; leica.clj: многопоточная качалка для data.cod.ru и dsvload.net.

;; Роман Захаров
;; октябрь, 2009

;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:require [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck])
  (:use clojure.contrib.test-is)
  (:import (java.io File InputStream ByteArrayOutputStream
                    ByteArrayInputStream)
           (java.net HttpURLConnection InetAddress URI URL URLEncoder)
           (org.htmlparser Parser)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags LinkTag)
           (org.htmlparser.nodes TagNode)))

(in-ns 'leica)

(def *ping-timeout* 3000)

;;; AUX

(defn file-length [#^File file]
  (if (.exists file) (.length file) 0))

(defn join-paths [path1 path2]
  (str (File. (File. path1) path2)))

(defn parse-data.cod.ru-page [url]
  "Парсит страницу файла на датакоде."
  ;; spacep = re.compile('Вам доступно ([\d\.]+) (.+)')
  ;; value, unit = (match.group(1), match.group(2))
  ;; self.space = int(float(value) * \
  ;;                       rule.match(unit, {'ГБ': 1073741824,
  ;;                                         'МБ': 1048576,
  ;;                                         'КБ': 1024}))
  (let [name (atom nil)
        link (atom nil)      
        parser (Parser. url)
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
    (.visitAllNodesWith parser visitor)
    {:name @name :link (new URI @link)}))

(defn job-cod.ru-link-name [job]
  (when-let [#^URI address (job :address)]
    (let [parsed (leica/parse-data.cod.ru-page (str address))]
      (when (and (parsed :name) (parsed :link))
        (assoc job :name (parsed :name) :link (parsed :link))))))

(defn job-link [job]
  (when-let [#^URI address (job :address)]
    (assoc job :link (.toASCIIString address))))

(defn job-name [job]
  (when-let [#^URI link (job :link)]
    (assoc job :name (second (re-find #"/([^/]+)$"
                                      (.getPath link))))))

(defn job-tag [pattern job]
  (when-let [#^URI link (job :link)]
    (when-let [tag (if pattern 
                     (some (fn [pat] (if (re-find pat (str link))
                                       (str pat)))
                           (if (sequential? pattern) pattern [pattern]))
                     (.getHost link))]
      (assoc job :tag tag))))

(defn job-length [job]
  (when-let [#^URI link (job :link)]
    (assoc job :length (Integer/parseInt
                        (:content-length (ha/headers (ha/http-agent link)))))))

(defn job-file [dir job]
  (when-let [name (job :name)]
    (assoc job :file (new File (join-paths dir name)))))

(defn download [job]
  (when-let [#^URI link (job :link)]
    (when-let [#^File file (job :file)]
      (let [headers {"Range" (str "bytes=" (file-length file) "-")}
            loader
            (ha/http-agent link
                           :headers headers
                           :handler (fn [remote]
                                      (with-open [local (duck/writer file)]
                                        (duck/copy (ha/stream remote) local))))]
        (ha/result loader)
        (ha/done? loader)))))

;;; RULE

(defn default-matcher [rule sample]
  "Дефолтный сопоставитель с образцом."
  (cond (fn? rule) (rule sample)
        (string? rule) (if (= rule sample) rule)
        (= (type rule) java.util.regex.Pattern) (re-find rule sample)))

(defn match [sample rules &
             [{:keys [matcher rule-pattern rule-response action]
               :or   {matcher default-matcher
                      rule-pattern first
                      rule-response second
                      action #(if %1 %2)}}]]
  "Находит среди правил первое правило, которому соответствует образец, и
   возвращает результат действия над этим правилом.
   Сопоставитель (matcher) сравнивает паттерн с образцом, и если
   в результате этого хоть что-то получается, то сравнение считается удачным.
        
   Обычно правила (rules) представлены в виде списка:
        ((паттерн, соответствие),
         (паттерн, соответствие),
         ...)"
  (some (fn [rule]
          (when-let [result (matcher (rule-pattern rule) sample)]
            (action result (rule-response rule))))
        rules))

;;; TESTING

;; (ha/string (ha/http-agent "http://data.cod.ru"))

;; (def agents (for [ip ip-list] (agent ip)))

;; (defn ping [hostname]
;;    (let [status (.isReachable (InetAddress/getByName hostname) *timeout*)]
;;       (if status (dosync (commute *up* conj hostname)))))

;; (defn ping-all []
;;    (doseq [ip-agent agents]
;;       (send-off ip-agent ping)))

;; (ping-all)
;; (apply await agents)

;; (doseq [host (filter deref agents)]
;;   (println (str @host " is up")))

(defstruct environment :agents :state)
;;    The world in which agents exist.

;; Агент это что-то, что воспринимает свое окружение и действует.
;; У агента есть тело, которое хранит состояние, и программа,
;; которая выбирает действие, основываясь на состоянии агента и 
;; восприятии окружения. Действие агента возвращается обратно в окружение
;; для его выполнения.

(defstruct agere 
  :name 
  :program 
  ;; слот программы агента, содержит функцию одного аргумента,
  ;; результата восприятия окружения, и возвращает действие.
  ;; Представление восприятия и действия зависит от конкретного
  ;; окружения и агента.
  :percept :action :alive)

;; Агент для скачивания файла.

(defstruct job
  :address ;; адрес задания.
  :actions ;; набор функций-действий агента.
  :link    ;; прямая ссылка на файл.
  :tag     ;; идентификатор по которому разделяются потоки загрузок
  :name    ;; имя файла.
  :path    ;; путь файла.
  :length  ;; размер файла.
  :name :program :percept :action :alive)

(defn reflex-job-program [percept]
  "Простая рефлексная программа для загрузочного агента."
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
         [[(missing :address) :rip]
          [(missing :actions) :rip]
          [(missing :link)    :obtain-link]
          [(missing :tag)     :obtain-tag]
          [(missing :name)    :obtain-name]
          [(missing :path)    :obtain-path]
          [(missing :length)  :obtain-length]
          [fully-loaded       :rip]
          [out-of-space       :rip]
          [otherwise          :download]])))

(defn make-job [line rules]
  "Конструктор агента из строки с адресом и набора правил."
  (when (and line rules)
    (when-let [[address actions]
               (match line rules {:matcher re-find :action list})]
      (struct-map job :program reflex-job-program
                  :address address
                  :actions actions))))

(defn run-environment [env] nil)

;;    Basic environment simulator. It gives each agent its percept, gets an action from each agent, and updates the environment. It also keeps score for each agent, and optionally displays intermediate results. [p 48]

;; Generic Functions that must be defined for each environment
;; For each new type of environment you want to define, you will need a defstructure that inherits from (includes) ENVIRONMENT, and you will need to write new methods (or inherit existing methods) for each of the following eight functions. Here are the ones that will change for each new environment:

(defmulti percept 
  "[agent env] Return the percept for this agent."
  identity)

(defmulti update-fn 
  "[env] Modify the environment, based on agents actions, etc."
  identity)

(defmulti legal-actions
  "[env] A list of the action operators that an agent can do."
  identity)

(defmulti termination?
  "[env] Return true if the simulation should end now."
  identity)

(defmulti display-environment
  "[env] Display the current state of the environment."
  identity)

(defmulti execute-agent-actions
  "[env] Each agent (if the agent is alive and has specified a legal action) takes the action."
  identity)

;;; TEST

;; Хосты упорядочены от частного к общему
(def job-rules
     [[#"http://dsv.data.cod.ru/\d{6}"
       {:obtain-link leica/job-cod.ru-link-name
        :obtain-tag  (partial leica/job-tag [#"files3?.dsv.data.cod.ru"
                                       #"files2.dsv.data.cod.ru"])
        :obtain-path (partial leica/job-file "/home/haru/inbox/dsv")
        :obtain-length leica/job-length
        :download    leica/download}]
      [#"http://[\w\.]*data.cod.ru/\d+"
       {:obtain-link leica/job-cod.ru-link-name
        :obtain-tag  (partial leica/job-tag [#"files3?.dsv.data.cod.ru"
                                       #"files2.dsv.data.cod.ru"])
        :obtain-path (partial leica/job-file "/home/haru/inbox/dsv")
        :obtain-length leica/job-length
        :download    leica/download}]
      [#"http://77.35.112.8[1234]/.+"
       {:obtain-link leica/job-link
        :obtain-name leica/job-name
        :obtain-tag  (partial leica/job-tag nil)
        :obtain-path (partial leica/job-file "/home/haru/inbox/dsv")
        :obtain-length leica/job-length
        :download    leica/download}]
      [#"http://dsvload.net/ftpupload/.+"
       {:obtain-link leica/job-link
        :obtain-name leica/job-name
        :obtain-tag  (partial leica/job-tag nil)
        :obtain-path (partial leica/job-file "/home/haru/inbox/dsv")
        :obtain-length leica/job-length
        :download    leica/download}]])

(let [jj {:link (URI. "http://files3.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
          :name "Hayate_the_combat_butler.mkv"
          :address (URI. "http://dsv.data.cod.ru/433148")}]
  (deftest test-tag
    (is (= (:tag (leica/job-tag nil jj))
           "files3.dsv.data.cod.ru"))
    (is (= (:tag (leica/job-tag #"files3?.dsv.data.cod.ru" jj))
           "files3?.dsv.data.cod.ru"))
    (is (= (:tag (leica/job-tag [#"files3?.dsv.data.cod.ru"
                                 #"files2.dsv.data.cod.ru"] jj))
           "files3?.dsv.data.cod.ru"))))

(deftest test-match
  (is (= (match "http://dsv.data.cod.ru/433148"
                '((#"http://dsv.data.cod.ru/\d{6}" :MATCH))
                {:rule-response rest})
         '(:MATCH))))


