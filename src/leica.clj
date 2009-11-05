;;; -*- mode: lisp; coding: utf-8 -*- author: Roman Zaharov zahardzhan@gmail.com
;;; leica.clj: многопоточная качалка для data.cod.ru и dsvload.net.

;; октябрь, 2009

;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:gen-class)
  (:require [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck]
            [clojure.contrib.logging :as log])
  (:use clojure.contrib.test-is clojure.contrib.seq-utils
        clojure.contrib.command-line
        clojure.contrib.json.write)
  (:import (java.io File FileOutputStream InputStream
                    ByteArrayOutputStream ByteArrayInputStream)
           (java.net HttpURLConnection InetAddress URI URL URLEncoder)
           (java.util Date)
           (java.util.logging Logger Level Formatter LogRecord StreamHandler)
           (org.apache.commons.httpclient HttpClient)
           (org.apache.commons.httpclient.methods GetMethod PostMethod)
           (org.apache.commons.httpclient.util EncodingUtil)
           (org.htmlparser Parser)
           (org.htmlparser.util ParserException)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags Div LinkTag)
           (org.htmlparser.nodes TagNode)))

(in-ns 'leica)

(def *usage* 
     "Использование:
leica [КЛЮЧИ] [ФАЙЛ С АДРЕСАМИ] [ЗАГРУЗОЧНАЯ ДИРЕКТОРИЯ]
АДРЕСА | leica [КЛЮЧИ] [ЗАГРУЗОЧНАЯ ДИРЕКТОРИЯ]
 
Качает файлы с датакода. По-умолчанию в текущую директорию.

Пишите о багах на zahardzhan@gmail.com.")

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

(defn agent? [x]
  (instance? clojure.lang.Agent x))

(defn push [coll x]
  (concat coll (list x)))

(defn next-after-when [pred? x xs]
  (when (some pred? xs)
    (let [[before after] (split-with (partial not= x) xs)]
      (some #(when (pred? %) %) (rest (cycle (concat after before)))))))

(defn file-length [#^File file]
  (if (.exists file) (.length file) 0))

(defmulti join-paths (fn [p1 p2] [(class p1) (class p2)]))

(defmethod join-paths [java.lang.String java.lang.String] [p1 p2]
  (str (File. (File. p1) p2)))

(defmethod join-paths [java.io.File java.lang.String] [p1 p2]
  (str (File. p1 p2)))

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
  (let [name (atom nil)
        link (atom nil)
        space (atom nil)
        parser (Parser/createParser page nil)
        visitor
        (proxy [NodeVisitor] []
          (visitTag
           [tag]
           (cond (instance? LinkTag tag)
                 (when-let [parsed-link (re-find #"http://files[\d\w\.]*data.cod.ru/.+"
                                              (.extractLink tag))]
                   (reset! link parsed-link))

                 (and (instance? TagNode tag) (= "b" (.getRawTagName tag)))
                 (when-let [parsed-name (.getAttribute tag "title")]
                   (reset! name parsed-name))

                 (instance? Div tag)
                 (let [[_ space-str unit-str]
                       (re-find #"Вам доступно ([\d\.]+) (\p{javaUpperCase}{2})" 
                                (.getStringText tag))]
                   (when (and space-str unit-str)
                     (let [space-val (Float/parseFloat space-str)
                           unit-val ({"ГБ" 1073741824 "МБ" 1048576 "КБ" 1024} unit-str)]
                       (reset! space (int (* space-val unit-val)))))))))]
    (try (.visitAllNodesWith parser visitor)
         {:name @name :link (when @link (new URI @link)) :space @space}
         (catch ParserException _ nil))))

(defn job-nop [job env]
  job)

(defn job-fail [job env]
  (assoc job :fail true))

(defn job-die [job env]
  (assoc job :alive false))

(defn job-datacodru-link-name [job env]
  (when-let [#^URI address (job :address)]
    (let [page-request (ha/http-agent address)]
      (ha/result page-request)
      (if (and (ha/done? page-request) (ha/success? page-request))
        (let [parsed (parse-datacodru-page (ha/string page-request))]
          (if (and (parsed :name) (parsed :link))
            (assoc job :name (parsed :name) :link (parsed :link) :fail false)
            (job-die job env)))
        ((http-error-status-handler page-request
                                    job-die job-fail) job env)))))

(defn job-link [job env]
  (when-let [#^URI address (job :address)]
    (assoc job :link (URI. (.toASCIIString address)) :fail false)))

(defn job-name [job env]
  (when-let [#^URI link (job :link)]
    (assoc job :name (second (re-find #"/([^/]+)$" (.getPath link))) :fail false)))

(defn job-tag [pattern job env]
  (when-let [#^URI link (job :link)]
    (when-let [tag (or (if pattern 
                         (some (fn [pat] (if (re-find pat (str link)) (str pat)))
                               (if (sequential? pattern) pattern [pattern])))
                       (.getHost link))]
      (assoc job :tag tag :fail false))))

(defn job-length [job env]
  (when-let [#^URI link (job :link)]
    (let [length-request (ha/http-agent link :method "HEAD")]
      (ha/result length-request)
      (if (and (ha/done? length-request) (ha/success? length-request))
        (if-let [length (:content-length (ha/headers length-request))]
          (assoc job :length (Integer/parseInt length) :fail false)
          (job-die job env))
        ((http-error-status-handler length-request
                                    job-die job-fail) job env)))))

(defn job-file [job env]
  (when-let [name (job :name)]
    (when-let [#^File working-path (env :working-path)]
      (assoc job :file (new File (join-paths working-path name)) :fail false))))

(defn download [job env]
  (when-let [#^URI link (job :link)]
    (when-let [#^File file (job :file)]
      (let [loader (ha/http-agent
                    link
                    :headers {"Range" (str "bytes=" (file-length file) "-")}
                    :handler (fn [remote]
                               (with-open [local (FileOutputStream. file true)]
                                 (duck/copy (ha/stream remote) local))))]
        (log/info (str "Начата загрузка " (job :name)))
        (ha/result loader)
        (if (and (ha/done? loader) (ha/success? loader))
          (do (log/info (str "Закончена загрузка " (job :name)))
              (job-die job env))
          ((http-error-status-handler
            loader
            #(do (log/info (str "Загрузка не может быть закончена " (job :name)))
                 (job-die %1 %2))
            #(do (log/info (str "Прервана загрузка " (job :name)))
                 (job-fail %1 %2))) job env))))))

(def #^{:doc "Хосты упорядочены от частного к общему."}
     *download-rules*
     [[#"http://dsv.data.cod.ru/\d{6}"
       {:obtain-link   job-datacodru-link-name
        :obtain-tag    (partial job-tag [#"files3?.dsv.data.cod.ru"
                                         #"files2.dsv.data.cod.ru"])
        :obtain-file   job-file
        :obtain-length job-length
        :download      download
        :die           job-die}]
      [#"http://[\w\.]*data.cod.ru/\d+"
       {:obtain-link   job-datacodru-link-name
        :obtain-tag    (partial job-tag nil)
        :obtain-file   job-file
        :obtain-length job-length
        :download      download
        :die           job-die}]
      [#"http://77.35.112.8[1234]/.+"
       {:obtain-link   job-link
        :obtain-name   job-name
        :obtain-tag    (partial job-tag nil)
        :obtain-file   job-file
        :obtain-length job-length
        :download      download
        :die           job-die}]
      [#"http://dsvload.net/ftpupload/.+"
       {:obtain-link   job-link
        :obtain-name   job-name
        :obtain-tag    (partial job-tag nil)
        :obtain-file   job-file
        :obtain-length job-length
        :download      download
        :die           job-die}]])

(defn reflex-download-program
  "Простая рефлексная программа агента для скачивания."
  [percept]
  (letfn [(out-of-space [percept]
                        (when-let [#^File file ((percept :self) :file)]
                          (< (.getUsableSpace file) (file-length file))))
          (fully-loaded [percept]
                        (<= ((percept :self) :length) 
                            (file-length ((percept :self) :file))))
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
  
  [line]
  (let [[address actions] (match line *download-rules* {:action list})]
    (when (and address actions)
      (agent {:type ::download
              :address (URI. address)
              :link nil :name nil :tag nil :file nil :length nil
              :actions actions
              :program reflex-download-program
              :alive true :fail false :percept nil :action nil}))))

(defn download-agents [lines]
  (remove (comp not agent?) (map download-agent lines)))

(defmulti act (fn [ag env] (:type ag)))

(defmulti alive? #(:type @%))
(defmulti dead? #(:type @%))
(defmulti fail? #(:type @%))
(defmulti tag #(:type @%))

(defmethod alive? ::download [ag] (:alive @ag))
(defmethod dead?  ::download [ag] (not (:alive @ag)))
(defmethod fail? ::download [ag] (:fail @ag))
(defmethod tag ::download [ag] (:tag @ag))

(defn execute-action [ag env]
  (let [result (atom nil)
        thread
        (Thread. #(let [percept {:self @ag}
                        action  ((@ag :program) percept)]
                    (log/debug (str (or (@ag :name) (@ag :address)) " " action))
                    (let [new-state (((@ag :actions) action) @ag @env)]
                      (reset! result new-state)
                      (log/debug (str (or (@ag :name) (@ag :address)) " " action " "
                                      (cond (:not (:alive new-state)) "агент умер"
                                            (:fail new-state) "агент провалился"
                                            :else "успешно"))))))]
    (.start thread)
    (.join thread)
    @result))

(defn environment [& [{:keys [working-path termination]
                       :or   {working-path nil
                              termination #()}}]]
  (agent {:type ::download :agents '() :tags {}
          :working-path working-path :termination termination}))

(defmulti add-agent (fn [env ag] (:type env)))
(defmulti add-agents (fn [env ags] (:type env)))
(defmulti add-tag (fn [env tag] (:type env)))
(defmulti run-environment :type)

(defmulti agents #(:type @%))
(defmulti tags #(:type @%))
(defmulti termination? #(:type @%))

(defmulti received-tag (fn [env ag] (:type env)))
(defmulti done (fn [env ag] (:type env)))

(defmethod add-agent ::download [env ag]
  (if-not (agent? ag) env
          (assoc env :agents (push (env :agents) ag))))

(defmethod add-agents ::download [env agents]
  (doseq [ag agents]
    (send *agent* add-agent ag))
  env)

(defmethod add-tag ::download [env tag]
  (if (or (nil? tag) (contains? (env :tags) tag)) env
      (assoc env :tags (assoc (env :tags) tag (atom false)))))

(defmethod run-environment ::download [env]
  (doseq [ag (:agents env)]
    (send-off ag act *agent*))
  env)
  
(defmethod agents ::download [env]
  (@env :agents))

(defmethod tags ::download [env]
  (@env :tags))

(defmethod termination? ::download [env]
  (or (empty? (agents env)) (every? dead? (agents env))))

(defn tag-locked? [env tag]
  (when (contains? (@env :tags) tag)
    @((@env :tags) tag)))

(defn tag-lock [env tag]
  (when (contains? (@env :tags) tag)
    (reset! ((@env :tags) tag) true)))

(defn tag-unlock [env tag]
  (when (contains? (@env :tags) tag)
    (reset! ((@env :tags) tag) false)))

(defmacro with-lock-env-tag [env tag & body]
  `(do (tag-lock ~env ~tag)
       (let [result# ~@body]
         (tag-unlock ~env ~tag)
         result#)))

(defmethod received-tag ::download [env ag]
  (when-let [next-alive-untagged-agent
             (next-after-when #(and (alive? %) (not (tag %)))
                              ag (agents *agent*))]
    (send-off next-alive-untagged-agent act *agent*))
  (send-off ag act *agent*)
  env)

(defmethod done ::download [env ag]
  (let [alive-unfailed-with-same-tag
        (some #(when (and (= (tag ag) (tag %)) (alive? %) (not (fail? %))) %)
              (agents *agent*))
        next-alive-with-same-tag
        (next-after-when #(and (= (tag ag) (tag %)) (alive? %))
                         ag (agents *agent*))]
    
    (cond alive-unfailed-with-same-tag
          (send-off alive-unfailed-with-same-tag act *agent*)

          next-alive-with-same-tag
          (send-off next-alive-with-same-tag act *agent*)

          (termination? *agent*) ((env :termination))))
  env)

(defmethod act ::download [ag env]
  (let [tag (:tag ag)]
    (cond (dead? *agent*) ag 

          (not tag) (do (let [result (execute-action *agent* env)]
                          (cond (not (:alive result)) (send env done *agent*)
                                (:fail result) (send-off *agent* act env)
                                (:tag result) (do (send env add-tag (:tag result))
                                                  (send env received-tag *agent*))
                                :else (send-off *agent* act env))
                          result))

          (tag-locked? env tag) ag

          :else (do (let [result (with-lock-env-tag env tag
                                   (execute-action *agent* env))]
                      (cond (not (:alive result)) (send env done *agent*)
                            (:fail result) (send env done *agent*)
                            :else (send-off *agent* act env))
                      result)))))

;;; COMMAND-LINE

(defn valid-path [#^String path]
  (let [#^File the-path (File. (.getCanonicalPath (File. path)))]
    (when (.exists the-path) the-path)))

(defn valid-jobs-file [#^String path]
  (when-let [#^File file (valid-path path)]
    (when (and (.isFile file) (.canRead file))
      file)))

(defn valid-output-dir [#^String path]
  (when-let [#^File dir (valid-path path)]
    (when (and (.isDirectory dir) (.canWrite dir))
      dir)))
 
(defn -main [& args]
  (with-command-line args
      *usage*
      [[quiet? q? "работать молча"]
       [debug? d? "писать подробные сообщения для отлова багов"]
       remaining-args]

    (let [root-logger (Logger/getLogger "")
          console-handler (first (.getHandlers root-logger))
          basic-formatter (proxy [Formatter] []
                            (format
                             [#^LogRecord record]
                             (let [time (new Date (.getMillis record))
                                   hour (.getHours time)
                                   min  (.getMinutes time)
                                   sec  (.getSeconds time)]
                               (str hour ":" min ":" sec " " 
                                    (.getName (.getLevel record)) ": "
                                    (.getMessage record) "\n"))))
          log-level (cond quiet? (Level/OFF)
                          debug? (Level/FINE)
                          :else  (Level/INFO))]
      (.setFormatter console-handler basic-formatter)
      (.setLevel console-handler log-level)
      (.setLevel root-logger log-level))

    (let [jobs-file (some valid-jobs-file remaining-args)
          working-path (or (some valid-output-dir remaining-args)
                           (valid-output-dir (System/getProperty "user.dir")))]
      (when (and jobs-file working-path)
        (let [lines (duck/read-lines jobs-file)
              e (environment {:working-path working-path 
                              :termination #(System/exit 0)})]
          (send e add-agents (download-agents lines))
          (await e)
          (send e run-environment))))))

;;; TESTS

(let [jj {:link (URI. "http://files3.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
          :name "Hayate_the_combat_butler.mkv"
          :address (URI. "http://dsv.data.cod.ru/433148")}
      jk {:link (URI. "http://files4.dsv.data.cod.ru/?WyIyMGI4%3D%3D")
          :name "Hayate_the_combat_butler.mkv"
          :address (URI. "http://dsv.data.cod.ru/433148")}
      j8 {:link (URI. "http://77.35.112.82/upload/Personal_Folders/Peshehod/Chelovek-Slon.mpg")
          :name "Chelovek-Slon.mpg"
          :address (URI. "http://77.35.112.82/upload/Personal_Folders/Peshehod/Chelovek-Slon.mpg")}]
  (deftest test-tag
    (is (= (:tag (leica/job-tag nil jj nil))
           "files3.dsv.data.cod.ru"))
    (is (= (:tag (leica/job-tag #"files3?.dsv.data.cod.ru" jj nil))
           "files3?.dsv.data.cod.ru"))
    (is (= (:tag (leica/job-tag [#"files3?.dsv.data.cod.ru"
                                 #"files2.dsv.data.cod.ru"] jj nil))
           "files3?.dsv.data.cod.ru"))
    (is (= (:tag (job-tag [#"files3?.dsv.data.cod.ru"
                           #"files2.dsv.data.cod.ru"] jk nil))
           "files4.dsv.data.cod.ru"))
    (is (= (:tag (job-tag nil j8 nil))
           "77.35.112.82"))))

(deftest test-match
  (is (= (match "http://dsv.data.cod.ru/433148"
                '((#"http://dsv.data.cod.ru/\d{6}" :MATCH))
                {:rule-response rest})
         '(:MATCH))))

(deftest test-run
  (is (nil?
       (and nil
            (do
              (def e (environment {:working-path (File. "/home/haru/inbox/dsv")}))
              (send e add-agents (download-agents
                                  ["http://dsv.data.cod.ru/458692"
                                   "http://77.35.112.82/upload/Personal_Folders/Peshehod/Chelovek-Slon.mpg"]))
              (await e)
              (send e run-environment))))))

(deftest test-run-step
  (is (nil?
       (and nil
            (do
              (def e (environment {:working-path (File. "/home/haru/inbox/dsv")}))
              (def a (download-agent "Goal.Icon.rar http://dsv.data.cod.ru/456136"))
              (def b (download-agent ""))
              (send e add-agent a)
              (send e add-agent b)
              (await e)
              (send-off a act e)
              (send-off b act e)
              )))))

;; ACCOUNT

(defn user-agent []
  (str "Leica by Zahardzhan & GO1d ("
       (System/getProperty "os.name") " "
       (System/getProperty "os.version") " "
       (System/getProperty "os.arch") ")"))

(defmacro with-datacod-auth
  [#^HttpClient client email password  & body]
  `(let [#^PostMethod post# (new PostMethod "http://nvwh.cod.ru/link/auth/")]
     (doto post#
       (.addParameter "refURL" "http://dsv.data.cod.ru")
       (.addParameter "email" ~email)
       (.addParameter "password" ~password))
    (let [post-status# (.executeMethod ~client post#)]
      ~@body)))

(let [#^HttpClient client (new HttpClient)]
  (with-datacod-auth client "zahardzhan@gmail.com" "zzzzzz"
    (let [#^GetMethod get (new GetMethod "http://dsv.data.cod.ru")]
      (let [get-status (.executeMethod client get)
            get-response (.getResponseBody get)]
        [get-status (parse-datacodru-page 
                     (EncodingUtil/getString get-response "UTF-8"))]))))
