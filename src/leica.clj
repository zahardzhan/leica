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
  (:import (java.io File InputStream ByteArrayOutputStream
                    ByteArrayInputStream)
           (java.net HttpURLConnection InetAddress URI URL URLEncoder)
           (org.htmlparser Parser)
           (org.htmlparser.util ParserException)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags LinkTag)
           (org.htmlparser.nodes TagNode)))

(in-ns 'leica)

(def *ping-timeout* 3000)

;;; AUX

(defn push [coll x] (concat coll (list x)))

(defn file-length [#^File file]
  (if (.exists file) (.length file) 0))

(defn join-paths [path1 path2]
  (str (File. (File. path1) path2)))

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
         {:name @name :link (new URI @link)}
         (catch ParserException _ nil))))

(defn job-datacodru-link-name [job]
  (when-let [#^URI address (job :address)]
    (let [page-agent (ha/http-agent address)]
      (if-not (ha/success? page-agent) job ;; TODO: Проработать коды ошибок
              (let [parsed (parse-datacodru-page (ha/string page-agent))]
                (when (and (parsed :name) (parsed :link))
                  (assoc job :name (parsed :name) :link (parsed :link))))))))

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
    (let [job-agent (ha/http-agent link)]
      (if-not (ha/success? job-agent) job
              (assoc job :length (Integer/parseInt
                                  (:content-length (ha/headers job-agent))))))))

(defn job-file [dir job]
  (when-let [name (job :name)]
    (assoc job :file (new File (join-paths dir name)))))

(defn job-die [job]
  (assoc job :alive false))

(defn download [job]
  (when-let [#^URI link (job :link)]
    (when-let [#^File file (job :file)]
      (let [headers {"Range" (str "bytes=" (file-length file) "-")}
            loader (ha/http-agent link
                                  :headers headers
                                  :handler
                                  (fn [remote]
                                    (with-open [local (duck/writer file)]
                                      (duck/copy (ha/stream remote) local))))]
        (ha/result loader)
        (if (ha/done? loader)
          (job-die job)
          job)))))
                      
;;; RULE

(defn default-matcher 
  "Дефолтный сопоставитель с образцом."
  [rule sample]
  (cond (fn? rule) (rule sample)
        (string? rule) (if (= rule sample) rule)
        (= (type rule) java.util.regex.Pattern) (re-find rule sample)))

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

;;; AGENCY

(defstruct
  #^{:doc "Агент для скачивания файла.

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
  :length  размер файла, что нужно скачать"}

  job 
  :name :program :actions :percept :action :alive
  :address :link :tag :file :length)

(defn alive? [agnt] (:alive agnt))
(def dead? (complement alive?))

(defn reflex-job-program
  "Простая рефлексная программа для загрузочного агента."
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

(defn make-job
  "Конструктор загрузочного агента из строки с адресом и набора правил."
  [line rules]
  (let [[address actions] (match line rules {:action list})]
    (when (and address actions)
      (struct-map job :program reflex-job-program :alive true
                  :address (URI. address) :actions actions))))

(defn act [ag action]
  (((ag :actions) action) ag))

;;; ENVIRONMENTS

;; (defstruct 
;;     #^{:doc "Агенты живут в этом окружении до тех пор, пока у них не появится таг.
;;   Потом они переходят в дочернее окружение с таким же тагом, и в нём уже
;;   загружаются. Все дочерние окружения выполняются как агенты.
;;   Окружение завершает работу после того как все агенты перейдут в 
;;   дочерние окружения и уже в них завершат своё дело.

;;   :type    тип окружения, для диспетчирезации мультиметодов
;;   :tagenvs отмеченные окружения располагаются в хэше {tag env, ...}
;;   :agents  неотмеченные агенты"}

;;   tagger-environment :type :tagenvs :agents)

;; (defstruct
;;     #^{:doc "Отмеченное окружение с отмеченными агентами.
;;   Окружение выполняется как агент в окружении более высокого порядка.

;;   :type    ::tagged
;;   :tag     отметка
;;   :agents  агенты в окружении"}
  
;;   tagged-environment :type :tag :agents)

;; (defn make-tagger-environment [agents]
;;   (struct-map tagger-environment :type ::tagger :agents agents :tagenvs {}))

;; (defn make-tagged-environment [tag agents]
;;   (struct-map tagged-environment :type ::tagged :agents agents :tag tag))

;; (defmulti add-agent 
;;   "Добавляет агента в окружение."
;;   (fn [env agnt] (:type env)))

;; (defmulti step
;;   "Базовый симулятор окружения, в котором окружение проживает один момент.
;;    Окружение дает каждому агенту восприятие, получает от него действие и
;;    выполняет это действие. В итоге окружение возвращает в себя обновленных агентов."
;;   :type)

;; (defmulti run-env
;;   "Симулятор окружения."
;;   :type)

;; (defmulti termination? 
;;   "Return true if the simulation should end now."
;;   :type)

;; (defmethod add-agent ::tagger [env agnt]
;;   (assoc env :agents (conj (env :agents) agnt)))

;; (defmethod add-agent ::tagged [env agnt]
;;   (assoc env :agents (conj (env :agents) agnt)))

;; (defn alive-agents [agents] (filter alive? agents))

;; (defn all-agents-is-dead? [env]
;;   (every? dead? (:agents env)))

;; (defn no-agents [env]
;;   (empty (:agents env)))

;; (defmethod termination? ::tagged [env]
;;   (or (no-agents env) (all-agents-is-dead? env)))

;; (defmethod termination? ::tagger [env]
;;   (and (or (no-agents env) (all-agents-is-dead? env))
;;        (every? (fn [tagenv] (termination? @tagenv)) (env :tagenvs))))

;; (defn execute-agent-actions [agents & [{:keys [forbidden]
;;                                         :or {:forbidden nil}}]]
;;   (for [agnt agents :when agnt]
;;     (let [percept {:self agnt}
;;           action  ((agnt :program) percept)]
;;       (if (contains? forbidden action) agnt
;;           (((agnt :actions) action) agnt)))))

;; (defmethod step ::tagger [env]
;;   (let [[tagged-agents untagged-agents] 
;;         (separate :tag (alive-agents (env :agents)))

;;         ;; Агенты с тагом перекидываются в дочернее окружение с таким же тагом,
;;         ;; если такое окружение не существует -- оно создается.
;;         updated-tagenvs 
;;         (loop [agents tagged-agents tag-envs (env :tagenvs)]
;;           (if-not (seq agents) tag-envs
;;                   (let [agnt (first agents)
;;                         tag (agnt :tag)
;;                         tag-env (or (tag-envs tag)
;;                                     (agent (make-tagged-environment tag [])))]
;;                     (send-off tag-env add-agent agnt)
;;                     (recur (rest agents) (if-not (tag-envs tag) 
;;                                            (assoc tag-envs tag tag-env)
;;                                            tag-envs)))))] 
;;     (assoc env :tagenvs updated-tagenvs
;;            :agents (execute-agent-actions untagged-agents
;;                                           {:forbidden [:download]}))))

;; (defmethod step ::tagged [env]
;;   (assoc env :agents (execute-agent-actions (alive-agents (env :agents)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct environment :type :agents :walkers)

(defmulti add-agent (fn [env ag] (:type env)))
(defmulti add-walker (fn [env tag walker] (:type env)))
(defmulti step :type)
(defmulti run :type)

(defn make-walker [env tag]
  (if-not tag
    (agent
     (fn []
       (doseq [ag (@env :agents) :when (not (@ag :tag))]
         (let [percept {:self @ag}
               action  ((@ag :program) percept)
               result  (act @ag action)]
           (dosync (ref-set ag result))
           (when-let [ag-tag (@ag :tag)]
             (send env add-walker ag-tag (make-walker env ag-tag)))))))))

(defn walk [walker]
  (walker) walker)

(defn make-download-env []
  (let [env (agent (struct-map environment :type :download :agents '() :walkers {}))]
    (send env add-walker nil (make-walker env nil))
    env))

(defmethod add-agent :download [env ag]
  (assoc env :agents (push (env :agents) (ref ag))))

(defmethod add-walker :download [env tag walker]
  (assoc env :walkers (assoc (env :walkers) tag walker)))

(defmethod step :download [env]
  (doseq [[tag walker] (env :walkers)]
    (send-off walker walk))
  env)

(def e (make-download-env))
(send e add-agent j1)
(send e step)
(send-off ((@e :walkers) nil) walk)

e
(agent-errors ((@e :walkers) nil))


;; (defmethod step :default [env]
;;   (do (for [[tag walker] @(env :walkers)]
;;         (send-off walker walk))
;;       env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Хосты упорядочены от частного к общему
(def job-rules
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

(let [jobs [(make-job "http://dsv.data.cod.ru/441778" job-rules)
            (make-job "gold http://dsv.data.cod.ru/443824" job-rules)]]
  (def j1 (jobs 0))
  (def j2 (jobs 1)))