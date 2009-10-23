;;; leica.clj: многопоточная качалка для data.cod.ru и dsvload.net.

;; Роман Захаров
;; октябрь, 2009

;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров"}
  leica
  (:require [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck])
  (:import (java.io InputStream ByteArrayOutputStream
                    ByteArrayInputStream)
           (java.net HttpURLConnection InetAddress URI URL URLEncoder)
           (org.htmlparser Parser)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags LinkTag)
           (org.htmlparser.nodes TagNode)))

(in-ns 'leica)

(def *ping-timeout* 3000)

(defn parse-data.cod.ru-page [#^String url]
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
    {:name @name :link @link}))

(defn job-cod.ru-link-name [job]
  (when-let [#^String address (job :address)]
    (let [parsed (leica/parse-data.cod.ru-page address)]
      (when (and (parsed :name) (parsed :link))
        (merge job {:name (parsed :name) :link (parsed :link)})))))

(defn job-link [job]
  (when-let [#^String address (job :address)]
    (merge job {:link (. (new URI address) toASCIIString)})))

(defn job-name [job]
  (when-let [#^String link (job :link)]
    (merge job {:name (second (re-find #"/([^/]+)$" (. (new URI link) getPath)))})))

(defn job-tag [pattern job]
  (when-let [#^String link (job :link)]
    (when-let [tag (or (some (fn [p] (and (re-find p link) p))
                             (if (seq? pattern) pattern [pattern]))
                       (. (new URI link) getHost))]
      (merge job {:tag tag}))))

;; (defn job-length [job]
  

;; def length(pattern=None):
;;     def action(job):
;;         try:
;;             responce = urlopen(Request(job.link))
;;             job.length = int(responce.info().getheader('Content-Length'))
;;         except URLError:
;;             raise Fail('СБОЙ ПОЛУЧЕНИЯ РАЗМЕРА ЗАГРУЖАЕМОГО ФАЙЛА')
;;         else:
;;             if job.length: return job
;;             else: raise RIP('НЕВОЗМОЖНО ПОЛУЧИТЬ РАЗМЕР ЗАГРУЖАЕМОГО ФАЙЛА')

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

;; An agent is something that perceives and acts. As such, each agent has a slot to hold its current percept, and its current action. The action will be handed back to the environment simulator to perform (if legal). Each agent also has a slot for the agent program, and one for its score as determined by the performance measure.

(defstruct agere :program :body :score :percept :action :name :alive)

;;    Agents take actions (based on percepts and the agent program) and receive a score (based on the performance measure). An agent has a body which can take action, and a program to choose the actions, based on percepts.

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


