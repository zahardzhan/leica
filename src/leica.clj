;;; leica.clj: многопоточная качалка для data.cod.ru и dsvload.net.

;; Роман Захаров, Александр Золотов
;; октябрь, 2009

;; Это свободная программа, используйте на свой страх и риск.

(ns #^{:doc "Многопоточная качалка для data.cod.ru и dsvload.net."
       :author "Роман Захаров, Александр Золотов"}
  leica
  (:require [clojure.contrib.http.agent :as ha]
            [clojure.contrib.duck-streams :as duck])
  (:import (java.io InputStream ByteArrayOutputStream
                    ByteArrayInputStream)
           (java.net HttpURLConnection)
           (org.htmlparser Parser)
           (org.htmlparser.visitors NodeVisitor)
           (org.htmlparser.tags LinkTag)
           (org.htmlparser.nodes TagNode)))

(defn parse-data.cod.ru-file-description-page [#^String url]
  "Парсит страницу файла на датакоде."
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

;;; TESTING

;; (ha/string (ha/http-agent "http://data.cod.ru"))

;; (import '(java.net InetAddress))
;; (def *timeout* 3000)
;; (def *mask* "192.16.1.")
;; (def *up* (ref []))
;; (def ip-list (for [x (range 1 255)] (str *mask* x)))
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

(defmulti percept [agent env])
;;    Return the percept for this agent.

(defmulti update-fn [env])
;;    Modify the environment, based on agents actions, etc.

(defmulti legal-actions [env])
;;    A list of the action operators that an agent can do.

(defmulti termination? [env])
;;    Return true if the simulation should end now.

(defmulti display-environment [env])
;;    Display the current state of the environment.

(defmulti execute-agent-actions [env]
;;    Each agent (if the agent is alive and has specified a legal action) takes the action.
