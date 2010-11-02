(defproject leica "0.5-master-SNAPSHOT" 
  :description "Download manager written in lisp."
  :url "http://github.com/zahardzhan/leica"
  :dependencies     [[org.clojure/clojure "1.2.0"]
                     [org.clojure/clojure-contrib "1.2.0"]
                     [hooks "1.0.0"]
                     [commons-codec      "1.4"]
                     [commons-httpclient "3.1"]
                     [commons-logging    "1.1.1"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]
                     [swank-clojure "1.2.1"]]
  :main leica)
