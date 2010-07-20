(defproject leica "0.4.0-master-SNAPSHOT" 
  :description "Multithreaded downloader for data.cod.ru."
  :url "http://github.com/zahardzhan/leica"
  :autodoc {:name "Leica", :page-title "Leica API documentation"}
  :dependencies     [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                     [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
                     [ahc-clj "0.1.1"]
                     [hooks "1.0.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :main leica)
