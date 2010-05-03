(defproject leica "0.2.0-master-SNAPSHOT" 
  :description "Multithreaded downloader for data.cod.ru"
  :url "http://github.com/zahardzhan/leica"
  :autodoc {:name "Leica", :page-title "Leica API documentation"}
  :dependencies     [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                     [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                     [commons-codec "1.4"]
                     [commons-httpclient "3.1"]
                     [commons-logging "1.1.1"]]
  :dev-dependencies [[swank-clojure "1.1.0"]
                     [autodoc "0.7.0"]]
  :main leica)
