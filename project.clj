(defproject leica "1.0.14-SNAPSHOT" 
  :description "Multithreaded downloader for data.cod.ru"
  :url "http://github.com/zahardzhan/leica"
  :dependencies [[org.clojure/clojure "1.1.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 [commons-codec "1.4"]
                 [commons-httpclient "3.1"]
                 [commons-logging "1.1.1"]
                 [org.htmlparser/htmlparser "1.6"]]
  :dev-dependencies [[swank-clojure "1.1.0"]]
  :main leica)