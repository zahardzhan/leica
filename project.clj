(defproject leica "1.0.14-no-env-SNAPSHOT" 
  :description "Multithreaded downloader for data.cod.ru"
  :url "http://github.com/zahardzhan/leica"
  :autodoc {:name "Лейка", :page-title "Документация API Лейки"}
  :dependencies     [[org.clojure/clojure "1.1.0-master-SNAPSHOT"]
                     [org.clojure/clojure-contrib "1.1.0-master-SNAPSHOT"]
                     [commons-codec "1.4"]
                     [commons-httpclient "3.1"]
                     [commons-logging "1.1.1"]
                     [org.htmlparser/htmlparser "1.6"]]
  :dev-dependencies [[swank-clojure "1.1.0"]
                     [autodoc "0.7.0"]]
  :main leica)