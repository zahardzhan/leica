# ./configure
# make
# make install

all: clear compile

clear:
	rm -Rf *~
	rm -f leica.jar
	rm -rf classes/*

compile:
	unzip -u -o lib/commons-codec-1.3.jar -d classes
	unzip -u -o lib/commons-httpclient-3.1.jar -d classes
	unzip -u -o lib/commons-logging-1.1.1.jar -d classes
	unzip -u -o lib/clojure.jar -d classes
	unzip -u -o lib/clojure-contrib.jar -d classes
	unzip -u -o lib/libhtmlparser.jar -d classes
	rm -rf classes/META-INF
	java -classpath "classes/:lib/:src/clojure/" clojure.main -e "(compile 'leica)"
	jar cmf Manifest.txt leica.jar -C classes .