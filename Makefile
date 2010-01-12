all: clear compile

clear:
	rm -Rf *~
	rm -f leica.jar
	rm -rf classes/*

compile:
	unzip -u -o lib/clojure-1.1.0-master-20091231.150150-10.jar -d classes
	unzip -u -o lib/clojure-contrib-1.0-20091212.214557-33.jar -d classes
	unzip -u -o lib/commons-codec-1.4.jar -d classes
	unzip -u -o lib/commons-logging-1.1.1.jar -d classes
	unzip -u -o lib/commons-httpclient-3.1.jar -d classes
	unzip -u -o lib/htmlparser-1.6.jar -d classes

	rm -rf classes/META-INF
	java -classpath "classes/:lib/:src/" clojure.main -e "(compile 'leica)"
	jar cmf Manifest.txt leica.jar -C classes .