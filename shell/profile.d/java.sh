# java.sh: -*-shell-script-*-
# $Id$

case "$1" in
    home)
	#export JAVA_HOME=/usr/local/sun/jdk1.3.0_02
	#export PATH=${JAVA_HOME}/bin:${PATH}
	export CLASSPATH=.:/usr/share/java/postgresql.jar
	;;
    *)
esac

