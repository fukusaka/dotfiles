# ~/.bash_profile: -*-shell-script-*-
# $Id$

umask 002

# パスの指定
export PATH=${HOME}/bin:${PATH}:/usr/local/sun/jdk/bin
export CONFDIR=${HOME}/lib/conf
export INFOPATH=~/info:/usr/info:/usr/share/info
export TEXINPUTS=.//:${CONFDIR}/tex//:

export LD_LIBRARY_PATH=/home/fukusaka/lib
export LD_RUN_PATH=/home/fukusaka/lib

export CLASSPATH=.:/usr/share/java/postgresql.jar

# コマンドの指定
export TEX=ptex
export CC=gcc

#export EDITOR=/usr/bin/vi
#export PAGER=/usr/bin/less	
#export TEXEDIT='/usr/bin/vi %s'

# 言語の指定
#export LANG=ja_JP.ujis
export LANG=ja_JP.eucJP
export LC_TIME=C
#export LC_ALL=ja_JP.ujis
#export LC_COLLATE=ja_JP.ujis
#export LC_MESSAGES=ja_JP.ujis
export OUTPUT_CHARSET=EUC-JP
