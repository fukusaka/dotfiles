# ~/.bash_profile: -*-shell-script-*-
# $Id: standard.sh 129 2005-07-19 10:56:11Z fukusaka $

if [ -f ${HOME}/.bashrc ]; then . ${HOME}/.bashrc; fi

umask 022

# 言語の指定
#export LANG=ja_JP.eucJP
export LANG=ja_JP.UTF-8
export LC_TIME=C

# パスの指定
export PATH=${HOME}/bin:${PATH}
export INFOPATH=~/info:/usr/info:/usr/share/info
	
# コマンドの指定
export CC=gcc
export EDITOR=/usr/bin/vi
test -x /usr/bin/pager && export PAGER=/usr/bin/pager
test -x /usr/bin/page && export PAGER=/usr/bin/page

# less/lv の指定
#unset LESSCHARSET
#LESSCHARDEF=8bcccbcc18b95.8bcccbcc18b95.
#JLESSCHARSET=japanese-euc
LV=-Ou8
#export LESSCHARSET LESSCHARDEF JLESSCHARSET LV

# その他
#export CANNAHOST=localhost

# TeX 設定
TEXINPUTS=.//:${TEXCONFDIR}//:
TEX=ptex
TEXEDIT=${EDITOR}
export TEX TEXINPUTS TEXEDIT
