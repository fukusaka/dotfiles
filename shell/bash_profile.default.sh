# ~/.bash_profile: -*-shell-script-*-
# $Id: standard.sh 129 2005-07-19 10:56:11Z fukusaka $

if [ -f ${HOME}/.bashrc ]; then . ${HOME}/.bashrc; fi

umask 022

# ����λ���
#export LANG=ja_JP.eucJP
export LANG=ja_JP.UTF-8
export LC_TIME=C

# �ѥ��λ���
export PATH=${HOME}/bin:${PATH}
export INFOPATH=~/info:/usr/info:/usr/share/info
	
# ���ޥ�ɤλ���
export CC=gcc
export EDITOR=/usr/bin/vi
test -x /usr/bin/pager && export PAGER=/usr/bin/pager
test -x /usr/bin/page && export PAGER=/usr/bin/page

# less/lv �λ���
#unset LESSCHARSET
#LESSCHARDEF=8bcccbcc18b95.8bcccbcc18b95.
#JLESSCHARSET=japanese-euc
LV=-Ou8
#export LESSCHARSET LESSCHARDEF JLESSCHARSET LV

# ����¾
#export CANNAHOST=localhost

# TeX ����
TEXINPUTS=.//:${TEXCONFDIR}//:
TEX=ptex
TEXEDIT=${EDITOR}
export TEX TEXINPUTS TEXEDIT
