# ~/.bash_profile: -*-shell-script-*-
# $Id$

case "$1" in
    home*)
	umask 022

    # ����λ���
	export LANG=ja_JP.eucJP
	export LC_TIME=C

    # �ѥ��λ���
       	export PATH=${HOME}/bin:${PATH}
	export INFOPATH=~/info:/usr/info:/usr/share/info

    # ���ޥ�ɤλ���
	export CC=gcc
	export EDITOR=/usr/bin/vi
	export PAGER=/usr/bin/pager

    # less/lv �λ���
	unset LESSCHARSET
	LESSCHARDEF=8bcccbcc18b95.8bcccbcc18b95.
	JLESSCHARSET=japanese-euc
	LV=-Oej
	export LESSCHARSET LESSCHARDEF JLESSCHARSET LV

    # ����¾
	#export CANNAHOST=localhost

	;;
    *)
esac
