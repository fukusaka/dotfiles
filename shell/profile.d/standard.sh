# ~/.bash_profile: -*-shell-script-*-
# $Id$

case "$1" in
    home*)
	umask 022

    # ����λ���
#	export LANG=ja_JP.eucJP
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

	;;
    *)
esac
