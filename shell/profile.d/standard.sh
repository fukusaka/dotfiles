# ~/.bash_profile: -*-shell-script-*-
# $Id$

case "$1" in
    home*)
	umask 022

    # 言語の指定
	export LANG=ja_JP.eucJP
	export LC_TIME=C

    # パスの指定
       	export PATH=${HOME}/bin:${PATH}
	export INFOPATH=~/info:/usr/info:/usr/share/info

    # コマンドの指定
	export CC=gcc
	export EDITOR=/usr/bin/vi
	export PAGER=/usr/bin/pager

    # less/lv の指定
	unset LESSCHARSET
	LESSCHARDEF=8bcccbcc18b95.8bcccbcc18b95.
	JLESSCHARSET=japanese-euc
	LV=-Oej
	export LESSCHARSET LESSCHARDEF JLESSCHARSET LV

    # その他
	#export CANNAHOST=localhost

	;;
    *)
esac
