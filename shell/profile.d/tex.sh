# tex.sh: -*-shell-script-*-
# $Id$

case "$1" in
    home)
	TEXINPUTS=.//:${TEXCONFDIR}//:
	TEX=ptex
        TEXEDIT=${EDITOR}
	export TEX TEXINPUTS TEXEDIT
	;;
    *)
esac
