#!/bin/sh

: ${EMACS:=/Applications/Emacs.app/Contents/MacOS/Emacs}
: ${ELCDIR:=~/common/conf/emacs/elisp-emacs24/muse-3.20}
: ${ADDON:=muse-3.20}

cd ${ADDON}
make EMACS=${EMACS} ELISPDIR=${ELCDIR} install-bin
make EMACS=${EMACS} ELISPDIR=${ELCDIR} clean
