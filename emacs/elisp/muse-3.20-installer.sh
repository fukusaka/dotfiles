#!/bin/sh

: ${EMACS:=/Applications/Emacs.app/Contents/MacOS/Emacs}
: ${ELCDIR:=~/common/conf/emacs/elisp-emacs24}
: ${SUBDIR:=muse-3.20}

cd ${SUBDIR}
make EMACS=${EMACS} ELISPDIR=${ELCDIR}/${SUBDIR} install-bin
make EMACS=${EMACS} ELISPDIR=${ELCDIR}/${SUBDIR} clean
