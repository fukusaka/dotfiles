# -*- coding: utf-8 -*-

CONFDIR=common

all:
	@ echo "link          リンクを張り巡らす"
	@ echo "clean         ゴミ削除"
	@ echo "simple-emacs  emacs.el の生成"
	@ echo "emacs-clean   emacs のbytecode を削除"

link:
	@ awk '!/^#/ { printf("ln -sf ${CONFDIR}/%s %s\n", $$1,$$2) }' link-list

clean:
	find . -name '*~' -exec rm -f {} \;

emacs-clean:
	rm -rf emacs/customize.d/emacs{20,21,22,23,24,25,xx}
	find emacs -name '*.elc' -exec rm -f {} \;

distclean: clean emacs-clean

SIMPLE_EMACS := \
$(notdir $(wildcard emacs/customize.d/[0-9][0-9]*))

SIMPLE_EMACS_ROOT := \
$(notdir $(wildcard emacs/customize.d/[0-3][0-9]*)) \
40color.el

simple-emacs:
	@file=/tmp/tmp$$$$ ; \
	( cd emacs/customize.d ; cat ${SIMPLE_EMACS} ) > $$file ; \
	sed -e "/INSERT_DOT_EMACS/r$$file" emacs/wrap-dotemacs > dotfiles/emacs; \
	( cd emacs/customize.d ; cat ${SIMPLE_EMACS_ROOT} ) > $$file ; \
	sed -e "/INSERT_DOT_EMACS/r$$file" emacs/wrap-dotemacs > dotfiles/emacs-root; \
	rm -f $$file
