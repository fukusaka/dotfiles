TARBALL=conf.tar.gz

CONFDIR=common/conf

all:
	@ echo "link -- リンクを張り巡らす"
	@ echo "pack -- tarball に固める"
	@ echo "clean -- ゴミ削除"
	@ echo "simple-emacs -- emacs.el の生成"

link:
	@ awk '!/^#/ { printf("ln -sf ${CONFDIR}/%s %s\n", $$1,$$2) }' link-list

pack:
	cd .. ; find conf -type f  -not -name '*~' -not -name '*.elc' \
	| tar cfz ${TARBALL} -T -

clean:
	find . -name '*~' -exec rm -f {} \;

distclean: clean
	rm -rf emacs/customize.d/emacs{20,21,22,xx}
	find emacs -name '*.elc' -exec rm -f {} \;

SIMPLE_EMACS_COMMON=\
00utils.el 05lang.el 06ime.el 08fixup.el 10standard.el 10bindings.el

SIMPLE_EMACS=\
${SIMPLE_EMACS_COMMON} \
15automode.el \
30frame.el 40color.el \
50dired-x.el 50programming.el 50tramp.el

SIMPLE_EMACS_ROOT=\
${SIMPLE_EMACS_COMMON} \
40color.el

simple-emacs:
	@file=/tmp/tmp$$$$ ; \
	( cd emacs/customize.d ; cat ${SIMPLE_EMACS} ) > $$file ; \
	sed -e "/INSERT_DOT_EMACS/r$$file" emacs/wrap-dotemacs > dotfiles/emacs; \
	( cd emacs/customize.d ; cat ${SIMPLE_EMACS_ROOT} ) > $$file ; \
	sed -e "/INSERT_DOT_EMACS/r$$file" emacs/wrap-dotemacs > dotfiles/emacs-root; \
	rm -f $$file
