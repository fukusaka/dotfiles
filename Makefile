TARBALL=conf.tar.gz

CONFDIR=common/conf

all:
	@ echo "link -- リンクを張り巡らす"
	@ echo "pack -- tarball に固める"

link:
	@ grep -v '^#' link-list | \
	  awk '!/^#/ { printf("cd %s && ln -sf %s/%s %s\n",$$2,"${CONFDIR}",$$1,$$3)}' | cat

pack:
	cd .. ; find conf -type f  -not -name '*~' \
			-not -name '*.elc' \
	| tar cfz ${TARBALL} -T -
clean:
	find . -name '*~' | xargs rm -f

distclean: clean
	find . -name 'emacs19' | xargs rm -rf
	find . -name 'emacs20' | xargs rm -rf
	find . -name '*.elc' | xargs rm -rf


SIMPLE_EMACS=\
05lang.el 10standard.el \
15automode.el \
30frame.el 40color.el \
50dired-x.el 50programming.el 50tramp.el

SIMPLE_EMACS_ROOT=\
05lang.el 10standard.el 40color.el

simple-emacs:
	@file=/tmp/tmp$$$$ ; \
	( cd emacs/customize.d ; cat ${SIMPLE_EMACS} ) | sed -e "/^;;;/d" > $$file ; \
	sed -e "/INSERT_DOT_EMACS/r$$file" emacs/wrap-dotemacs > dotfiles/emacs; \
	( cd emacs/customize.d ; cat ${SIMPLE_EMACS_ROOT} ) | sed -e "/^;;;/d" > $$file ; \
	sed -e "/INSERT_DOT_EMACS/r$$file" emacs/wrap-dotemacs > dotfiles/emacs-root; \
	rm -f $$file
