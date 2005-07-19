TARBALL=conf.tar.gz

CONFDIR=~/common/conf

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
