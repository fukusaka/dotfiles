TARBALL=../conf.tar.gz

OWNDIR=lib/conf

all:
	@ echo "link -- "
	@ echo "pack -- "

link:
	@( cd ~ ; 			\
	   awk -v PWD="${OWNDIR}/" '!/^#/ { print "ln -sf",PWD $$1,$$2}' \
	   ${OWNDIR}/link-list	\
	   | sh			\
	 )

pack:
	find . -type f  -not -name '*~' \
			-not -name '*.elc' \
	| sed -n '/.\//s///p' \
	| tar cfz ${TARBALL} -T -
clean:
	find . -name '*~' | xargs rm -f

distclean: clean
	find . -name 'emacs19' | xargs rm -rf
	find . -name 'emacs20' | xargs rm -rf
	find . -name '*.elc' | xargs rm -rf
