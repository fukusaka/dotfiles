TARBALL=../conf.tar.gz

all:
	@ echo "link -- "
	@ echo "pack -- "

link:

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
