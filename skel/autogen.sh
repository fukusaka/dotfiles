#!/bin/sh


config()
{
echo "Adding libtools."
libtoolize --automake --copy

echo "Building macros."
aclocal $ACLOCAL_FLAGS

echo "Building config header."
autoheader

echo "Building makefiles."
automake   --add-missing --copy

echo "Building configure."
autoconf

echo 'run "configure; make"'
}

remove()
{
echo "Removing libtools."
rm -f ltconfig ltmain.sh

echo "Removing aclocal."
aclocal $ACLOCAL_FLAGS
rm -f aclocal.m4

echo "Removing config header."
rm -f config.h.in

echo "Removing makefiles."
rm -f install-sh missing mkinstalldirs
rm -f Makefile.in
subdirs=`grep '^SUBDIRS' Makefile.am | sed 's/^SUBDIRS *= *\(.*\)$/\1/'`
for dir in ${subdirs}
do
    rm -f  ${dir}/Makefile.in
done

echo "Removing configure."
rm -f configure config.guess config.sub stamp-h.in
}

case "$1" in
config)
    config
    ;;
remove)
    remove
    ;;
*)
    echo "Usage: $0 {config|remove}"
    ;;
esac
