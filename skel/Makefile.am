# Toplevel
#SUBDIRS = src
#EXTRA_DIST = Moimoi
#bin_SCRIPTS = moimoi-config

# Program
bin_PROGRAMS = moimoi
EXTRA_PROGRAMS = moon

moimoi_SOURCES = moimoi.c 
EXTRA_moimoi_SOURCES = moimoi-sun.c

LDADD =
moimoi_LDADD =

BUILT_SOURCES = \
	moimoi.no

# Library
noinst_LIBRARIES = libmisc.a

libmisc_a_SOURCES = getopt.c 

libcpio_a_LIBADD

# With Libtool

# lib_LTLIBRARIES = libpop.la

# libpop_la_SOURCES

#INCLUDES = -I$(includedir) 

# For Guile
#SUFFIXES = .x
#.c.x:
#	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@

