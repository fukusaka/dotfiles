package @@FBASE@@;
# -*- coding: utf-8 -*-
# file: @@FNAME@@
#
# Copyright (C) @@YEAR@@ @@NAME@@
#
#     Author: @@NAME@@ <@@ADDRESS@@>
#     Created:       <@@DATE@@>
#     Last Modified: <@@DATE@@>
#
@@LICENSE@@

use strict;
use warnings;
use utf8;

#use Exporter qw(import);
use Carp;
use Data::Dump qw(dump);
use Data::Dumper qw(Dumper);
use English qw(-no_match_vars);

#our @EXPORT = qw ();
#our @EXPORT_OK = qw ();

1;

__END__

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
FBASE: (file-name-sans-extension (file-name-nondirectory buffer-file-name))
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%Y/%m/%d %H:%M:%S")
LICENSE: (my-ask-license-string "# ")
@@@@
