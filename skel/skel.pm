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

use Carp;
use Data::Dumper qw(Dumper);
use English qw( -no_match_vars );
#use Exporter qw(import);

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
