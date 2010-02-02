#!/usr/bin/perl -T
# $Id$

# @@FNAME@@ --
#
# Copyright (C) @@YEAR@@ @@NAME@@
#
# Author: @@NAME@@ <@@ADDRESS@@>
# Maintainer: @@NAME@@ <@@ADDRESS@@>
# Created: @@DATE@@
# Version: 1.0
# Keywords: 

@@LICENSE@@

use strict;
use warnings;
use utf8;                     # ソースコードのencoding指定
use open ':locale';           # 暗黙に:stdも呼ばれる

#use open ':encoding(utf8)';  # I/OのencodingをUTF8固定にする
#use open ':std';             # STD三兄弟のencoding指定

#use encoding 'utf8';         # でも代用可能(use utf8以下)

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (moi::ask-license-string "# ")
@@@@

# @@FNAME@@ ends here
