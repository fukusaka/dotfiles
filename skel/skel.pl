#!/usr/bin/perl -T
# -*- coding: utf-8 -*-
#
# Copyright (C) @@YEAR@@ @@NAME@@
#
# Author: @@NAME@@ <@@ADDRESS@@>
# Created: @@DATE@@
# Version: 1.0
# Keywords:
#
@@LICENSE@@

use strict;
use warnings;
use utf8;                     # ソースコードのencoding指定
use open ':encoding(utf8)';   # I/OのencodingをUTF8固定にする
use open ':std';              # STD三兄弟のencoding指定
#use Data::Dumper;

# 安全じゃないBASH変数の削除
delete @ENV{qw(IFS CDPATH ENV BASH_ENV)};

# PATHの明示指定
$ENV{PATH} = q(/bin:/usr/bin);

# @@FNAME@@ ends here

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%d %b %Y")
LICENSE: (my-ask-license-string "# ")
@@@@
