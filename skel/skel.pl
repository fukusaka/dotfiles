#!/usr/bin/env perl
# -*- coding: utf-8 -*-
# file: @@FNAME@@
#
# Copyright (C) @@YEAR@@ @@NAME@@
#
# Author: @@NAME@@ <@@ADDRESS@@>
# Created: <@@DATE@@>
# Last Modified: <@@DATE@@>
#
@@LICENSE@@

use strict;
use warnings;

# ソースコードのencoding指定
use utf8;

use Carp;
use Data::Dump qw(dump);
use Data::Dumper qw(Dumper);
use English qw(-no_match_vars);

# I/OのencodingをUTF8固定にする
use open ':encoding(UTF-8)';
use open ':std';

# 安全じゃないBASH変数の削除
delete @ENV{qw(IFS CDPATH ENV BASH_ENV)};

# PATHの明示指定
$ENV{PATH} = q(/bin:/usr/bin);

# Dumperで任意の文字(UTF-8を含む)をエスケープしない方法
#{
#    package Data::Dumper;
#    our $Useqq = 1;
#    no warnings 'redefine';
#    sub qquote { my $s=shift; return "'$s'"; }
#}
# Dumper でハッシュキーをソートする方法
$Data::Dumper::Sortkeys = \sub { sort keys %{$_[0]}};

# Ex
#print Dumper(\$var);
#print Data::Dumper->Dump([$var $opt],[qw/var opt/]);

__END__

@@@@
FNAME: (file-name-nondirectory buffer-file-name)
NAME: programmer-name
ADDRESS: programmer-mail-address
YEAR: (format-time-string "%Y")
DATE: (format-time-string "%Y/%m/%d %H:%M:%S")
LICENSE: (my-ask-license-string "# ")
@@@@
