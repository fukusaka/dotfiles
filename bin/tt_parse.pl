#!/usr/bin/env perl
use strict;
use warnings;
use Path::Class qw(file);
use Template::Parser;

my $file     = file(shift);
my $template = $file->slurp;
my $parser   = Template::Parser->new;
if (!$parser->parse($template)) {
    (my $error = $parser->error) =~ s/\s{2,}/ /g;
    printf 'file:%s %s', $file->absolute, $error;
}
