#!/usr/bin/perl

$sockfile = "/tmp/mppps";

$logfile = ">>/tmp/mppps.log";

$AF_UNIX = 1;
$SOCK_STREAM = 1;

$sockaddr = 'S a14';

$this = pack($sockaddr, $AF_UNIX,$sockfile);


if ( -e $sockfile ) {
    exit if connect(CS,$this);
    unlink($sockfile);
}

open(logfile) || die "open: $!";

socket(S,$AF_UNIX,$SOCK_STREAM,0) || die "socket: $!";

bind(S,$this) || die "bind: $!";
listen(S,5) || die "listen: $!";

select(S); $|=1;
select(NS); $|=1;
select(logfile); $|=1;
select(STDOUT);

print logfile "mpppx.pl [$$] $^T: boot\n";
open(tmp,">~/.mpppx.pid") && print tmp $$ && close(tmp);

for (;;) {
    ($addr = accept(NS,S)) || die "$!";

    if (($child = fork()) == 0) {
	setpgrp(0,$$);
	while (<NS>) {
	    print logfile $_;
	}
	close(NS);
	exit;
    }
    close(NS);
}

close(S);
unlink($sockfile);
