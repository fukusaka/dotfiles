#!/usr/bin/perl

$sockfile = "/tmp/mppps";

$AF_UNIX = 1;
$SOCK_STREAM = 1;

$sockaddr = 'S a14';

$that = pack($sockaddr, $AF_UNIX,$sockfile);

socket(S,$AF_UNIX,$SOCK_STREAM,0) || die "socket: $!";

connect(S,$that) || die "connect: $!";

$msg = $ARGV;

select(S); $| = 1; select(STDOUT);

while (<>) {
    print S $_;
}

close(S);
