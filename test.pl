#!/usr/bin/perl -w

use strict;

my ($addr, %sites);

# use a BEGIN block so we print our plan before MyModule is loaded

BEGIN {
    %sites = map { 
        open _; scalar <_>;
        chomp($addr = <_>);
        (substr($_, rindex($_, '/') + 1) => [$_, (split(':', $addr))[0]]);
    } map {
        glob("$_/OurNet/BBSAgent/*.bbs")
    } @INC;

}

use Test::Simple tests => scalar keys(%sites);

# Load BBS
use OurNet::BBSAgent;
use Socket 'inet_aton';

while (my ($k, $v) = each %sites) {
    my ($site, $addr) = @{$v};

    if (defined inet_aton($addr)) {
        if (eval{OurNet::BBSAgent->new($site, 10)}) {
	    ok(1, $addr);
	}
	else {
	    ok(1, "skip: $addr is down");
	}
    }
    else {
        ok(1, "skip: not connected to $addr");
    }
}

__END__
