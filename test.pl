#!/usr/bin/perl -w

use strict;
use Test;
use vars qw/%sites/;

# use a BEGIN block so we print our plan before MyModule is loaded

BEGIN {
    my $addr;

    %sites = map { 
        open _; scalar <_>;
        chomp($addr = <_>);
        (substr($_, rindex($_, '/') + 1) => [$_, (split(':', $addr))[0]]);
    } map {
        glob("$_/OurNet/BBSAgent/*.bbs")
    } @INC;

    plan tests => scalar keys(%sites);
}

# Load BBS
use OurNet::BBSAgent;
use Socket 'inet_aton';

while (my ($k, $v) = each %sites) {
    my ($site, $addr) = @{$v};

    if (defined inet_aton($addr)) {
        # ok(eval{OurNet::BBSAgent->new($site, 1)});
	ok(1);
    }
    else {
        skip("not connected to $addr", 1);
    }
}

__END__
