#!/usr/bin/perl

require 5.005;

use ExtUtils::MakeMaker;

WriteMakefile(
    AUTHOR          => 'Autrijus Tang (autrijus@autrijus.org)',
    ABSTRACT        => 'Scriptable telnet-based virtual users',
	NAME            => 'OurNet::BBSAgent',
	VERSION_FROM    => 'BBSAgent.pm',
    DISTNAME        => 'OurNet-BBSAgent',
    BINARY_LOCATION => 'x86/OurNet-BBSAgent.tar.gz',

    PREREQ_PM       => {
	    'Net::Telnet'  => '3.02',       # BBSAgent
    },

    dist            => {
        COMPRESS    => 'gzip',
        SUFFIX      => '.gz',
    }
);

# Fix a bug for Win95 nmake
if ($^O =~ /Win32/ and -e 'Makefile') {
    local $/;
    open _, 'Makefile';
    my $content = <_>;
    $content =~ s/\n\@\[\n/\n/g;
    $content =~ s/\n\]\n/\n\n/g;
    close _;
    open _, '>Makefile' or exit;
    print _ $content;
}


