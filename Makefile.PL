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
	'Net::Telnet'	=> '3.02',	# BBSAgent
	'Test::Simple'	=> '0.01',	# test script
    },

    dist            => {
	COMPRESS    => 'gzip',
	SUFFIX      => '.gz',
    }
);
