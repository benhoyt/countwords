#!/usr/bin/perl -w
# I'm with Doug McIlroy.
# Write this once in a few mins, and optimize
# next year if necessary (if ever).
# See the chapter on Efficiency in Programming Perl.
use strict;

my %WORDFREQ;
while (my $line = <>) {
    chomp($line);
    foreach my $w (split(' ', lc($line))) {
        $WORDFREQ{$w}++;
    }
}

foreach my $w (sort { $WORDFREQ{$b} <=> $WORDFREQ{$a} } keys %WORDFREQ) {
    print "$w $WORDFREQ{$w}\n";
}
