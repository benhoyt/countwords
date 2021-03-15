#!/usr/bin/perl -w
# I'm with Doug McIlroy.
# Write this once in a few mins, and optimize
# next year if necessary (if ever).
# See the chapter on Efficiency in Programming Perl.
use strict;

my %WORDFREQ;
while (my $line = <>) {
    chomp $line;
    $line = lc($line);
    my @words = split(' ', $line);
    foreach my $w (@words) {
	$WORDFREQ{$w}++;
    }
}

my @freq_order = sort { $WORDFREQ{$b} <=> $WORDFREQ{$a} } keys %WORDFREQ;

foreach my $w (@freq_order) {
    print "$w $WORDFREQ{$w}\n";
}
