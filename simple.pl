#!/usr/bin/env perl

use strict;
use warnings;

my %freq;
while ( my $line = <> ) {
    chomp $line;
    $line = lc $line;
    my @words = split ' ', $line;
    foreach my $w (@words) {
        $freq{ $w }++;
    }
}

my @sorted =
  map  { $_->[0] }
  sort { $a->[1] <=> $b->[1] }
  map  { [ $_, $freq{$_} ] }
  keys %freq;

foreach my $w ( reverse @sorted ) {
    printf "%s %d\n", $w, $freq{$w};
}
