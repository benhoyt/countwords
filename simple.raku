my %counts is BagHash;
%counts.add($_.lc.words) for $*IN.lines;
.put for %counts.sort(*.value).reverse;
