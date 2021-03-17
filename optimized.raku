my %counts is default(0);
for open($*IN, :enc<ascii>).lines(:close) {
  for .lc.words -> \w {
    %counts.AT-KEY(w) == 0 ?? %counts.STORE_AT_KEY(w, 1) !! ++%counts.AT-KEY(w)
  }
}
# From codesections on SO : https://stackoverflow.com/a/66500447
sub my-reverse(\a, \b) { a.value > b.value ?? False !! True }
%counts.sort(&my-reverse).fmt('%s',"\n").print;
