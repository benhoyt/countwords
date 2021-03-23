#!/usr/bin/env tclsh
fconfigure stdin -buffering line
while {[gets stdin data] >= 0} {
  foreach word [split [string tolower $data]] {
    incr table($word)
  }
}
# Remove the count of null elements resulting from the split function
# being applied to consecutive whitespace in the input
catch {unset table()} err
foreach {word count} [lsort -decreasing -integer -index 1 -stride 2 [array get table]] {
  puts "$word $count"
}
