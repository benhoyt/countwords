#!/usr/bin/env tclsh
fconfigure stdin -buffering line
while {[gets stdin data] >= 0} {
  foreach word [string tolower [split $data]] {
    incr table($word)
  }
}
foreach {word count} [lsort -decreasing -integer -index 1 -stride 2 [array get table]] {
  puts "$word $count"
}
