#! /usr/bin/env ijconsole
require 'format/printf'
9!:37 ] 0 _ 0 _
m =: 256 $ 0
m =: 1 (,(a.i.'Aa')+/i.26) }m
s =: _2]\"1 }.".;._2 (0 : 0)
'  X   L  ']0
  0 0 1 1  NB. other
  0 2 1 0  NB. letter
)
words=: (0;s;m) ;: tolower fread '/dev/stdin'
exit '%d %s' echo@sprintf \:~ (#;{.) /.~ words
