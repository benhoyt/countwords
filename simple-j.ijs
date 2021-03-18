#! /usr/bin/env ijconsole
load 'format/printf'
words=: a: -.~ ' ' splitstring (LF,' ') rplc~ tolower fread '/dev/stdin'
exit '%d %s' printf"1 \:~ (#;{.) /.~ words
