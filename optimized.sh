export LC_ALL=C
tr 'A-Z' 'a-z' | tr -s ' ' '\n' | sort -S 2G | uniq -c | sort -nr
