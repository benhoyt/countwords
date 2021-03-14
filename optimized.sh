tr 'A-Z' 'a-z' | tr -s ' ' '\n' | LC_ALL=C sort -S 2G | uniq -c | sort -nr
