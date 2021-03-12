tr 'A-Z' 'a-z' | tr -s ' ' '\n' | sort | uniq -c | sort -nr
