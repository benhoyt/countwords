{
    $0 = tolower($0)
    for (i = 1; i <= NF; i++)
        counts[$i]++
}

END {
    for (k in counts)
        print k, counts[k] | "sort -nr -k2"
}
