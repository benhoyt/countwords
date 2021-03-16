#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
set -o noglob
shopt -s lastpipe

# Count frequencies
declare -iA words_to_freq
while read -r line; do
	lowercase_line="${line,,}"
	for word in $lowercase_line; do
		words_to_freq["${word}"]+=1
	done
done

# Create a map of unique frequences to tab deliminted words
declare -A freq_to_words
for word in "${!words_to_freq[@]}"; do
	freq="${words_to_freq["${word}"]}"
	if [[ -v "freq_to_words[${freq}]" ]]; then
		freq_to_words["${freq}"]+=$'\t'"${word}"
	else
		freq_to_words["${freq}"]="${word}"
	fi
done

# Create array to sort
declare -ia freq_sorted
declare -i i=0
for freq in "${!freq_to_words[@]}"; do
	freq_sorted[$i]=$freq
	i+=1
done

# Sort frequences
# https://en.wikipedia.org/wiki/Insertion_sort
declare -i j
for ((i = 1; i < ${#freq_sorted[@]}; i++)); do
	x=${freq_sorted[$i]}
	j=$((i - 1))
	while ((j >= 0)) && ((freq_sorted[j] < x)); do
		freq_sorted[$j + 1]=${freq_sorted[$j]}
		j=$((j - 1))
	done
	freq_sorted[$j + 1]=$x
done

# Print
for freq in "${freq_sorted[@]}"; do
	IFS=$'\t' read -ra words <<<"${freq_to_words[$freq]}"
	for word in "${words[@]}"; do
		printf "%s %d\n" "${word}" "${freq}"
	done
done
