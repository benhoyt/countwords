#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail
shopt -s lastpipe

# Count frequencies
OLD_LANG=$LANG
LANG='C'
declare -iA words_to_freq
eof='false'
set -o noglob
while [[ "${eof}" == 'false' ]]; do
	if ! IFS='' read -N 65536 -r b; then
		eof='true'
	fi
	if ! IFS='' read -r l; then
		eof='true'
	fi
	for word in ${b,,}${l,,}; do
		words_to_freq[$word]+=1
	done
done
set +o noglob
LANG=$OLD_LANG

# Construct a sparse array with outputs for each freq
declare -a output
for word in "${!words_to_freq[@]}"; do
	freq=${words_to_freq[$word]}
	output[freq]+=$word' '$freq$'\n'
done

# Compact array, then reverse
output=("${output[@]}")
((i = 0, j = ${#output[@]} - 1))
while ((j >= 0)); do
	r[i++]=${output[j--]}
done

printf '%s' "${r[@]}"
