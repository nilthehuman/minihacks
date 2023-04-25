#!/bin/bash

# NLP homework
# trims parenthesized substrings from an input string and outputs the rest

parens_opened=0
buffers=()
while read -n1; do
	if [[ '(' == $REPLY ]]
	then
		buffers+=('')
		parens_opened=$(($parens_opened+1))
	fi
	if [[ 0 -eq $parens_opened ]]
	then
		echo -n "$REPLY"
	else
		buffers[-1]="${buffers[-1]}${REPLY}"
	fi
	if [[ ')' == $REPLY ]]
	then
		buffers=( "${buffers[@]:0:${#buffers[@]}-1}" )
		if [[ 0 -lt $parens_opened ]]
		then
			parens_opened=$(($parens_opened-1))
		fi
	fi
done
printf "%s" "${buffers[@]}"
