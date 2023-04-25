#!/bin/bash

# NLP homework
# filter out unneeded words from an input that has one word per line

stopwordfile='./stopwords.txt'
while read; do
	echo "$REPLY" | grep -q -f - ${stopwordfile}
	if [[ "0" -ne "$?" ]]
	then
		echo "$REPLY"
	fi
done
