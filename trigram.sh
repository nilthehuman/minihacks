#!/bin/bash

# NLP homework
# spit out all "three consecutive words" bits of the input on separate lines

function shifted_echo {
    echo "$REPLY" | sed 's/\W/\n/g' | grep -v '^$' | tail -n+"$1"
}

read
paste --delimiters=, <(shifted_echo 1) <(shifted_echo 2) <(shifted_echo 3) | head -n-2
