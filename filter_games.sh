#!/bin/bash

NAME=$1
COLOR=$2

if [[ $COLOR != 'white' && $COLOR != 'black' ]]; then
    echo '2nd argument has to be white or black'
    exit 1
fi

if [[ $COLOR = 'white' ]]; then
    FILTERS=''
else
    FILTERS='1. e4'
    NON_FILTERS='1. e4 e5 2.'
fi

cat pgns/one_per_line.pgn | grep --color -i "$COLOR \"$NAME" | grep -F "$FILTERS"  > filtered.pgn

cat filtered.pgn | sed -e 's/\]/]\n/g' -e 's/\[Event/\n[Event/' | sed -e 's/^ *//' | tail -n +2 > result.pgn

rm -f filtered.pgn
