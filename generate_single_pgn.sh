#!/bin/bash

mkdir -p pgns
cd pgns

rm -f one_per_line.pgn

cat $(ls -c *.pgn *.PGN) > all_games.pgn
cat all_games.pgn | tr '\r\n' ' ' > oneline.pgn
echo >> oneline.pgn

cat oneline.pgn | sed -e 's/\[Event \"/\n[Event \"/g' > one_per_line.pgn

rm -f all_games.pgn
rm -f oneline.pgn
