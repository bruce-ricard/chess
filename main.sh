#!/bin/bash

echo 'Downloading new games'
./download_mechanics_games.sh > /dev/null 2> /dev/null

./generate_single_pgn.sh

echo $'Filtering out oponent\'s games'
./filter_games.sh "$1" "$2"

echo 'Done! The games are in the file result.pgn'
