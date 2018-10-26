#!/bin/bash

set -x

cd pgns

curl 'http://www.chessclub.org/all-games.php' > all-games.php

ALL_ZIPS="$(grep '\.zip' all-games.php | sed -e 's/.*<a href="\(.*\.zip\)">.*/\1/' | tail -n +2)"

for zip_file in $ALL_ZIPS; do
    NAME=$(basename "$zip_file")
    if [[ ! -f $NAME ]]; then
        curl "http://www.chessclub.org/${zip_file}" > $NAME
        unzip $NAME
    fi
done

rm -f all-games.php

curl 'http://www.chessclub.org/index.php' > index.php

CURRENT_TNM=$(grep -i '\.pgn' index.php | grep CurrentEvent | sed -e 's/.*<a href="\(.*\.PGN\)">.*/\1/')

for game in $CURRENT_TNM; do
    NAME=$(basename "$game")
    curl "http://www.chessclub.org/${game}" > $NAME
done

rm -f index.php
