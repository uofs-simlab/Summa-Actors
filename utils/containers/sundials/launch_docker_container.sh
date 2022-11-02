#! /bin/bash

# Program Locations to bind
export SUMMA_ACTORS=/Users/kyleklenk/SUMMA-Projects/Summa-Actors
export SUMMA=/Users/kyleklenk/SUMMA-Projects/Summa-Sundials/summa
export CONTAINTER_NAME="Summa-Sundials"
docker run -d -it --name ${CONTAINTER_NAME} --mount type=bind,source=${SUMMA_ACTORS},target=/Summa-Actors \
    --mount type=bind,source=${SUMMA},target=/SUMMA summa-sundials:latest
