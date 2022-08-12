#! /bin/bash

export PROJECT_DIR=/Users/kyleklenk/SUMMA-Projects/Summa-Actors
export NA_TEST=/home/local/kck540/NA_Summa_Test
export SUMMA=/home/local/kck540/SUMMA-Projects/summa-reference/summa
docker run -d -it --name SUMMA-Actors --mount type=bind,source=${PROJECT_DIR},target=/Summa-Actors summa-actors:latest
    # --mount type=bind,source=${NA_TEST},target=/NA_Test \
    # --mount type=bind,source=${SUMMA},target=/SUMMA \