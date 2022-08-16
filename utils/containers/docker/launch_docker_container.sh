#! /bin/bash

export PROJECT_DIR=/Users/kyleklenk/SUMMA-Projects/Summa-Actors
export NA_TEST=/home/local/kck540/NA_Summa_Test
export SUMMA=/Users/kyleklenk/SUMMA-Projects/summa
docker run -d -it --name SUMMA-Actors --mount type=bind,source=${PROJECT_DIR},target=/Summa-Actors \
    --mount type=bind,source=${SUMMA},target=/SUMMA summa-actors:latest
    # --mount type=bind,source=${NA_TEST},target=/NA_Test \
    # \