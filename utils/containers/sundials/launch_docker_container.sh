#! /bin/bash

export PROJECT_DIR=/Users/kyleklenk/SUMMA-Projects/Summa-Actors
export NA_TEST=/home/local/kck540/NA_Summa_Test
export SUMMA=/Users/kyleklenk/SUMMA-Projects/Summa-Sundials/summa
docker run -d -it --ulimit memlock=32768:32768 --name SUMMA-Sundials --mount type=bind,source=${PROJECT_DIR},target=/Summa-Actors \
    --mount type=bind,source=${SUMMA},target=/SUMMA summa-sundials:latest
