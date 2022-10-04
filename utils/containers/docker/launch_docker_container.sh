#! /bin/bash

export PROJECT_DIR=/home/local/kck540/SUMMA-Projects/Summa-Distributed/Summa-Actors
export GLADWELL=/gladwell
docker run -d -it --name SUMMA-Actors --mount type=bind,source=${PROJECT_DIR},target=/Summa-Actors \
    --mount type=bind,source=${GLADWELL},target=/gladwell summa-actors:latest