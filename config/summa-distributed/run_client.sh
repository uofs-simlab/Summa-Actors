#!/bin/bash
PORT = 4444
HOSTNAME = "simlab03"

export LD_LIBRARY_PATH=/Summa-Actors/bin/:/usr/local/lib
cd /Summa-Actors/config/
python3 configuration.py $HOSTNAME $PORT
cd /Summa-Actors/build
# make
/Summa-Actors/bin/summaMain -c /Summa-Actors/config/Summa_Actors_Settings.json