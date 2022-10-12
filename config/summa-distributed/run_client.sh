#!/bin/bash
export LD_LIBRARY_PATH=/Summa-Actors/bin/:/usr/local/lib
cd /Summa-Actors/config/
python3 configuration.py "simlab03" 4444
cd /Summa-Actors/build
# make
/Summa-Actors/bin/summaMain -c /Summa-Actors/config/Summa_Actors_Settings.json