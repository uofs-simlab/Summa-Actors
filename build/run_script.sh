#!/bin/bash
cd /Summa-Actors/build
make -f /Summa-Actors/build/makefile-container all
export LD_LIBRARY_PATH=/Summa-Actors/bin
cd /Summa-Actors/bin
./summaMain -s -c ../config/