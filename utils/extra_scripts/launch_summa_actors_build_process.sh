#!/bin/bash
#############
## File in the path that SUMMA-Actors top level directory is located in
###############

SUMMA_ACTORS=/home/kklenk/Summa-Projects/Summa-Actors # top level directory

apptainer exec --bind $SUMMA_ACTORS:/Summa-Actors summa_actors.sif /Summa-Actors/build/build_summa_actors_container.sh