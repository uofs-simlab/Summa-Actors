#!/bin/bash


apptainer exec --bind /gladwell:/gladwell \
     --bind /scratch:/scratch \
     --bind /home/kck540/Summa-Actors:/Summa-Actors \
     /gladwell/kck540/container_files/summa_actors.sif \
     /Summa-Actors/config/summa-distributed/run_server.sh > /scratch/kck540/Summa_Distributed_test/logs/`hostname`-server.out