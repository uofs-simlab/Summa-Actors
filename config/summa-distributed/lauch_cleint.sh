#!/bin/bash


nohup apptainer exec --bind /gladwell:/gladwell \
     --bind /scratch:/scratch \
     --bind /home/kck540/Summa-Actors:/Summa-Actors \
     /gladwell/kck540/container_files/summa_actors.sif \
     /Summa-Actors/config/summa-distributed/run_client.sh 2>&1 > \
     /scratch/kck540/Summa_Distributed_test/logs/`hostname`-client.out &