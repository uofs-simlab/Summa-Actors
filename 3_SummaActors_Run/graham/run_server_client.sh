#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=24:00:00
#SBATCH --mem=2G
#SBATCH --job-name=SummaActors-StressTest
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/SummaActorsOutput/slurm/slurm-%A_%a.out
#SBATCH --account=def-spiteri

# ----------------------------------------------------------------------------------------------
# RUN WITH:
# sbatch --array1-[number of jobs] [script name]
# 
# EXAMPLE: sbatch --array=0-50 run_server_client.sh
# ----------------------------------------------------------------------------------------------

gruMax=517315 # North America, Merit Hydro basins
gruCount=10000 # The number of GRUs you want to compute
startGRU=1   # The starting index of the GRU you would like to compute
offset=$SLURM_ARRAY_TASK_ID

if [ $offset -eq 0 ]
then
    /home/kklenk/SummaProjects/SummaActors/bin/summaMain -s -p 4444 -g 1 -c $gruCount -m /project/6008034/kklenk/settings/SummaActorsSettings/fileManager.txt
else
    /home/kklenk/SummaProjects/SummaActors/bin/summaMain -p 4444 -j $offset -m /project/6008034/kklenk/settings/SummaActorsSettings/fileManager.txt
fi
