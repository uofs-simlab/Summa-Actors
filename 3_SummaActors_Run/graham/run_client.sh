#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=0:30:00
#SBATCH --mem=2G
#SBATCH --job-name=Summa-StressTest
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/SummaActorsOutput/slurm/slurm-client%A_%a.out
#SBATCH --account=rpp-kshook

# ----------------------------------------------------------------------------------------------
# RUN WITH:
# sbatch --array1-[number of jobs] [script name]
# 
# EXAMPLE: sbatch --array=0-100 run_all.sh
# ----------------------------------------------------------------------------------------------

/home/kklenk/SummaProjects/SummaActors/bin/summaTest -g 1 -c 100