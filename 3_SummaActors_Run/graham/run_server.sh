#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=1G
#SBATCH --job-name=Summa-StressTest
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/SummaActorsOutput/slurm/slurmServer-%A_%a.out
#SBATCH --account=rpp-kshook

/home/kklenk/SummaProjects/SummaActors/bin/summaMain -s -p 4444 -g 1 -c 1000 -m /project/6008034/kklenk/settings/SummaActorsSettings/fileManager.txt