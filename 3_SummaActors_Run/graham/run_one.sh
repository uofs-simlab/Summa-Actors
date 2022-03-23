#!/bin/bash
#SBATCH --cpus-per-task=4
#SBATCH --time=02:00:00
#SBATCH --mem=8G
#SBATCH --job-name=TEST_summa_na_grid_era5
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/SummaActorsOutput/slurm/slurm-%A.out
#SBATCH --account=rpp-kshook

/home/kklenk/SummaProjects/SummaActors/bin/summaMain -g 1 -c 100 -m /project/6008034/kklenk/settings/SummaActorsSettings/fileManager.txt
1