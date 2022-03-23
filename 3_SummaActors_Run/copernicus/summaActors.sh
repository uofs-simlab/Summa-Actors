#!/bin/bash
#SBATCH --cpus-per-task=6
#SBATCH --time=24:00:00
#SBATCH --mem=24G
#SBATCH --job-name=SummaActors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/summaActors/slurm/slurm-%A.out
#SBATCH --account=hpc_c_giws_clark

/globalhome/kck540/HPC/SummaActors/bin/summaMain -g 1000 -c 518
