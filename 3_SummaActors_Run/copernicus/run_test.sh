#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=0:30:00
#SBATCH --mem=250M
#SBATCH --job-name=SummaActors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/summaActors/slurm/slurm-%A_%a.out
#SBATCH --account=hpc_c_giws_clark


/globalhome/kck540/HPC/SummaActors/bin/summaTest -g 1 -c 4
