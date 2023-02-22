#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=02:00:00
#SBATCH --mem=32G
#SBATCH --job-name=Summa-Actors
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Actors/slurm/Summa-Actors%A_%a.out
#SBATCH --account=hpc_c_giws_clark

/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summa_actors -g 1 -n 50 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json \
 --config-file=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/caf-application.conf

