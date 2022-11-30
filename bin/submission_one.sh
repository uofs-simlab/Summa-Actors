#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=01:00:00
#SBATCH --mem=1G
#SBATCH --job-name=Summa-Actors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Actors/slurm/Summa-Actors%A.out
#SBATCH --account=hpc_c_giws_clark

/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summaMain -g 1 -n 10 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json

/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/build/summa/bin/summa.exe -g 1 10 -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/fileManager.txt