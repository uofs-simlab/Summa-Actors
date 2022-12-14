#!/bin/bash
#SBATCH --cpus-per-task=32
#SBATCH --time=02:00:00
#SBATCH --mem=32G
#SBATCH --job-name=Summa-Actors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Actors/slurm/Summa-Actors%A_%a.out
#SBATCH --account=hpc_c_giws_clark
#SBATCH --array=0-9

# offset=$SLURM_ARRAY_TASK_ID
# startGRU=$((offset*100+1))
# /globalhome/kck540/HPC/Summa-Projects/Summa-Actors/build/summa/bin/summa.exe -g $startGRU 100 -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/OutputTesting/fileManager_SummaOriginal.txt
# then
/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summa_actors -g 1 -n 10000 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json

