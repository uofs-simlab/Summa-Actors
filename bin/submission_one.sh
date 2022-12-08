#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=01:00:00
#SBATCH --mem=2G
#SBATCH --job-name=Summa-Actors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Actors/slurm/Summa-Actors%A_%a.out
#SBATCH --account=hpc_c_giws_clark
#SBATCH --array=0-1

offset=$SLURM_ARRAY_TASK_ID
if [ $offset -eq 0 ]
then
    /globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summaMain -g 1 -n 15 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json

else
    /globalhome/kck540/HPC/Summa-Projects/Summa-Actors/build/summa/bin/summa.exe -g 1 15 -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/OutputTesting/fileManager_SummaOriginal.txt

fi
# /globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summaMain -g 1 -n 1 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json
