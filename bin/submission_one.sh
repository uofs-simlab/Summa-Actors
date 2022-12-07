#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --time=01:00:00
#SBATCH --mem=32G
#SBATCH --job-name=Summa-Actors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Actors/slurm/Summa-Actors%A_%a.out
#SBATCH --account=hpc_c_giws_clark

# offset=$SLURM_ARRAY_TASK_ID
# if [ $offset -eq 0 ]
# then
#     /globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summaMain -g 1 -n 1 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json

# else
#     /globalhome/kck540/HPC/Summa-Projects/Summa-Actors/build/summa/bin/summa.exe -g 1 1 -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/OutputTesting/fileManager_SummaOriginal.txt

# fi
/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summaMain -g 1 -n 1500 -c /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/summa_actors_input/Summa_Actors_Settings.json
