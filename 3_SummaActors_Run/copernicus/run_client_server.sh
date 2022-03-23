#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=8:00:00
#SBATCH --mem=2G
#SBATCH --job-name=SummaActorsOutputBugTest
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/summaActors/slurm/slurm-%A_%a.out
#SBATCH --account=hpc_c_giws_clark

# EXAMPLE: sbatch --array=0-50 run_server_client.sh

gruMax=517315 # North America, Merit Hydro basins
gruCount=50000 # The number of GRUs you want to compute
startGRU=1   # The starting index of the GRU you would like to compute
offset=$SLURM_ARRAY_TASK_ID

if [ $offset -eq 0 ]
then
    /globalhome/kck540/HPC/SummaActors/bin/summaMain -s -p 4444 -g 1 -c $gruCount -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/settings/SUMMA/fileManager.txt
else
    /globalhome/kck540/HPC/SummaActors/bin/summaMain -p 4444 -j $offset -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/settings/SUMMA/fileManager.txt
fi
