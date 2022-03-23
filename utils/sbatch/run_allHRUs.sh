#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --mem=32G
#SBATCH --job-name=SummaActorsOutputBugTest
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/summaActors/slurm/slurm-%A_%a.out
#SBATCH --array=0-258%4
#SBATCH --account=hpc_c_giws_clark

# EXAMPLE: sbatch --array=0-258%4 run_allHRUs.sh

# Define the GRU settings
gruMax=517315 # North America, Merit Hydro basins
gruCount=2000

# Get the array ID for further use
offset=$SLURM_ARRAY_TASK_ID 

# Start at 1 for array ID 1, 2022 for array ID 2, etc
gruStart=$(( 1 + gruCount*offset ))
check=$(( $gruStart + $gruCount ))
# Check that we don't specify too many basins
if [ $check -gt $gruMax ]
then
    gruCount=$(( gruMax-gruStart+1 ))
fi

/globalhome/kck540/HPC/SummaActors/bin/summaMain -g ${gruStart} -c ${gruCount} -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/settings/SUMMA/fileManager.txt