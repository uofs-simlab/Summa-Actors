#!/bin/bash
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --time=1:00:00
#SBATCH --mem=0
#SBATCH --job-name=Summa-Actors
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Sundials-Output/slurm/slurm-%A.out
#SBATCH --account=hpc_c_giws_clark

module load StdEnv/2020
module load gcc/9.3.0
module load openblas/0.3.17
module load netcdf-fortran/4.5.2

# for Actors
module load caf

export LD_LIBRARY_PATH=/globalhome/kck540/HPC/Libraries/sundials/v6.6/instdir/lib64:$LD_LIBRARY_PATH


gru_max=12000
gru_count=25
max_job=3
summa_exe=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summa_actors
# config_summa=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/config.json
file_manager=/project/gwf/gwf_cmt/kck540/domain_NorthAmerica/Summa-Projects/input_data/summa_actors_input/fileManager.txt
config_summa=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/Summa_Actors_Settings.json

offset=$SLURM_ARRAY_TASK_ID
gru_start=$(( 1 + gru_count*offset ))
check=$(( $gru_start + $gru_count ))

# Adust the number of grus for the last job
# if [ $check -gt $gru_max ] || [ $offset -eq $(( max_job-1 )) ] 
# then
#     gru_count=$(( gru_max-gru_start+1 ))
# fi

$summa_exe -g $gru_start $gru_count -c $config_summa --caf.scheduler.max-threads=32