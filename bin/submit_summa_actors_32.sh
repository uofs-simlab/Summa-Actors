#!/bin/bash
#SBATCH --cpus-per-task=32
#SBATCH --nodes=1
#SBATCH --time=92:00:00
#SBATCH --mem=0
#SBATCH --job-name=Summa-Actors
#SBATCH --output=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/Output/slurm-run-1-%A_%a.out
#SBATCH --account=hpc_c_giws_clark
# SBATCH --array=0-2

module load StdEnv/2020
module load gcc/9.3.0
module load openblas/0.3.17
module load netcdf-fortran/4.5.2

# for Actors
module load caf

export LD_LIBRARY_PATH=/globalhome/kck540/HPC/Libraries/sundials/instdir/lib64:$LD_LIBRARY_PATH


gru_max=12000
gru_count=4000
max_job=3
summa_exe=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summa_actors
config_summa=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/Summa_Actors_Settings.json

offset=$SLURM_ARRAY_TASK_ID
gru_start=$(( 1 + gru_count*offset ))
check=$(( $gru_start + $gru_count ))

# Adust the number of grus for the last job
if [ $check -gt $gru_max ] || [ $offset -eq $(( max_job-1 )) ] 
then
    gru_count=$(( gru_max-gru_start+1 ))
fi

$summa_exe \
        -g $gru_start \
        -n $gru_count \
        -c $config_summa \
        --caf.scheduler.max-threads=$SLURM_CPUS_PER_TASK