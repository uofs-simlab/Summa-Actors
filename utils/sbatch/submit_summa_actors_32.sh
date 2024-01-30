#!/bin/bash
#SBATCH --nodes=1
#SBATCH --exclusive
#SBATCH --time=48:00:00
#SBATCH --mem=0
#SBATCH --job-name=Summa-Actors
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/Summa-Global-Actors-Output/Africa/slurm/slurm-%A_%a.out
#SBATCH --account=hpc_c_giws_clark


module load StdEnv/2020
module load gcc/9.3.0
module load openblas/0.3.17
module load netcdf-fortran/4.5.2

# for Actors
module load caf

gru_count=220000
max_job=3
total_hru=648266
summa_exe=/globalhome/kck540/HPC/Summa-Projects/Summa-Actors/bin/summa_be
config_summa=/project/gwf/gwf_cmt/kck540/Summa-Global-Actors-Input/Africa/settings_summa/Copernicus_Summa_Actors_Config.json

# offset=$SLURM_ARRAY_TASK_ID
# gru_start=$(( 1 + gru_count*offset ))
# check=$(( $gru_start + $gru_count ))

# $summa_exe -g $gru_start $gru_count -c $config_summa --caf.scheduler.max-threads=32
$summa_exe -g 648001 266 -c $config_summa --caf.scheduler.max-threads=32
