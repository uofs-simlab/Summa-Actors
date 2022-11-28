#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --mem=32G
#SBATCH --constraint=broadwell
#SBATCH --job-name=Summa-Actors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/OUTPUT/FULL_NA_RUN/slurm/slurm-%A_%a.out
#SBATCH --account=rpp-kshook
#SBATCH --array=0-517

gru_max=517315
gru_count=1000
max_job=518
summa_exe=/home/kklenk/Summa-Projects/Summa-Actors/bin/summaMain
config_summa=/home/kklenk/projects/rpp-kshook/kklenk/INPUT/summa_settings/Summa_Actors_Settings.json
config_caf=/home/kklenk/Summa-Projects/Summa-Actors/bin/caf-application.conf

offset=$SLURM_ARRAY_TASK_ID
gru_start=$(( 1 + gru_count*offset ))
check=$(( $gru_start + $gru_count ))

# Adust the number of grus for the last job
if [ $check -gt $gru_max ] || \
       ( [ $job_check -lt $total_gru ] && \
         [ $offset -eq $(( max_job-1 )) ] )
then
    gru_count=$(( gru_max-gru_start+1 ))
fi

$summa_exe \
  -g $gru_start \
  -n $gru_count \
  -c $config_summa \
  --config-file=$config_caf
