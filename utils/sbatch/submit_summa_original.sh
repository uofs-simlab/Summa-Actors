#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --time=72:00:00
#SBATCH --mem=32G
#SBATCH --constraint=broadwell
#SBATCH --job-name=Summa-Actors
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/Sundials_Output/slurm/slurm-%A_%a.out
#SBATCH --account=rpp-kshook
#SBATCH --array=0-517

total_gru=517315
gru_per_job=1000
max_job=518
cpu_num=8
summa_exe=summa.exe
file_manager=fileManager.txt
log_dir=logs

offset=$SLURM_ARRAY_TASK_ID
gru_start=$(( 1 + gru_per_job*offset ))
job_check=$(( $gru_start + $gru_per_job ))

# Adust the number of grus for the last job
if [ $job_check -gt $total_gru ]
then
    gru_per_job=$(( gruMax-gru_start+1 ))
fi

gru_per_task=$(( gru_per_job/cpu_num ))

for ((cpu=0; cpu<cpu_num; cpu++))
do
    task_gru_start=$(( gru_start + cpu * gru_per_task ))
    task_check=$(( task_gru_start + gru_per_task ))

    if [ $task_check -gt $total_gru ] || \
           ( [ $task_check -lt $total_gru ] && \
             [ $cpu -eq $(( cpu_num-1 )) ] && \
             [ $offset -eq $(( max_job-1 )) ] )
    then
        gru_per_task=$(( total_gru - task_gru_start))
    fi

    $summa_exe \
        -g $task_gru_start $gru_per_task \
        -m $file_manager \
        > $log_dir/summa_log_$task_gru_start.txt &
done
wait
