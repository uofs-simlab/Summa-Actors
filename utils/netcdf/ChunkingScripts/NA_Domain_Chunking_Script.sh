#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=1:15:00
#SBATCH --mem=20G
#SBATCH --job-name=Forcing_dataConversion
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/scratch/SummaActorsOutput/slurm/forcingdata-%A_%a.out
#SBATCH --account=def-spiteri_cpu

# ----------------------------------------------------------------------------------------------
# RUN WITH:
# sbatch --array1-[number of jobs] [script name]
# sbatch --array=0-100 run_all.sh
# ----------------------------------------------------------------------------------------------



YEAR=1979

offset=$SLURM_ARRAY_TASK_ID

start=$(( YEAR + offset ))

python3 /project/6008034/kklenk/NA_Domain_Chunking.py ${start}