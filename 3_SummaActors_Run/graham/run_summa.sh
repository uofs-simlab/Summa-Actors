#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=96:00:00
#SBATCH --mem=1G
#SBATCH --job-name=TEST_summa_na_grid_era5
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/home/kklenk/slurmlog/slurm-%A_%a.out
#SBATCH --account=rpp-kshook

# Ensure array size is correct ie. --array=1-[numJobs]
#SBATCH --array=1-10

JOBLIST=/home/kklenk/actors/summa/code/graham/3_MS_model_runs/run_graham_serialSumma_NA_gridEra5_1/joblist/summa_joblist.txt


# Make the log directory in case 
mdkir -p logs/
# ----------------------------------------------------------------------------------------------
# RUN WITH
# sbatch run_summa.sh
# ----------------------------------------------------------------------------------------------

JOBSTRING=$(sed -n "${SLURM_ARRAY_TASK_ID}p" $JOBLIST)

# Seperate the SUMA call and its agruments from the ">" delimiter
OIFS=$IFS
IFS=">"
read -ra COMMAND <<< "$JOBSTRING"
IFS=$OIFS

#--------------------------------------------------------------------
# RUN
#--------------------------------------------------------------------
echo "Starting run at: `date`"

${COMMAND[0]} > ${COMMAND[1]}

# Echo end-of-run
echo "Program finished with exit code $? ar: `date`"

${JOBSTRING}