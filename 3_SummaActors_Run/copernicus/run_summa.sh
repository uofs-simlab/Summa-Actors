#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --time=71:00:00
#SBATCH --mem=24G
#SBATCH --job-name=SummaActorsNA
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/project/gwf/gwf_cmt/kck540/domain_NorthAmerica/simulations/slurm/slurm-%A_%a.out
#SBATCH --account=hpc_c_giws_clark

# Ensure array size is correct ie. --array=1-[numJobs]
#SBATCH --array=0-2499

JOBLIST=/globalhome/kck540/HPC/SummaActors/3_MS_model_runs/run_graham_serialSumma_NA_gridEra5_1/copernicus/joblist/summa_joblist.txt


# Make the log directory in case 
mkdir -p logs/
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