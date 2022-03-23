#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --time=36:00:00
#SBATCH --mem=32G
#SBATCH --job-name=SummaActors-1-250
#SBATCH --mail-user=kyle.klenk@usask.ca
#SBATCH --mail-type=ALL
#SBATCH --output=/scratch/gwf/gwf_cmt/kck540/summaActors/slurm-Mar-21/1-250-8CPU-%A.out
#SBATCH --account=hpc_c_giws_clark

/globalhome/kck540/HPC/SummaActors/bin/summaMain -g 1 -c 250 -m /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/settings/SUMMA/fileManager.txt -v -8CPU --config-file=/globalhome/kck540/HPC/SummaActors/3_SummaActors_Run/copernicus/caf-application.conf
