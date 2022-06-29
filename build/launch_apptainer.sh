#!/bin/bash
singularity shell --bind /globalhome/kck540/HPC/SummaProjects/Summa-Actors:/Summa-Actors \
    --bind /scratch/gwf/gwf_cmt/kck540/SummaOutput/SummaActors:/output \
    --bind /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/settings/SUMMA:/SUMMA \
    --bind /project/gwf/gwf_cmt/kck540/domain_NorthAmerica/forcing/SummaChunkedData/:/forcing \
    summa-actors.sif
