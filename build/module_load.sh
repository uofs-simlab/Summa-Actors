#!/bin/bash

#### load modules if using Compute Canada or Copernicus ####
module load gcc/9.3.0
module load netcdf-fortran
module load openblas
module load caf

export LD_LIBRARY_PATH=/globalhome/kck540/HPC/SummaProjects/Summa-Actors/bin