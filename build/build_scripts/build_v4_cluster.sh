#!/bin/bash
  
# build on Copernicus or Graham, from cmake directory run this as ./build_actors.cluster.bash
# for Summa
module load StdEnv/2020
module load gcc/9.3.0
module load openblas/0.3.17
module load netcdf-fortran/4.5.2
module load caf # actors

export FLAGS_OPT="-flto=1;-fuse-linker-plugin"
export SUNDIALS_PATH=/globalhome/kck540/HPC/Libraries/sundials/v7.0/instdir

cmake -B ./cmake_build -S ../summa/build/cmake/. -DCMAKE_BUILD_TYPE=Sundials_Actors_Cluster
cmake --build ./cmake_build --target all -j 
