#!/bin/bash
  
# build on Copernicus or Graham, from cmake directory run this as ./build_actors.cluster.bash
# for Summa
module load gcc/9.3.0
module load netcdf-fortran
module load openblas
module load caf

export SUNDIALS_PATH="/globalhome/kck540/HPC/Libraries/sundials/instdir"

cmake -B ../cmake_build -S . -DCMAKE_BUILD_TYPE=Actors_Sundials_Cluster
cmake --build ../cmake_build --target all

