#!/bin/bash
  
# build on Copernicus or Graham, from cmake directory run this as ./build_actors.cluster.bash
# for Summa
module load StdEnv/2020
module load gcc/9.3.0
module load openblas/0.3.17
module load netcdf-fortran/4.5.2
module load caf # actors

export CMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH:/home/kklenk/Libraries/sundials"

cmake -B ./cmake_build -S ../summa/build/. -DUSE_ACTORS=ON -DUSE_SUNDIALS=ON
cmake --build ./cmake_build --target all -j