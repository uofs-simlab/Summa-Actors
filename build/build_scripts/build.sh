#!/bin/bash

# If compiling on a Digital Research Alliance of Canada cluster,
# load the following modules:
# module load StdEnv/2020
# module load gcc/9.3.0
# module load openblas/0.3.17
# module load netcdf-fortran/4.5.2

# -----------------------------------
# If compiling V3 use the folowing
# -----------------------------------
# cmake -B ./cmake_build -S ..
# cmake --build ./cmake_build --target all -j

# -----------------------------------
# If compiling V4 without sundials use the folowing
# -----------------------------------
  
# cmake -B ./cmake_build -S .. -DUSE_V4=ON
# cmake --build ./cmake_build --target all -j

# -----------------------------------
# If compiling V4 with sundials use the folowing (default)
# -----------------------------------

cmake -B ./cmake_build -S .. -DUSE_SUNDIALS=ON
cmake --build ./cmake_build --target all -j
