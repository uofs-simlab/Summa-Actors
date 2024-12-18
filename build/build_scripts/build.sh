#!/bin/bash

# If compiling on a Digital Research Alliance of Canada cluster,
# load the following modules:
# module load StdEnv/2020
# module load gcc/9.3.0
# module load openblas/0.3.17
# module load netcdf-fortran/4.5.2

# If compiling on Anvil, load the following modules:
# module load gcc/11.2.0 
# module load openblas 
# module load openmpi 
# module load netcdf-fortran


# If a library cannot be found by Cmake, you can specify the path like so:
export CMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH:/u1/kck540/Projects/hydrology/Summa-Actors/utils/dependencies/caf/"
export CMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH:/u1/kck540/Projects/hydrology/Summa-Actors/utils/dependencies/sundials/"


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

cmake -B ./cmake_build -S .. -DUSE_SUNDIALS=ON -DCMAKE_BUILD_TYPE=Debug
cmake --build ./cmake_build --target all -j
