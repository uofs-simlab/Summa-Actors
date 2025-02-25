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

# -----------------------------------

# Compiling the LATEST version of the code
# -----------------------------------
INSTALL_DIR=$PWD/../../utils/dependencies/install
export CMAKE_PREFIX_PATH="$INSTALL_DIR/sundials:$INSTALL_DIR/caf:$INSTALL_DIR/netcdf-fortran:$INSTALL_DIR/netcdf-c:$CMAKE_PREFIX_PATH"

cmake -B ./cmake_build -S .. -DUSE_SUNDIALS=ON -DCMAKE_BUILD_TYPE=Release
cmake --build ./cmake_build --target all -j


# -----------------------------------
# If compiling V4 without sundials use the following
# -----------------------------------
  
# cmake -B ./cmake_build -S .. -DUSE_V4=ON
# cmake --build ./cmake_build --target all -j

# -----------------------------------
# If compiling V3 use the following
# -----------------------------------
# cmake -B ./cmake_build -S ..
# cmake --build ./cmake_build --target all -j

# -----------------------------------
# If compiling V4 with sundials use the following (default)
# -----------------------------------

