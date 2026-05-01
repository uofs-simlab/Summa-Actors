#!/bin/bash

# If compiling on a Digital Research Alliance of Canada cluster,
# load the following modules:
# module load StdEnv/2023
# module load gcc/12.3
# module load openblas/0.3.24
# module load openmpi/4.1.5
# module load netcdf-fortran/4.6.1

# If compiling on Anvil, load the following modules:
# module load gcc/14.2.0
# module load openmpi/4.1.6
# module load openblas/0.3.17
# module load netcdf-fortran/4.5.3

# -----------------------------------

# Compiling the LATEST version of the code
# -----------------------------------
INSTALL_DIR=$PWD/../../utils/dependencies/install
export CMAKE_PREFIX_PATH="$INSTALL_DIR/sundials:$INSTALL_DIR/caf:$CMAKE_PREFIX_PATH"

# will only need this if not using modules
#export CMAKE_PREFIX_PATH="$INSTALL_DIR/netcdf-fortran:$INSTALL_DIR/netcdf-c:$INSTALL_DIR/lapack:$CMAKE_PREFIX_PATH"

cmake -B ./cmake_build -S .. -DUSE_SUNDIALS=ON -DCMAKE_BUILD_TYPE=Release
cmake --build ./cmake_build --target all -j



# -----------------------------------
# If compiling without sundials use the following
  
# cmake -B ./cmake_build -S .. -DUSE_SUNDIALS=OFF -DCMAKE_BUILD_TYPE=Release
# cmake --build ./cmake_build --target all -j



