#!/bin/bash
  
# If most libraries were installed with apt-get, then the following should work
# Otherwise, you may need to adjust the paths
export FC=gfortran                                            # Fortran compiler family
export LINK_DIRS='/usr/local/lib;/usr/lib'                               # Link directories for cmake
export INCLUDES_DIRS='/usr/local/include;/usr/include'      # directories for INCLUDES cmake variable (cmake uses semicolons as separators)
export LIBRARY_LINKS='-llapack;-lgfortran;-lnetcdff;-lnetcdf' # list of library links (cmake uses semicolons as separators)

# Set the following paths
export SUNDIALS_PATH="/usr/local/sundials"
export ACTOR_FRAMEWORK_PATH="/usr/local"

cmake -B ./cmake_build -S ../summa/build/cmake/. -DCMAKE_BUILD_TYPE=Sundials_Actors
cmake --build ./cmake_build --target all
