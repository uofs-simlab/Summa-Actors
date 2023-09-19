#!/bin/bash
  
# build on Mac, from cmake directory run this as ./build_actors.mac.bash

# Mac Example using MacPorts:
export FC=gfortran                                            # Fortran compiler family
export LINK_DIRS='/usr/local/lib;/usr/lib'                               # Link directories for cmake
export INCLUDES_DIRS='/usr/local/include;/usr/include'      # directories for INCLUDES cmake variable (cmake uses semicolons as separators)
export LIBRARY_LINKS='-llapack;-lgfortran;-lnetcdff;-lnetcdf' # list of library links (cmake uses semicolons as separators)
#export FLAGS_OPT="-flto=1"                                   # -flto=1 is slow to compile, but might want to use

export SUNDIALS_PATH="/usr/local/sundials/v6.6"
export ACTOR_FRAMEWORK_PATH="/usr/local"
cmake -B ../cmake_build -S . -DCMAKE_BUILD_TYPE=Sundials_Actors_Debug
cmake --build ../cmake_build --target all
