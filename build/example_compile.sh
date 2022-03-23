#!/bin/bash

#### load modules if using Compute Canada or Copernicus ####
# module load gcc/9.3.0
# module load netcdf-fortran
# module load openblas
# module load caf

#### Specifiy Master Directory, parent of build directory

# export F_MASTER=/path/to/summaActors

#### Specifiy Compilers ####
# export FC=gfortran
# export CC=g++

#### Includes and Libraries ####

# export INCLUDES = NETCDF and OPENBLAS
# export LIBRARIES = NETCDF and OPENBLAS

# export ACTORS_INCLUDES = C++ Actor Framework 
# export ACTORS_LIBRARIES = C++ Actor Framework and
#   The directory in which libsumma.so resides

#### Compile with the Makefile ####
# make -f ${F_MASTER}/build/makefile lib # libsumma.so part

# mv libsumma.so ${F_MASTER}/bin # optional move of libsumma (just ensure that summaMain knows where to find it)

# make -f ${F_MASTER}/build/makefile main # summaMain part

# mv summaMain ${F_MASTER}/bin # optional move, cleans things up

# export LD_LIBRARY_PATH=Path/to/libsumma.so