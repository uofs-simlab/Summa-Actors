#! /bin/bash

# set the SUNDIALS version (see https://github.com/LLNL/sundials/releases for options)
SUNDIALS_VER=7.4.0 # latest SUNDIALS version is recommended

SUNDIALSDIR=$PWD/install/sundials

# Load the necessary modules (cluster dependent below work for anvil)
# module load gcc/11.2.0 
# module load netlib-lapack
# module load netcdf-fortran

wget https://github.com/LLNL/sundials/archive/refs/tags/v$SUNDIALS_VER.tar.gz
tar -xzf v$SUNDIALS_VER.tar.gz
cd sundials-$SUNDIALS_VER
mkdir build
cd build
cmake ../ -DBUILD_FORTRAN_MODULE_INTERFACE=ON \
    -DCMAKE_Fortran_COMPILER=gfortran \
    -DCMAKE_INSTALL_PREFIX=$SUNDIALSDIR
make -j 4
make install
