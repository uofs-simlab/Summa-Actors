#! /bin/bash


SUNDIALSDIR=$PWD/install/sundials

# Load the necessary modules (cluster dependent below work for anvil)
# module load gcc/11.2.0 
# module load netlib-lapack
# module load netcdf-fortran

wget https://github.com/LLNL/sundials/archive/refs/tags/v7.1.1.tar.gz
tar -xzf v7.1.1.tar.gz
cd sundials-7.1.1
mkdir build
cd build
cmake ../ -DBUILD_FORTRAN_MODULE_INTERFACE=ON \
    -DCMAKE_Fortran_COMPILER=gfortran \
    -DCMAKE_INSTALL_PREFIX=$SUNDIALSDIR
make -j 4
make install