#!/bin/bash
INSTALL_DIR=$PWD/caf

# Load the necessary modules (cluster dependent below work for anvil)
# module load gcc/11.2.0 
# module load netlib-lapack
# module load netcdf-fortran

wget https://github.com/actor-framework/actor-framework/archive/refs/tags/1.0.0.tar.gz
tar -xvf 1.0.0.tar.gz


echo "Installing CAF to $INSTALL_DIR"

cd actor-framework-1.0.0
./configure --prefix=$INSTALL_DIR
cd build
make -j 8
make install
