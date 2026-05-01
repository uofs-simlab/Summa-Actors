#!/bin/bash
#####################################################################
# CAF Installation Script
#
# Usage:
#   ./install_caf.
#

#####################################################################

# If compiling on a Digital Research Alliance of Canada cluster,
# load the following modules:
# module load StdEnv/2023
# module load gcc/12.3
# module load openblas/0.3.24
# module load netcdf-fortran/4.6.1

# If compiling on Anvil, load the following modules:
# module load gcc/14.2.0
# module load openblas/0.3.17
# module load netcdf-fortran/4.5.3

CAF_ver=1.1.0 # CAF version number

export CAFDIR=$PWD/install/caf
wget https://github.com/actor-framework/actor-framework/archive/refs/tags/$CAF_ver.tar.gz
tar -xvf $CAF_ver.tar.gz
echo "Installing CAF to $CAFDIR"
cd actor-framework-$CAF_ver
./configure --prefix=$CAFDIR
cd build
make -j 8
make install
