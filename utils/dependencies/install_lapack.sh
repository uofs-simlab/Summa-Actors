#! /bin/bash
#####################################################################
# LAPACK Installation Script
#
# Usage:
#   ./install_lapack.sh
#
# After installation, add the following to the build.sh script:
#   export LAPACK_PATH="path/to/lapack/liblapack.so"
# Then modify the CMAKE command to include the following flag:
#  -DUSE_CUSTOM_LAPACK=ON

export BLASDIR=$PWD/install/lapack
wget https://github.com/Reference-LAPACK/lapack/archive/refs/tags/v3.12.1.tar.gz
tar -xf v3.12.1.tar.gz
cd lapack-3.12.1/
mkdir build
cd build
cmake -DCMAKE_INSTALL_LIBDIR=$BLASDIR -DBUILD_SHARED_LIBS=ON .. 
cmake --build . -j --target install
