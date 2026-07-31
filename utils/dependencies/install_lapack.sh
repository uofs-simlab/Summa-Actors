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
#####################################################################

LAPACK_ver=3.12.1 # LAPACK version number

export BLASDIR=$PWD/install/lapack
wget https://github.com/Reference-LAPACK/lapack/archive/refs/tags/v$LAPACK_ver.tar.gz
tar -xf v$LAPACK_ver.tar.gz
cd lapack-$LAPACK_ver/
mkdir build
cd build
cmake -DCMAKE_INSTALL_LIBDIR=$BLASDIR -DBUILD_SHARED_LIBS=ON .. 
cmake --build . -j --target install
