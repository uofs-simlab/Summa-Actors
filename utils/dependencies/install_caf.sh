#!/bin/bash
#####################################################################
# CAF Installation Script
#
# Usage:
#   ./install_caf.
#

#####################################################################

export CAFDIR=$PWD/install/caf
wget https://github.com/actor-framework/actor-framework/archive/refs/tags/1.0.2.tar.gz
tar -xvf 1.0.2.tar.gz
echo "Installing CAF to $CAFDIR"
cd actor-framework-1.0.2
./configure --prefix=$CAFDIR
cd build
make -j 8
make install