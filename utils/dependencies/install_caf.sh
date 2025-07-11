#!/bin/bash
#####################################################################
# CAF Installation Script
#
# Usage:
#   ./install_caf.
#

#####################################################################

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
