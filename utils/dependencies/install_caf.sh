#!/bin/bash


wget https://github.com/actor-framework/actor-framework/archive/refs/tags/1.0.0.tar.gz
tar -xvf 1.0.0.tar.gz

INSTALL_DIR=$PWD/caf

echo "Installing CAF to $INSTALL_DIR"

cd actor-framework-1.0.0
./configure --prefix=$INSTALL_DIR
cd build
make -j 8
make install
