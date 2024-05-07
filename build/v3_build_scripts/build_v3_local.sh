#! /bin/bash

# Builds Summa-Actors with version 3 of SUMMA (no-sundials)
# To enable debuggin symbols, change BE to Debug

cmake -B cmake_build -S . -DCMAKE_BUILD_TYPE=BE
cmake --build cmake_build --target all -j