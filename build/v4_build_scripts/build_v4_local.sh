#!/bin/bash

export CMAKE_PREFIX_PATH="/opt/homebrew/opt/openblas/"
  
cmake -B ./cmake_build -S .. -DUSE_SUNDIALS=ON
cmake --build ./cmake_build --target all -j
