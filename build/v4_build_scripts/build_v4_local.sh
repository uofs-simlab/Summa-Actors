#!/bin/bash
  
cmake -B ./cmake_build -S ../summa/build/. -DUSE_ACTORS=ON -DUSE_SUNDIALS=ON
cmake --build ./cmake_build --target all -j
