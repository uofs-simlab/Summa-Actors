#!/bin/bash
#####################################################################
# SUMMA-Actors build script -- generic Linux (no module system)
#
# Run from build/build_scripts/ :
#     ./build.sh                 # configure + build
#     USE_SUNDIALS=OFF ./build.sh # build without the IDA solver
#     CLEAN=1 ./build.sh         # wipe build/cmake_build first
#
# Expects the dependencies (lapack, netcdf-c, netcdf-fortran, caf, and
# -- when USE_SUNDIALS=ON -- sundials) to have been installed into
# utils/dependencies/install/ by the scripts in utils/dependencies/.
# If they live elsewhere, prepend their locations to CMAKE_PREFIX_PATH
# before running, or edit INSTALL_DIR below.
#
# For an HPC module system use build_cluster.sh; on macOS use build_mac.sh.
#####################################################################
set -euo pipefail

cd "$(dirname "$0")"                       # build/build_scripts/
BUILD_DIR=$(cd .. && pwd)                  # build/
INSTALL_DIR=$(cd ../../utils/dependencies/install && pwd)

USE_SUNDIALS=${USE_SUNDIALS:-ON}
NJOBS=$(command -v nproc >/dev/null && nproc || echo 4)

export CMAKE_PREFIX_PATH="\
$INSTALL_DIR/caf:\
$INSTALL_DIR/netcdf-fortran:$INSTALL_DIR/netcdf-c:$INSTALL_DIR/lapack:\
${CMAKE_PREFIX_PATH:-}"

if [ "$USE_SUNDIALS" = "ON" ]; then
  export CMAKE_PREFIX_PATH="$INSTALL_DIR/sundials:$CMAKE_PREFIX_PATH"
fi

[ "${CLEAN:-0}" = "1" ] && { echo "==> Removing $BUILD_DIR/cmake_build"; rm -rf "$BUILD_DIR/cmake_build"; }

echo "==> Configuring (USE_SUNDIALS=$USE_SUNDIALS)"
cmake -B "$BUILD_DIR/cmake_build" -S "$BUILD_DIR" \
      -DUSE_SUNDIALS="$USE_SUNDIALS" -DCMAKE_BUILD_TYPE=Release

echo "==> Building (-j$NJOBS)"
cmake --build "$BUILD_DIR/cmake_build" --target all -j"$NJOBS"

echo "==> Done: $(cd "$BUILD_DIR/.." && pwd)/bin/summa_actors.exe"
