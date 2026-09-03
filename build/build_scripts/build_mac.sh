#!/bin/bash
#####################################################################
# SUMMA-Actors build script for macOS (Apple Silicon / Intel)
#
# Run from build/build_scripts/ :
#     ./build_mac.sh            # configure + build
#     CLEAN=1 ./build_mac.sh    # wipe build/cmake_build first
#
# Prerequisites (MacPorts):
#     sudo port install gcc15 cmake netcdf-fortran +gcc15 OpenBLAS +gcc15+lapack
#
# SUNDIALS is expected at  ../../../../SummaSundials/sundials/instdir/
# (relative to this script).  Set SUNDIALS_INSTDIR to override, or set
# USE_SUNDIALS=OFF to build without it.
#
# Apple's system clang cannot link the gfortran-built objects and GNU g++
# cannot parse Apple's <mach/*.h>, so everything is built with the MacPorts
# GCC toolchain.  The C++ Actor Framework needs a 2-line source patch to
# skip the mach-header code path; this script applies it idempotently.
#####################################################################
set -euo pipefail

# --- locations ------------------------------------------------------
cd "$(dirname "$0")"                       # build/build_scripts/
SCRIPT_DIR=$PWD
BUILD_DIR=$(cd .. && pwd)                  # build/
REPO_DIR=$(cd ../.. && pwd)               # repo root
DEP_DIR=$REPO_DIR/utils/dependencies
CAF_VER=1.1.0
CAF_SRC=$DEP_DIR/actor-framework-$CAF_VER
CAF_INSTALL=$DEP_DIR/install/caf

USE_SUNDIALS=${USE_SUNDIALS:-ON}
SUNDIALS_INSTDIR=${SUNDIALS_INSTDIR:-$SCRIPT_DIR/../../../../SummaSundials/sundials/instdir}

# --- toolchain (MacPorts GCC) -------------------------------------
CC=${CC:-/opt/local/bin/gcc}
CXX=${CXX:-/opt/local/bin/g++}
FC=${FC:-/opt/local/bin/gfortran}
for c in "$CC" "$CXX" "$FC"; do
  [ -x "$c" ] || { echo "ERROR: compiler not found: $c  (sudo port install gcc15)"; exit 1; }
done
NJOBS=$(sysctl -n hw.ncpu)

# --- 1. C++ Actor Framework -------------------------------------
if [ -d "$CAF_INSTALL/lib/cmake/CAF" ]; then
  echo "==> CAF already installed at $CAF_INSTALL  (skipping; rm -rf it to rebuild)"
else
  echo "==> Building CAF $CAF_VER"
  [ -d "$CAF_SRC" ] || { echo "ERROR: $CAF_SRC missing (run utils/dependencies/install_caf.sh once, or extract the tarball)"; exit 1; }

  # Patch telemetry/importer/process.cpp: GNU g++ cannot compile the
  # CAF_MACOS mach-header path.  Falls back to the no-op metrics stub.
  PROC=$CAF_SRC/libcaf_core/caf/telemetry/importer/process.cpp
  if grep -q "CAF_MACOS excluded" "$PROC"; then
    echo "    process.cpp already patched"
  else
    perl -0pi -e 's/#if defined\(CAF_MACOS\) \|\| defined\(CAF_LINUX\) \|\| defined\(CAF_NET_BSD\)/#if defined(CAF_LINUX) || defined(CAF_NET_BSD) \/\/ CAF_MACOS excluded for GNU g++/' "$PROC"
    perl -0pi -e 's/^#ifdef CAF_MACOS\b/#if 0 \/\/ CAF_MACOS excluded for GNU g++/m' "$PROC"
    grep -q "CAF_MACOS excluded" "$PROC" || { echo "ERROR: failed to patch $PROC"; exit 1; }
    echo "    patched process.cpp"
  fi

  rm -rf "$CAF_SRC/build"
  ( cd "$CAF_SRC" && CC="$CC" CXX="$CXX" ./configure --prefix="$CAF_INSTALL" )
  cmake --build "$CAF_SRC/build" -j"$NJOBS" --target install
  [ -d "$CAF_INSTALL/lib/cmake/CAF" ] || { echo "ERROR: CAF install incomplete"; exit 1; }
fi

# --- 2. CMAKE_PREFIX_PATH --------------------------------------
export CMAKE_PREFIX_PATH="$CAF_INSTALL:/opt/local:${CMAKE_PREFIX_PATH:-}"
CMAKE_ARGS=(
  -B "$BUILD_DIR/cmake_build" -S "$BUILD_DIR"
  -DCMAKE_C_COMPILER="$CC"
  -DCMAKE_CXX_COMPILER="$CXX"
  -DCMAKE_Fortran_COMPILER="$FC"
  -DCMAKE_BUILD_TYPE=Release
  -DUSE_SUNDIALS="$USE_SUNDIALS"
)
if [ "$USE_SUNDIALS" = "ON" ]; then
  [ -f "$SUNDIALS_INSTDIR/lib/cmake/sundials/SUNDIALSConfig.cmake" ] \
    || { echo "ERROR: SUNDIALS not found at $SUNDIALS_INSTDIR (set SUNDIALS_INSTDIR=... or USE_SUNDIALS=OFF)"; exit 1; }
  export CMAKE_PREFIX_PATH="$SUNDIALS_INSTDIR:$CMAKE_PREFIX_PATH"
  echo "==> SUNDIALS: $SUNDIALS_INSTDIR"
fi

# --- 3. configure + build -------------------------------------
[ "${CLEAN:-0}" = "1" ] && { echo "==> Removing $BUILD_DIR/cmake_build"; rm -rf "$BUILD_DIR/cmake_build"; }

echo "==> Configuring"
cmake "${CMAKE_ARGS[@]}"

echo "==> Building (-j$NJOBS)"
cmake --build "$BUILD_DIR/cmake_build" --target all -j"$NJOBS"

echo "==> Done: $REPO_DIR/bin/summa_actors.exe"
