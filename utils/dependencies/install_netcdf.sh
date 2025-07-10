#! /bin/bash

#####################################################################
# NetCDF Installation Script
#
# This script installs NetCDF-C and NetCDF-Fortran along with their 
# dependencies (zlib and HDF5). The libraries will be installed in the 
# directory where this script is executed.
#
# Usage:
#   ./install_netcdf.sh
#
# If you encounter any issues, please post an issue on the Summa-Actors repository:
#   https://github.com/uofs-simlab/Summa-Actors/issues
#
# After installation, update the CMAKE_PREFIX_PATH environment variable:
#   export CMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH:/path/to/netcdf-c"
#   export CMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH:/path/to/netcdf-fortran"
#
# Then, run cmake as you normally would.
#####################################################################

#### version numbers for packages ####
# zlib
zlib_ver=1.3.1
# HDF5
HDF5_ver=1.14.6

export NETCDFCDIR=$PWD/install/netcdf-c
export NETCDFFDIR=$PWD/install/netcdf-fortran
export H5DIR=$PWD/install/hdf5
export ZDIR=$PWD/install/zlib

ROOTDIR=$PWD

# Install zlib
{
  wget https://github.com/madler/zlib/releases/download/v$zlib_ver/zlib-$zlib_ver.tar.gz
  tar -xf zlib-$zlib_ver.tar.gz
  cd zlib-$zlib_ver
  ./configure --prefix=$ZDIR
  make check
  make install
} 2>&1 | tee zlib.log

# Instal HDF5
# determine name variables (dealing with notation for patch levels)
if [[ $(echo "$HDF5_ver" | grep -o "\." | wc -l) -eq 3 ]]; then  # if version number has three decimal point characters
  HDF5_ver_dash=$(echo "$HDF5_ver" | sed 's/\.\([^.]*\)$/-\1/') # replace third decimal point with a dash
else
  HDF5_ver_dash=$HDF5_ver
fi
cd $ROOTDIR
{
  wget https://github.com/HDFGroup/hdf5/releases/download/hdf5_$HDF5_ver/hdf5-$HDF5_ver_dash.tar.gz
  tar -xf hdf5-$HDF5_ver_dash.tar.gz
  cd hdf5-$HDF5_ver_dash
  #wget https://github.com/HDFGroup/hdf5/releases/download/hdf5_1.14.4.2/hdf5-1.14.4-2.tar.gz
  #tar -xf hdf5-1.14.4-2.tar.gz
  #cd hdf5-1.14.4-2
  ./configure --with-zlib=${ZDIR} --prefix=${H5DIR} --enable-hl
  make install
} 2>&1 | tee hdf5.log

# Install NetCDF-C
cd $ROOTDIR
{
  wget https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.9.2.tar.gz
  tar -xf v4.9.2.tar.gz
  cd netcdf-c-4.9.2
  CPPFLAGS="-I${H5DIR}/include -I${ZDIR}/include" \
  LDFLAGS="-L${H5DIR}/lib -L${ZDIR}/lib" \
  ./configure --prefix=${NETCDFCDIR}
  make install
} 2>&1 | tee netcdf-c.log

# Install NetCDF-Fortran
cd $ROOTDIR
{
  wget https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz
  tar -xf v4.6.1.tar.gz 
  cd netcdf-fortran-4.6.1/
  LD_LIBRARY_PATH="${NETCDFCDIR}/lib:${LD_LIBRARY_PATH}" \
  CPPFLAGS="-I${NETCDFCDIR}/include" \
  LDFLAGS="-L${NETCDFCDIR}/lib" \
 ./configure --prefix=${NETCDFFDIR}
  make install
} 2>&1 | tee netcdf-fortran.log
