#! /bin/bash

ROOTDIR=$PWD
export CC=gcc
export FC=gfortran

export CPPFLAGS=""
export LDFLAGS=""
# Install zlib
export ZDIR=$PWD/zlib
{
  wget https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz
  tar -xf zlib-1.3.1.tar.gz
  cd zlib-1.3.1
  ./configure --prefix=$ZDIR
  make check
  make install
} 2>&1 | tee zlib.log
export CPPFLAGS=""
export LDFLAGS=""

# # Instal HDF5
cd $ROOTDIR
export H5DIR=$PWD/hdf5
{
  wget https://github.com/HDFGroup/hdf5/releases/download/hdf5_1.14.4.2/hdf5-1.14.4-2.tar.gz
  tar -xf hdf5-1.14.4-2.tar.gz
  cd hdf5-1.14.4-2
  ./configure --with-zlib=${ZDIR} --prefix=${H5DIR} --enable-hl
  make install
} 2>&1 | tee hdf5.log
export CPPFLAGS=""
export LDFLAGS=""

# Install NetCDF-C
cd $ROOTDIR
export NCDIR="$PWD/netcdf-c"
{
  wget https://github.com/Unidata/netcdf-c/archive/refs/tags/v4.9.2.tar.gz
  tar -xf v4.9.2.tar.gz
  cd netcdf-c-4.9.2
  export CPPFLAGS="-I${H5DIR}/include -I${ZDIR}/include"
  export LDFLAGS="-L${H5DIR}/lib -L${ZDIR}/lib"
  ./configure --prefix=${NCDIR}
  make install
} 2>&1 | tee netcdf-c.log
export CPPFLAGS=""
export LDFLAGS=""

# Install NetCDF-Fortran
cd $ROOTDIR
export NFDIR=$PWD/netcdf-fortran
{
  wget https://github.com/Unidata/netcdf-fortran/archive/refs/tags/v4.6.1.tar.gz
  tar -xf v4.6.1.tar.gz 
  cd netcdf-fortran-4.6.1/
  export LD_LIBRARY_PATH="${NCDIR}/lib:${LD_LIBRARY_PATH}"
  export CPPFLAGS="-I${NCDIR}/include"
  export LDFLAGS="-L${NCDIR}/lib"
  echo $CPPFLAGS
 ./configure --prefix=${NFDIR}
  make install
} 2>&1 | tee netcdf-fortran.log
