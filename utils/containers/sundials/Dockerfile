FROM ubuntu:20.04

WORKDIR /code

# Get library dependencies
RUN apt-get update -y && \
    apt-get upgrade -y && \
    DEBIAN_FRONTEND="noninteractive" apt-get install -y software-properties-common \
    libnetcdf-dev \
    libnetcdff-dev \
    liblapack-dev \
    libopenblas-dev \
    cmake \
    g++ \
    git \
    libssl-dev \
    make \
    gfortran \
    wget \
    python3-pip \
    valgrind \
    gdb &&\
    apt-get autoclean

RUN pip3 install xarray
RUN pip3 install netcdf4

# Install the C++ Actor Framework From Git
RUN git clone https://github.com/actor-framework/actor-framework.git
WORKDIR /code/actor-framework
RUN ./configure
WORKDIR /code/actor-framework/build
RUN make
RUN make test
RUN make install

WORKDIR /code

# Install Sundials
RUN wget https://github.com/LLNL/sundials/releases/download/v5.8.0/sundials-5.8.0.tar.gz
RUN tar -xzf sundials-5.8.0.tar.gz
RUN mkdir sundials
WORKDIR /code/sundials
RUN mkdir instdir
RUN mkdir builddir
WORKDIR /code/sundials/builddir
RUN cmake ../../sundials-5.8.0  -DBUILD_FORTRAN_MODULE_INTERFACE=ON -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=/code/sundials/instdir -DEXAMPLES_INSTALL_PATH=/code/sundials/instdir/examples
RUN make
RUN make install

# Change workdir for when we attach to this container
WORKDIR /Summa-Actors
ENV LD_LIBRARY_PATH=/Summa-Actors/bin:/usr/local/lib:/code/sundials/instdir/lib