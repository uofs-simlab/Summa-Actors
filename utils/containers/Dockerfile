FROM ubuntu:22.04

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

# Install the C++ Actor Framework 0.18.6
WORKDIR /opt
RUN git clone https://github.com/actor-framework/actor-framework.git
WORKDIR /opt/actor-framework
RUN ./configure --prefix=/usr/local/caf
WORKDIR /opt/actor-framework/build
RUN make -j 4
RUN make install


# Install Sundials
WORKDIR /opt
RUN wget https://github.com/LLNL/sundials/releases/download/v7.0.0/sundials-7.0.0.tar.gz
RUN tar -xzf sundials-7.0.0.tar.gz
WORKDIR /opt/sundials-7.0.0
RUN mkdir build/
RUN mkdir /usr/local/sundials
WORKDIR /opt/sundials-7.0.0/build
RUN cmake ../ -DBUILD_FORTRAN_MODULE_INTERFACE=ON \
        -DCMAKE_Fortran_COMPILER=gfortran \
        -DCMAKE_INSTALL_PREFIX=/usr/local/sundials
RUN make -j 4
RUN make install

WORKDIR /code


