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
    python3-pip \
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
# Change workdir for when we attach to this container
WORKDIR /Summa-Actors
ENV LD_LIBRARY_PATH=/Summa-Actors/bin:/usr/local/lib





