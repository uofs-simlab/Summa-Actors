FROM ubuntu:20.04

WORKDIR /code

# Get library dependencies
RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" apt-get install -y software-properties-common \
    libnetcdf-dev \
    libnetcdff-dev \
    liblapack-dev \
    libopenblas-dev

# Get gfortran dependencies
RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y \
    && apt-get update \
    && apt-get install -y gfortran-7

# get some other useful dependencies
RUN apt update -y \
    && apt upgrade -y \
    && DEBIAN_FRONTEND="noninteractive" apt install -y \
         cmake \
         g++ \
         git \
         libssl-dev \
         make \
         gfortran \
         gdb \
    && apt-get autoclean

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





