#FROM ubuntu:20.04
FROM linuxbrew/brew:latest


# This dockerfile creates the environement for compiling and
# running SUMMA4CHM. Once started cd into /code/build/source/cppwrap.
# make lib
# make
# export LD_LIBRARY_PATH=/code/build/source/cppwrap:D_LIBRARY_PATH
# ./program

WORKDIR /code

RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" apt-get install -y software-properties-common \
    libnetcdf-dev \
    libnetcdff-dev \
    liblapack-dev

RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y \
    && apt-get update \
    && apt-get install -y gfortran-7

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

RUN brew install caf

ADD . /code

ENV LD_LIBRARY_PATH=/code/build:/home/linuxbrew/.linuxbrew/Cellar/caf/0.18.5/lib/

# RUN cp -r /home/linuxbrew/.linuxbrew/Cellar/caf/0.18.5/lib/* /usr/local/lib/
# RUN cp -r /home/linuxbrew/.linuxbrew/Cellar/caf/0.18.*/include/caf /usr/local/include/
# RUN cp -f /usr/local/lib/libcaf_core.so.0.18.* /code/build/source/cppwrap/




