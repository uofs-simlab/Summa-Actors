#### Compiling Summa-Actors ####
SummaActors can be compiled by modifing the following components in the makefile
 - F_MASTER = directory/above/build
 - FC = gfortran
 - CC = g++
 - INCLUDES = Path/to/netcdf/includes
 - LIBRARIES = Path/to/netcdf/lib & Path/to/openblas
        -lnetcdff -lopenblas
 - ACTORS_INCLUDES = $INCLUDES & Path/to/CAF/includes
 - ACTORS_LIBRARIES = $LIBRARIES & PATH/to/CAF/lib
        -lcaf_core -lcaf_io -lsumma -lopenblas -lnetcdff

Once all above variables are set compilation is done with:

make

There is an example bash file included in the /build directory that can be modified for compiling Summa. This is benficial in cluster environments as you can load required modules within the script. The script can then be run with:

source compilation_script

#### Running Summa-Actors #####
Once the binary is compiled it can be run like the following example command:

./summaMain -g 1 -c 10 -m /path/to/file/manager/ --config-file=/path/to/actors/config/file

 -g = starting index of the first GRU to compute
 -c = number of grus to run
 -m = path to the file manager
 --config-file = /path/to/config/file

#### Config File ####
the configuraton file is used to specifiy the number of threads that Summa-Actors can spawn. If you would like to spawn as many as your system has then you can omit this argument. This argument is mainly needed for cluster environments to ensure that Summa-Actors threads to Core ratio is 1:1.

The contents of the configuration file look like the following:
caf {
  # Parameters selecting a default scheduler.
  scheduler {
    max-threads = 8
  }
}


