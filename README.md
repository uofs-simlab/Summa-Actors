# SUMMA-Actors: Structure for Unifying Multiple Modeling Alternatives with Actors

SUMMA-Actors is a modified version of the already existing SUMMA software that can be 
found [here](https://github.com/CH-Earth/summa#readme). SUMMA-Actors uses the Actor Model to increase scalability and fault-tolerance. It is built using the [C++ Actor Framework](https://github.com/actor-framework/actor-framework). 

## Documentation
A more in-depth documentation can be found [here](https://summa-actors.readthedocs.io/en/latest/)

## Compiling Summa-Actors
SUMMA-Actors is written in C++ and FORTRAN and is currently tested for the following compilers:
 * g++
 * gfortran

SUMMA-Actors depends on the following Libraries:
 * [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
 * [OpenBLAS](https://github.com/xianyi/OpenBLAS)
 * [C++ Actor Framework](https://github.com/actor-framework/actor-framework) 

Once the following libraries have been installed SUMMA-Actors can be compiled in 
one of two ways. The first way is to modify the makefile directly and the second 
is to invoke the makefile by shellscript:

### Method 1: Makefile
The method is best used for a workstation build that does not have access to a Compute Canada environment where inlcudes and libraires will need to be explicitly specified. 
 1. Makefile:
  Changes need to be made to the following variables in the Makefile:
    - F_MASTER = directory/above/build
    - FC = gfortran
    - CC = g++
    - INCLUDES = Path/to/netcdf/includes
    - LIBRARIES = Path/to/netcdf/lib & Path/to/openblas
        -lnetcdff -lopenblas
    - ACTORS_INCLUDES = $INCLUDES & Path/to/CAF/includes
    - ACTORS_LIBRARIES = $LIBRARIES & PATH/to/CAF/lib & PATH/to/libsumma.so
        -lcaf_core -lcaf_io -lsumma -lopenblas -lnetcdff

  After the following SUMMA-Actors can be compiled with `make`.

  Note: SUMMA is compiled as a shared library (libsumma.so) and the main program 
  will need to know where this library is located in order to properly link it.
  By compiling both libsumma.so and summaMain in the same directory (build/) should be 
  enough. However if there are issues, specifing where libsumma.so will be compiled can be done 
  by adding the path to `ACTORS_LIBRARIES` in the makefile this should rectify the issues.
  
### Method 2: Shell Script
 2. ShellScript:
  In the build directory exists a example_compile.sh script that can be modified.
  This is usually used for HPC computing environments and includes which modules 
  to load for Compute Canada or the University of Saskatchewan's Copernicus. 
  example_compile.sh contains instructions on what parts to modify and how to 
  invoke the makefile from the script.
  
  Once the shellscript has been modified running it with `source your_script.sh` will compile 
  SUMMA-Actors.


After SUMMA-Actors is compiled you should be left with a libsumma.so and a summaMain file 
in your build directory.

It is important to set the `LD_LIBRARY_PATH` environment variable before attempting to run 
summa. This variable needs to point to the location of libsumma.so. If you compiled using the 
shellscript using the `source` command and following the directions within the example_compile.sh 
this should already be set for you. 


## Running Summa-Actors

Once the binary is compiled it can be run like the following example command:

./summaMain -g 1 -n 10 -c /path/to/config/directory --config-file=/path/to/actors/config/file

 -g = starting index of the first GRU to compute
 -c = number of grus to run
 -m = path to the file manager
 OPTIONAL: --config-file = /path/to/config/file
  This config file specifies to the C++ Actor Framework how many threads to use when executing the program. If left out the C++ Actor Framework will automatically set this value based on your system.

#### Config File ####
SUMMA-Actors settings can be modified from a JSON file provided in config/Summa_Actors_Settings.json.
There are three types of actors that can be configured:
  * SummaActor
    - OutputStructureSize = The number of timesteps in which an HRU can hold before needing to contact 
    the file_access_actor to write the data to a file.
    - maxGRUPerJob = The number of GRUs that will be attemtpted to run at once. For example, if this value 
    is set to 500 and you invoke the program with ./summaMain -g 1 -n 1000 -c /path/to/config/directory. 
    SUMMA-Actors will only spawn 500 actors at a time and compute all 1000 in two batches.

    Both of the above setting control the amount of RAM SUMMA-Actors uses. Larger numbers can cause your 
    job to run out of memory. We have found that setting both to 500 uses around 20GB of RAM for reference.

  * JobActor
    - FileManagerPath = Path the the fileManager.txt file needed by SUMMA. This is remained relativley 
    unchanged from the original version of SUMMA. With two additions. An example file is provided in the 
    config/ directory called fileManager_example.txt
    - outputCSV = Boolean value for if you would like individual HRU run-time statsicts when they complete.
    - csvPath = The path that the csv file will be written to.
  
  * HRUActor
    - printOutput = Boolean value for if you would like each HRU to print information on where it is in 
    its computation. ie. what timestep it is on and some other timing information.
    - outputFrequency = The frequency in which you would like an HRU printing to stdout. The number specified is the interval in timesteps in which an HRU will print. Note: Lower numbers can see decreased performance as stdout will begin to lag the more that needs to be printed.


## Credits
The inital implementation of SUMMA is credited to the inital publications below. These 
publications can be found in [Water Resources Research](http://onlinelibrary.wiley.com/journal/10.1002/(ISSN)1944-7973).

 * Clark, M. P., B. Nijssen, J. D. Lundquist, D. Kavetski, D. E. Rupp, R. A. Woods, J. E. Freer, E. D. Gutmann, A. W. Wood, L. D. Brekke, J. R. Arnold, D. J. Gochis, R. M. Rasmussen, 2015a: A unified approach for process-based hydrologic modeling: Part 1. Modeling concept. _Water Resources Research_, [doi:10.1002/2015WR017198](http://dx.doi.org/10.1002/2015WR017198).<a id="clark_2015a"></a>

 * Clark, M. P., B. Nijssen, J. D. Lundquist, D. Kavetski, D. E. Rupp, R. A. Woods, J. E. Freer, E. D. Gutmann, A. W. Wood, D. J. Gochis, R. M. Rasmussen, D. G. Tarboton, V. Mahat, G. N. Flerchinger, D. G. Marks, 2015b: A unified approach for process-based hydrologic modeling: Part 2. Model implementation and case studies. _Water Resources Research_, [doi:10.1002/2015WR017200](http://dx.doi.org/10.1002/2015WR017200).<a id="clark_2015b"></a>

We also credit the original creators of the C++ Actor Framework which allowed us to implement the actor model into SUMMA-Actors. Links to their research work can be found 
below.

 * Charousset, D., Schmidt, T. C., Hiesgen, R., Wählisch, M., 2013: Native actors: 
 a scalable software platform for distributed, heterogeneous environments. _AGERE!_, 
 [doi:10.1145/2541329.2541336](http://dx.doi.org/10.1145/2541329.2541336).

 * Charousset, D., Schmidt, T. C., Hiesgen, R., 2016: Revisiting actor programming in 
 C++. _Computer Languages, Systems & Structures_, [doi:10.1016/j.cl.2016.01.002](http://
 dx.doi.org/10.1016/j.cl.2016.01.002)



