# SUMMA-Actors: Structure for Unifying Multiple Modeling Alternatives with Actors

SUMMA-Actors is a modified version of the already existing SUMMA software that can be found [here](https://github.com/CH-Earth/summa#readme). SUMMA-Actors uses the Actor Model to increase scalability and fault-tolerance. It is built using the [C++ Actor Framework](https://github.com/actor-framework/actor-framework). 

## Documentation
A more in-depth documentation can be found [here](https://git.cs.usask.ca/numerical_simulations_lab/actors/Summa-Actors/-/wikis/home). SUMMA-Actors depends on many files from the [original SUMMA repo](https://github.com/CH-Earth/summa). Below is a quick start guide for compiling and running SUMMA-Actors, please consult our wiki for more in-depth documentation. Consider creating an issue for any missing documentation, asking questions, or providing suggestions.

## Preliminaries
SUMMA-Actors is meant to wrap around the existing SUMMA implementation. Therefore you will need to clone the SUMMA repo into the SUMMA-Actors build directory. SUMMA-Actors works with the master branch of SUMMA, which can be found [here](https://github.com/CH-Earth/summa). Below the directory structure is explained in more detail and the steps to compiling SUMMA-Actors are explained.

## Directory Structure
SUMMA-Actors is set up with the following sub-directories, we will consider the top level Summa Actors directory the `root_dir`:
 - bin
 - build
   - includes
   - source
   - summa (https://github.com/KyleKlenk/summa/tree/summa-actors, summa-actors branch)
   - CMakeLists.txt
 - utils
 - README.md

First clone Summa-Actors to your workstation. Then cd into `build/` and clone `summa` into Summa-Actor's build directory as folder summa.

## Compiling SUMMA-Actors -- With SUMMA verswion 3.x.x

### Dependencies
SUMMA-Actors has only one additional dependency, the [C++ Actor Framework](https://github.com/actor-framework/actor-framework), specifically the [0.18.6 
release](https://github.com/actor-framework/actor-framework/archive/refs/tags/0.18.6.tar.gz).

The following steps can be used to install the C++ Actor Framework on a Linux system:
```bash
wget https://github.com/actor-framework/actor-framework/archive/refs/tags/0.18.6.tar.gz
tar -xzf 0.18.6.tar.gz
cd actor-framework-0.18.6/
./configure --prefix=/path/to/install
cd build
make # [-j]
make install # [as root if necessary]
```

Additional dependencies required by both SUMMA and SUMMA actors are:
 * g++
 * gfortran
 * [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
 * [OpenBLAS](https://github.com/xianyi/OpenBLAS)

### Steps To Compile
 ```
 git clone https://git.cs.usask.ca/numerical_simulations_lab/actors/Summa-Actors.git
 cd Summa-Actors/build
 git clone -b summa-actors https://github.com/KyleKlenk/summa.git
 cmake -B cmake_build -S . -DCMAKE_BUILD_TYPE=Build_Type
 cmake --build cmake_build --target all -j
 ```
Available build types are:
  * BE or BE_Debug: Builds for local machine
  * BE_Cluster or BE_Cluster_Debug: See below section (Modules for Alliance Machines)

### Modules for Alliance Machines
To compile on Alliance machines the following modules can be loaded:
```bash
module load StdEnv/2020
module load gcc/9.3.0
module load openblas/0.3.17
module load netcdf-fortran/4.5.2
module load caf
```

## Compiling SUMMA-Actors -- With SUMMA version 4.x.x
The latest version of SUMMA introduced sundials as a dependency for its numerical solver. As a result the steps to compile SUMMA-Actors have changed
slightly. The dependecies above still apply, wiht the addition of sundials. The following steps can be used to install sundials on a Linux system:
```bash
wget https://github.com/LLNL/sundials/releases/download/v7.0.0/sundials-7.0.0.tar.gz
tar -xzf sundials-7.0.0.tar.gz
mkdir sundials && cd sundials
mkdir /usr/local/sundials
mkdir builddir && cd builddir
cmake ../../sundials-7.0.0 -DBUILD_FORTRAN_MODULE_INTERFACE=ON \
        -DCMAKE_Fortran_COMPILER=gfortran \
        -DCMAKE_INSTALL_PREFIX=/usr/local/sundials \
        -DEXAMPLES_INSTALL_PATH=/code/sundials/instdir/examples
make
make install
```

### Steps to Compile
```bash
git clone https://git.cs.usask.ca/numerical_simulations_lab/actors/Summa-Actors.git
cd Summa-Actors/build
git clone -b develop https://github.com/KyleKlenk/summa.git
cd Summa-Actors/build/build_scripts
# There are two files here, one for building on a local machine and one for building on the alliance cluster
# If local build ensure that the paths in the script are correct and 
# match the paths to the dependencies on your system
./build_v4_local.sh
```


## Running SUMMA-Actors
SUMMA-Actors can be run in two modes, distributed and non-distributed. The distributed mode used to create ad-hoc clusters. The command line arguments are kept as close to the original SUMMA as possible. However, there are options not yet available in SUMMA-Actors that are in the original SUMMA. Here is a full list of the available and unavailable options:
```   
Usage: summa_actors -m master_file [-g startGRU countGRU] [-c config_file] [-b backup_server] [-s server_mode]
  Available options:
    -m, --master:         Define path/name of master file (can be specified in config)
    -g, --gru:            Run a subset of countGRU GRUs starting from index startGRU 
    -c, --config:         Path name of the Summa-Actors config file (optional but recommended)
        --gen-config:     Generate a config file
    -b, --backup-server:  Start backup server, requires a server and config_file
    -s, --server-mode:    Enable server mode
    -h, --help:           Print this help message
  Unimplemented Options:
    -n --newFile          Define frequency [noNewFiles,newFileEveryOct1] of new output files
    -s --suffix           Add fileSuffix to the output files
    -h --hru              Run a single HRU with index of iHRU
    -r --restart          Define frequency [y,m,d,e,never] to write restart files
    -p --progress         Define frequency [m,d,h,never] to print progress
    -v --version          Display version information of the current build
```
Instructions for using each mode are provided below, if you just want to skip ahead and get started using the default options.


### Non-Distributed Mode (Similar to original SUMMA)
Using SUMMA-Actors in non-distributed mode is like running the normal SUMMA. The difference is that HRUs will run concurrently in SUMMA-Actors and maximize resource use. Usage is very close to SUMMA, and the same input files are required. In depth documentation for configuring a SUMMA run can be found [here](https://summa.readthedocs.io/en/latest/).

Usage is as follows: ./summa_actors -m master_file [-g startGRU countGRU] [-c config_file]

The master_file for SUMMA-Actors can either be defined explicitly on the command line or it can be included in the new to SUMMA-Actors config_file. The config_file is optional but it is highly recommended as it does allow users to fine tune how SUMMA-Actor will perform. We are working to automate these aspects but for now its best to define it for the domain and compute environment in use. Using `./summa_actors --gen-config` will generate a configuration file that can then be filled in. See the section on config_file in this readme for more information. 


### Distributed Mode
Using SUMMA-Actors in distributed mode allows SUMMA-Actors to solve batches and dymically assign them to nodes and reassign them in the event of node failures.

To use this feature there are 3 actors that can be spawned to cluster nodes together and add additional redundancy to the system. All settings are to be defined in the config_file for this mode. In the config file the user must set `distributed_mode` to true, define a `port`, and configure the domain to run. The domain is defined by how many total GRUs the user wishes to compute and the number of GRUs that should be assembled into a batch. Finally, the `server_list` should be set to the hostname of the backup_servers that are started.

To start the system one start the server first with `./summa_actors -c -s`
Backup servers can be added with `./summa_actors -c -b`
Clients can simply be added with `./summa_actors -c`

NOTE: Each system will need a copy of the forcing data or input data, or the data should be in a singular location like on a cluster system.

### Data-Assimilation Mode
Data-assimilation mode forces all HRUs to run a single timestep before moving on. This is akin to how real world experiments are run where data is collected and then used to
update the model. To run this mode on a single node there are a few options that are needed to be set in the config file:
  - `data_assimilation_mode` (true/false) Enables data-assimilation mode
  - `batch_size` (int) number of HRU actors a HRU_batch actor will run. (9999 = default) and (1) will not use an intermediate batch actor

This mode also works in a distributed environment, the user just needs to ensure that the config file contains 
the following distributed mode settings:
 - `distributed_mode` (true/false) Enables the distributed mode
 - `num_nodes` (int) Number of nodes for the simulation
  
Using the distributed mode requires the user to declare how many nodes or more specifically, how many client actors will connect to the server. Once all 
actors are connected the server will start the simulation.
```bash
# To start the Server
$summa_exe -g 1 $total_hru -c $config_summa -s

# To start the client
$summa_exe -g 1 $total_hru -c $config_summa --host=$host
```


### Config File
The config file is a JSON file that is used to configure SUMMA-Actors. It is highly recommended to use a config file as it allows the user to fine tune how SUMMA-Actors will perform. Using `./summa_actors --gen-config` will generate a configuration file that can then be filled in. Below is a list of the available options and their descriptions.

#### Distributed_Settings
The distributed settings are used to configure the distributed mode of SUMMA-Actors. The following options are available:
  - distributed_mode: (true/false) Enables distributed mode
  - servers_list: (list of strings) List of hostnames for backup servers
  - port: (int) Port to use for communication
  - total_hru_count: (int) Total number of HRUs in the entire domain
  - num_hru_per_batch: (int) Number of HRUs to assemble into a batches

#### Summa_Actor
The Summa_Actor settings are used to restrict how many HRU actor can run at once. The following options are available:
  - max_gru_per_job: (int) Maximum number of HRUs that can be run at once

#### Job_Actor
The Job_Actor settings can specify the file manager path so it does not need to be specified on the command line, and the maximum number of times to attempt an HRU before giving up. The following options are available:
  - file_manager_path: (string) Path to the file manager file
  - max_run_attempts: (int) Maximum number of times to attempt an HRU before giving up

#### File_Access_Actor
The File_Access_Actor settings are used to configure the file access actor. The following options are available:
  - num_partitions_in_output_buffer: (int) Number of partitions in the output buffer
  - num_timesteps_in_output_buffer: (int) Number of timesteps in the output buffer

#### HRU_Actor
The HRU_Actor settings are used to configure the HRU actor. The following options are available:
  - print_output: (true/false) Print output to the screen
  - output_frequency: (int) Frequency to print output to the screen
  - dt_init_factor: (int) Factor to multiply dt_init by
  - rel_tol: (float) Relative tolerance for the HRU actor (Requires Sundials)
  - abs_tol: (float) Absolute tolerance for the HRU actor (Requires Sundials)

## Credits
The initial implementation of SUMMA is credited to the initial publications below. These 
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



