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
   - summa (https://github.com/CH-Earth/summa, develop branch or summa-sundials)
   - CMakeLists.txt
 - utils
 - README.md

First clone Summa-Actors to your workstation. Then cd into `build/` and clone `summa` into Summa-Actor's build directory as folder summa.

## Compiling SUMMA-Actors

### Dependencies
SUMMA-Actors has only one additional dependency, the [C++ Actor Framework](https://github.com/actor-framework/actor-framework). Instructions for installing the C++ Actor Framework can be found [in their readme](https://github.com/actor-framework/actor-framework#build-caf-from-source).

Additional dependencies required by both SUMMA and SUMMA actors are:
 * g++
 * gfortran
 * [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
 * [OpenBLAS](https://github.com/xianyi/OpenBLAS)
 * [SUNDIALS V6.6](https://github.com/LLNL/sundials/releases/tag/v6.6.0) (optional)

### Steps To Compile
 ```
 git clone https://git.cs.usask.ca/numerical_simulations_lab/actors/Summa-Actors.git
 cd Summa-Actors/build
 git clone https://github.com/CH-Earth/summa.git
 mkdir build_dir/
 cd build_dir/
 cmake ..
 make -j
 ```

## Running SUMMA-Actors
SUMMA-Actors can be run in two modes, distributed and non-distributed. The distributed mode is meant to run on a cluster, or it can be used to create ad-hoc clusters. The command line arguments are kept
as close to the original SUMMA as possible. However, there are options not yet available in SUMMA-Actors that are in the original SUMMA. Here is a full list of the Available options and unavailable options:
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


### Config File

{
    "Distributed_Settings": {
        "distributed_mode": false,
        "servers_list": [{"hostname": "cnic-giws-cpu-19001-04"}, {"hostname": "cnic-giws-utl-19002"}, {"hostname": "cnic-giws-utl-19003"}],
        "port": 4444,
        "total_hru_count": 800,
        "num_hru_per_batch": 50
    },
  
    "Summa_Actor": {
        "max_gru_per_job": 4000
    },
  
    "File_Access_Actor": {
      "num_partitions_in_output_buffer": 8,
      "num_timesteps_in_output_buffer": 500
    },
    
    "Job_Actor": {
        "file_manager_path": "/scratch/gwf/gwf_cmt/kck540/Summa-Actors/settings/file_manager_actors.txt",
        "max_run_attempts": 3
    },
  
    "HRU_Actor": {
        "print_output": true,
        "output_frequency": 100000,
        "dt_init_factor": 1
    }
}


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



