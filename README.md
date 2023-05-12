# SUMMA-Actors: Structure for Unifying Multiple Modeling Alternatives with Actors

SUMMA-Actors is a modified version of the already existing SUMMA software that can be 
found [here](https://github.com/CH-Earth/summa#readme). SUMMA-Actors uses the Actor Model to increase scalability and fault-tolerance. It is built using the [C++ Actor Framework](https://github.com/actor-framework/actor-framework). 

## Documentation
A more in-depth documentation can be found [here](https://git.cs.usask.ca/numerical_simulations_lab/actors/Summa-Actors/-/wikis/home). SUMMA-Actors depends on many files from the [original SUMMA repo](https://github.com/CH-Earth/summa). Below is a quick start guide for compiling and running SUMMA-Actors, please consult our wiki for more in-depth documentation. Consider creating an issue for any missing documentaion, asking questions, or providing suggestions.

## Directory Structure
SummaActors is set up with the following sub-directoies, we will consider the top level Summa Actors directory the `root_dir`:
 - bin
 - build
   - cmake 
   - includes
   - makefiles
   - source
   - summa (https://github.com/CH-Earth/summa)
 - utils
 - README.md

 First clone Summa-Actors to your workstation. Then cd into `build/` and clone `summa-sundials` into Summa-Actor's build directory (you can name it whatever you want, `{$YOUR_SUMMA_DIR}`)

## Compiling Summa-Actors
To compile SUMMA-Actors, use `cmake` with the `CMakeLists.txt` located in `build/{$YOUR_SUMMA_DIR}/build/cmake`. You have the option to compile with or without the sundials library, depending on the `-DCMAKE_BUILD_TYPE=build_type` option.  
If compiling with sundials make sure to install the `sundials IDA solver version 6.3.0` before attempting to compile SUMMA-Actors. Then chose Sundials_Actors, Sundials_Actors_Debug, Sundials_Actors_Cluster, or Sundials_Actors_Cluster_Debug. Otherwise, chose 
BE_Actors, BE_Actors_Debug, BE_Actors_Cluster, or BE_Actors_Cluster_Debug.

Before compiling, make sure to install the following dependencies:
 * g++
 * gfortran
 * [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
 * [OpenBLAS](https://github.com/xianyi/OpenBLAS)
 * [C++ Actor Framework](https://github.com/actor-framework/actor-framework)

Here are the steps to compile SUMMA-Actors:
 - cd into `build/{$YOUR_SUMMA_DIR}/build/cmake`
 - change the build type in file `build.cluster.bash` to one of the above containing Cluster and Actors
 - run `./build.cluster.bash` 
 - or if you are not using a cluster, you can do the same on the `build.mac.bash` if you have installed the Actors Framework
- The `summa_actors` executable is created in the `bin/` directory.

SUMMA-Actors supports four build types: Debug, Cluster, Release, and Cluster_Debug. The default build type is Release. You can set the build type by using the `-DCMAKE_BUILD_TYPE=build_type` option, where `build_type` is one of the four options listed above. To compile with the Cluster build type, make sure to load the following modules with `module load` before compiling when working on clusters:
 - gcc/9.3.0
 - netcdf-fortran
 - openblas
 - caf

## Running SUMMA-Actors
Running SUMMA-Actors is done with the following command:
    ./summa_actor -g startGRU -n numGRU -c path_to_config_file

If you are running SUMMA-Actors on a cluster, you will need to specify the number of threads when not using whole nodes.
  This can be done with the --caf.scheduler.max-threads option
    ./summa_actor -g startGRU -n numGRU -c path_to_config_file --caf.scheduler.max-threads $SLURM_CPUS_PER_TASK

The values for -g and -n are integers where -c is the full path to the configuraiton file for summa actors.

The configuration file is a json file. The contents of the JSON file are below:

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


The settings above should work for most use cases, some of the feautures we want to automate such as max_gru_per_job. However, the only field that you should have to adjust is the `file_manager_path`. This is the path to the file that manages the complete configuration of the SUMMA simulation. The summa confiuration files are explained in more depth in the follwoing (documentation)[https://summa.readthedocs.io/en/latest/input_output/SUMMA_input/]



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



