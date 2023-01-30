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

 First clone Summa-Actors to your workstation. Then cd into `build/` and clone `summa` or `summa-sundials` into Summa-Actor's build directory. 

## Compiling Summa-Actors
The best way to compile SUMMA-Actors is with `cmake`. The `CMakeLists.txt` is found in `build/cmake/` When compiling Summa-Actors there are two options, compiling with the sundials library and compiling without. Which compilation instructions to use are deffined by the following line inside the cmake file:
    
    option(SUNDIALS "Use SUNDIALS" ON)

ON compiles with Sundials, OFF compiles without sundials.

Summa-Actors has the following dependencies:
 * g++
 * gfortran
 * [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
 * [OpenBLAS](https://github.com/xianyi/OpenBLAS)
 * [C++ Actor Framework](https://github.com/actor-framework/actor-framework)

Once the following libraries have been installed and the proper Sundials setting is set use complete the following:
 - cd into `build/cmake/`
 - create another build directory
 - cd into `build/cmake/build`
 - run `cmake ..`
 - run `make`
 - a `summa_actors` executable should have been created inside the `bin/` directory


## Running SUMMA-Actors
Running SUMMA-Actors is done with the following command:
    ./summa_actor -g startGRU -n numGRU -c path_to_config_file

The values for -g and -n are integers where -c is the full path to the configuraiton file for summa actors.

The configuration file is a json file. The contents of the JSON file are below:

  {
    "Distributed_Settings": {
        "distributed_mode": false,
        "servers_list": [{"hostname": "simlab01"}, {"hostname": "simlab05"}],
        "port": 4444,
        "total_hru_count": 517315,
        "num_hru_per_batch": 1000
    },

    "Summa_Actor": {
      "max_gru_per_job": 500
    },

    "File_Access_Actor": {
      "num_partitions_in_output_buffer": 4,
      "num_timesteps_in_output_buffer": 500
    },
  
    "Job_Actor": {
      "file_manager_path": "/gladwell/kck540/Sundials_Settings/fileManager_actors.txt",
      "output_csv": false,
      "csv_path": ""
    },

    "HRU_Actor": {
      "print_output": true,
      "output_frequency": 1000
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



