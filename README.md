# SUMMA-Actors: Structure for Unifying Multiple Modeling Alternatives with Actors
SUMMA-Actors is a powerful extension of the existing [SUMMA](https://github.com/CH-Earth/summa#readme) hydrological modeling framework, designed to leverage the Actor Model for enhanced scalability and fault-tolerance. SUMMA-Actors is built using the [C++ Actor Framework](https://github.com/actor-framework/actor-framework) and the key highlights include:
  * Scalability: Actors process messages concurrenty, effortlessly scaling to thousands of HRUs/GRUs.
  * Fault-Tolerance: Individual HRUs/GRUs can fail without affecting the rest of the simulation.

## Resources
  * SUMMA-Actors Wiki: https://github.com/uofs-simlab/Summa-Actors/wiki
  * SUMMA Documentation: https://summa.readthedocs.io/en/latest/
  * Laugh-test framework: https://git.cs.usask.ca/numerical_simulations_lab/hydrology/laugh_tests

## Bug Reports, Feature Requests, and Questions
For bug reports, feature requests, and questions, please open an issue on the GitHub at https://github.com/uofs-simlab/Summa-Actors/issues

## Quick Start
SUMMA-Actors is separate from the original SUMMA codebase and requires a 
"Fortran" version of SUMMA to be downloaded from GitHub. For those new to 
SUMMA, we recommend using the latest version of SUMMA (4.x.x), as it includes 
the most up-to-date features and bug fixes. Below, we only provide steps for 
both version 4.x.x of SUMMA and the specific steps for version 3.x.x can be 
found in our wiki: https://github.com/uofs-simlab/Summa-Actors/wiki

Below we provide some background to SUMMA-Actors' structure, but for those 
who do not like long documentation you can skip to compilation steps here:
[Version 4.x.x Build Instructions](###Version-4.x.x-Build-Instructions)

### Directory Structure
Upon downloading the SUMMA-Actors repository, you will be presented with 
the following directory structure:
```
Summa-Actors/
├── bin/
├── build/
|   ├── build_scripts/
│   |   ├── build.sh
|   ├── cmake/
│   ├── includes/
│   ├── source/
├── utils/
|   ├── ciroh_build_scripts
|   ├── containers/
│   ├── dependencies/
│   └── docs
├── .gitignore
└── README.md
```
 * `bin/`: Contains the compiled SUMMA-Actors executable after building
 * `build/`: Contains the build scripts, the Summa-Actors specific source code, 
            and a Fortran version of SUMMA.
 * `utils/`: Contains utility scripts and Dockerfiles for building and running 
            SUMMA-Actors. **This folder also contains `dependencies`, 
            which has scripts for installing many of the libraries needed**             


### Dependencies
  SUMMA-Actors requires the following dependencies to be installed on you 
  system:
  * [SUMMA](https://github.com/ashleymedin/summa/tree/develop) 
  * g++
  * gfortran
  * [OpenBLAS](https://github.com/xianyi/OpenBLAS)
  * [NetCDF-Fortran](https://github.com/Unidata/netcdf-fortran)
  * NetCDF-C 
  * [tbb](https://github.com/oneapi-src/oneTBB) (Threading Building Blocks) 
  * [C++ Actor Framework (1.0.0)](https://github.com/actor-framework/actor-framework/releases/tag/1.0.0)
  * [Sundials v7.0.0 (Only for SUMMA version 4.x.x)](https://github.com/LLNL/sundials/releases/tag/v7.0.0)

#### Installing Dependencies
We assume for these instructions that you have a C++ compiler and a 
Fortran compiler. This code is tested with g++ and gfortran, but other
compilers may work.

For LAPACK, NetCDF-Fortran, NetCDF-C, C++ Actor Framework, and Sundials, we 
have prepared scripts located in the `utils/dependencies` folder. These scripts 
will automatically download and install the dependencies in the 
`utils/dependencies/install` folder. We have configured a script to 
automatically look here for the dependencies.

If you are using a module system, you can modify the `build.sh` script in the 
`build/build_scripts` folder to point to the correct locations.

To install each dependency, follow these steps, some libraries will take some 
time to compile: 
  1) `cd utils/dependencies`
  2) `./install_lapack.sh`
  3) `./install_netcdf.sh`
  5) `./install_caf.sh`
  6) `./install_sundials.sh`

**NOTE: Installing SUMMA is part of the build instructions below**
 
### Version 4.x.x Build Instructions
  1) git clone https://github.com/uofs-simlab/Summa-Actors.git
  2) cd Summa-Actors/build/
  3) git clone -b develop https://github.com/ashleymedin/summa.git
  4) cd build_scripts/
  5) ./build.sh

Note: If you did not install the dependencies in the `utils/dependencies` folder,
you will need to modify append to the $CMAKE_PREFIX_PATH environment variables
to include the location of where you installed the dependencies. If you use
a module system, this should handle appending the correct paths.

## Running SUMMA-Actors
Running SUMMA-Actors is similar to running the original version of SUMMA. **Input and configuration files remain identical** alowing exising projects and `fileManager.txt` files to be used seamlessly with SUMMA-Actors. Please refer to the [SUMMA documentation](https://summa.readthedocs.io/en/latest/) regarding input files and simulation configuration. The only difference, if desired, is the option to use a `config.json` file to fine tune how SUMMA-Actors will perform. Please refer to the [relevant section](###Config-File-and-Advanced-Features) for more information on the `config.json` file and the more advanced features of SUMMA-Actors.

Below is the help message for SUMMA-Actors, which provides a brief overview of both the avialable options and the currently unimplemented options.
```   
Usage: summa_actors -m master_file [-g startGRU countGRU] [-c config_file] [-b backup_server] [-s server_mode]
  Available options:
    -m, --master:         Define path/name of master file (can be specified in config)
    -g, --gru:            Run a subset of countGRU GRUs starting from index startGRU 
    -c, --config:         Path name of the Summa-Actors config file (optional but recommended)
    -s, --suffix          Add fileSuffix to the output files
        --gen-config:     Generate a config file
    -b, --backup-server:  Start backup server, requires a server and config_file
        --server-mode:    Enable server mode
    -h, --help:           Print this help message
  Unimplemented Options:
    -n, --newFile         Define frequency [noNewFiles,newFileEveryOct1] of new output files
    -h, --hru             Run a single HRU with index of iHRU
    -r, --restart         Define frequency [y,m,d,e,never] to write restart files
    -p, --progress        Define frequency [m,d,h,never] to print progress
    -v, --version         Display version information of the current build
```

### Example Usage

```bash
./summa_actors -g 1 10 -m /path/to/master_file.txt
```

### Config File and Advanced Features

#### Config File
The `config.json` file is a JSON file that is used to configure SUMMA-Actors. It can be generated by running `./summa_actors --gen-config`, and allows some fine tunning of the SUMMA-Actors program including operating SUMMA-Actors in additional modes. The details of the config file can be found on our wiki page [here](https://github.com/uofs-simlab/Summa-Actors/wiki/Config-File). 

Example usage of the `config.json` file is as follows. Note that the `config.json` file has a field for the `file_master.txt` file, so the `-m` flag is not required when using the `config.json` file.

```bash
./summa_actors -g 1 10 -c /path/to/config.json
```

#### Advanced Features
SUMMA-Actors has additional feature that are not covered in this README. For more information on these features, please refer to the [SUMMA-Actors Advanced Features Wiki Page](https://github.com/uofs-simlab/Summa-Actors/wiki/Advanced-Features). Here is a short summary of some of the optional features:
 
 * Distributed Mode: Run SUMMA-Actors across nodes, or create your own ad-hoc cluster.
 * Data Assimilation Mode: Use SUMMA-Actors to perform data assimilation, restricting all HRUs to complete a timestep before moving to the next.
 * Asynchronous Mode: Default mode of SUMMA-Actors, where HRUs can complete timesteps concurrently and independently.

## Scientific Use:
Please feel free to contribute to our project by submitting pull requests or opening issues. We only ask that if you use SUMMA-Actors that you kindly cite one of our publications:
```bibtex
@article{klenk2024improving,
  title={Improving resource utilization and fault tolerance in large simulations via actors},
  author={Klenk, Kyle and Spiteri, Raymond J},
  journal={Cluster Computing},
  pages={1--18},
  year={2024},
  publisher={Springer}
}

@inproceedings{klenk2024high,
  title={High-Throughput Scientific Computation with Heterogeneous Clusters: A Kitchen-Sink Approach using the Actor Model},
  author={Klenk, Kyle and Moayeri, Mohammad Mahdi and Spiteri, Raymond J},
  booktitle={Proceedings of the 2024 SIAM Conference on Parallel Processing for Scientific Computing (PP)},
  pages={78--89},
  year={2024},
  organization={SIAM}
}
```


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


