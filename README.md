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
one of two ways. The first way is to modify the Makefile directly and the second 
is to invoke the makefile by shellscript:

### Method 1: Makefile
The method is best used for a workstation build that does not have access to a Compute Canada Software Stack where includes and libraries will need to be explicitly specified.

The Makefile is located in the `build` directory and the following variables will need to be uncommented and changed:
  - F_MASTER = Path/to/Summa-Actors/  # this is the directory above build/
  - FC = gfortran
  - CC = g++
  - INCLUDES = Path/to/netcdf/includes
  - LIBRARIES = Path/to/netcdf/lib 
                Path/to/openblas
                -lnetcdff -lopenblas
  - ACTORS_INCLUDES = $INCLUDES 
                      Path/to/CAF/includes
  - ACTORS_LIBRARIES = $LIBRARIES 
                       PATH/to/CAF/lib 
                       $(F_MASTER)/bin
        -lcaf_core -lcaf_io -lsumma -lopenblas -lnetcdff

After the following SUMMA-Actors can be compiled with `make`.

Once compiled you will need to set the library path variable with the following command (replace F_Master with the full path to the SUMMA-Actors directory):
`export LD_LIBRARY_PATH=/F_MASTER/bin`

See section Running SUMMA-Actors for instructions on how to use the program.
  
### Method 2: Shell Script
This method is best used for cluster environments that have access to the Compute Canada Software Stack. This method has been tested on the University of Saskatchewan's Copernicus and Compute Canada's Graham. This method will invoke the Makefile and no modifications should be made to the Makefile for this method.

The compilation script is located in the `build` as `compile_summa.sh`. The only variable within the compile script that needs to be changed is the `F_MASTER` path. This is the absolute path to the SUMMA-Actors directory.

Once the F_MASTER path has been specified the following command can be used to build SUMMA-Actors from the build directory:
`source compile_summa.sh`

This script should load all of the required modules from the Compute Canada Software Stack as well as set the environment variable `LD_LIBRARY_PATH` required for running SUMMA-Actors.

## Running Summa-Actors
SUMMA-Actors requires some setup to be configured properly. There is a python script `configuration.py` in the `config` directory to help with the process. See the `Config File` section for more information on how to use the configuration script.

SUMMA-Actors has the following options:
```
-g, Specifies the starting index of the first GRU to compute
-n, Specifies the number of GRUs to compute
-c, Specifies the path to the config directory
OPTIONAL:
--config-file, specifies specific configurations for the C++ Actor Framework. For more information about specific configurations see the following [link](https://actor-framework.readthedocs.io/en/stable/ConfiguringActorApplications.html#configuration-files). The most important config option is specifying the maximum number of threads. This can be done with the automatic configuration script, more on this in the next section.
```
The following is an example invocation of SUMMA-Actors:
`./summaMain -g 1 -n 10 -c /path/to/config/directory --config-file=/path/to/actors/config/file`

Once all the required settings of the `Summa_Actors_Settings.json` file have been filled out, running the python script `configuration.py` (the same file as before. If the Summa_Actors_Settings.json file exists this will invoke the setup for running SUMMA-Actors) will create the required slurm submission script as well as the config file for specifying the number of hardware threads for CAF++. This will also create all of the subdirectories necessary for program output and the you should see a file `run_summa.sh` in the build directory that should require no further modification. 

#### Config File ####
The configuration file is meant to help automate the job submission process. This does require some initial setup to get working properly. On the first install of SUMMA-Actors the `config` directory should only contain the file `configuration.py`. Running the python file with `python3 configuration.py` will create a file `Summa_Actors_Settings.json`. This file manages the settings and paths for SUMMA-Actors. Below is a detailed explanation of the settings as well as how to use the configuration script to set up a slurm submission script.

SUMMA-Actors settings is categorized into six objects, `configuration`, `JobSubmissionParams`, `SummaActors`, `FileAccessActor`, `JobActor`, and `HRUActor` containing multiple key value pairs.
  
* Configuration
The configuration object contains all the key:value pairs necessary for creating the fileManager.txt file that is used to specify the settings for SUMMA-Actors. This is very similar to fileManager.txt file used in the original version of SUMMA. Specifying the values in the JSON file will allow the configuration script to create the fileManager.txt file as well as specify the path to the job actor. Below is an example of how the settings should look.
```
"controlVersion": "SUMMA_FILE_MANAGER_V3.0.0",
"simStartTime": "1979-01-01 00:00",
"simEndTime": "2019-12-31 23:00",
"tmZoneInfo": "utcTime",
"settingsPath": "/Path/To/Settings/",
"forcingPath": "/Path/To/Forcing/Data/",
"outputPath": "/Path/To/Output/",  #NOTE: Specify a top level directory, subdirectories of slurm, netcdf, csv will be created automatically by the configuration script
"forcingFreq": "month",
"forcingStart": "1979-01-01",
"decisionsFile": "modelDecisions.txt",
"outputControlFile": "outputControl.txt",
"globalHruParamFile": "localParamInfo.txt",
"globalGruParamFile": "basinParamInfo.txt",
"attributeFile": "attributes.nc",
"trialParamFile": "trialParams.nc",
"forcingListFile": "forcingFileList.txt",
"initConditionFile": "coldState.nc",
"outFilePrefix": "SummaActors",
"vegTableFile": "TBL_VEGPARM.TBL",
"soilTableFile": "TBL_SOILPARM.TBL",
"generalTableFile": "TBL_GENPARM.TBL",
"noahmpTableFile": "TBL_MPTABLE.TBL"
```

* JobSubmission
The JobSubmission object contains all the key:value pairs necessary for creating the slurm submission script. The script will be created automatically from the inputted settings with the configuration settings. Below is an example of how the settings should look.
```
"cpus-per-task": 8,
"memory": "32G",
"job-name": "SummaActors",
"account": "Account Name",
"numHRUs": 517315,
"maxNumberOfJobs": 517,
"maxGRUsPerSubmission": 1000,
"executablePath": "/Path/To/SUMMA"
```
SUMMA-Actors is still a work in progress so the specific settings for running jobs optimally may depend on the specifics of your workflow. The testing for SUMMA-Actors has been done with the North American Domain Dataset. For this running jobs of 1000 HRUs with 8 CPUs and 32 GB of Ram has been the most optimal. You are welcome to experiment and find what works best for you.

NOTE: For optimal scheduler performance it is recommended to use 1 core per 4GB of RAM. So if you wanted to use 4 CPUs you should only use 16 GB of RAM. This is not a requirement and more of a guideline. Running SUMMA-Actors with less RAM could result in issues if the number of HRUs per Submission is not adjusted accordingly as well as some other settings in the `SummaActor` settings. See the `general guidelines` section for more information.

* SummaActor
The SummaActor object contains key:value pairs necessary for controlling how large the output structure is as well as how many GRUs run at one time within a job. These are necessary values to control the RAM the program uses. When you submit a job of 1000 GRUs and the `maxGRUPerJob` value is set to 500, SUMMA-Actor will only run 500 GRUs at one time. It will essentially complete all 1000 GRUs in two separate batches, computing one batch of 500 then another batch. Each batch will create their own output file. Below is an example of how these settings should look.
```
"OuputStructureSize": 250,  # How many timesteps of data the output structure can hold at once before needing to write the data to a file.  
"maxGRUPerJob": 500
```

* FileAccessActor
In order to speed up the write speed of SUMMA-Actors it is necessary to write output in large chunks. The setting in the FileAccessActor controls how many chunks the output structure is divided into. A good rule of thumb is to use as many vectors as there are CPUs, this will ensure optimal performance. Below is an example of what these settings should look like.
```
"num_vectors_in_output_manager": 8 
```

* JobActor
These settings control the path to the fileManager.txt file. This is set up automatically by the configuration script. You may also print output as csv. These are specific HRU timings of how long each HRU spent in certain parts of the program. The path will be set up automatically upon running the configuration script. The only setting that needs to be changed is the boolean value to control if output is desired. Below is an example of how these settings should look.
```
"FileManagerPath": "", # SET AUTOMATICALLY 
"outputCSV": true,
"csvPath": ""          # SET AUTOMATICALLY
```

* HRUActor
These settings control how often if at all the HRU will display output. The frequency is per timestep. If this value is set very low it will slow down the execution of the program. A good value is anything over 10000 for production runs. Below is an example for how these settings should look.
```
"printOutput": true,
"outputFrequency": 50000
```

#### General Guidelines ####
For the optimal performance and the best chance of jobs succeeding it is recommend to use the following settings:
```
"cpus-per-task": 8,
"memory": "32G",
"maxGRUsPerSubmission": 1000,
"maxGRUPerJob": 500
"num_vectors_in_output_manager": 8 
"outputFrequency": 50000
```
Feel free to experiment with different values, although you may encounter out of memory errors with your jobs. The amount of RAM used is very dependent on which HRUs are being computing. If you want to use 
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



