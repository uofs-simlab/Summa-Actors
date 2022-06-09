#### parent directory of the 'build' directory ####
# F_MASTER =

#### fortran compiler ####
# FC = 

#### C++ compiler ####
# CC =

#### Includes AND Libraries ####
# INCLUDES =
# LIBRARIES =

# ACTORS_INCLUDES =
# ACTORS_LIBRARIES =


# Production runs
FLAGS_NOAH = -O3 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC
FLAGS_COMM = -O3 -ffree-line-length-none -fmax-errors=0 -fPIC
FLAGS_SUMMA = -O3 -ffree-line-length-none -fmax-errors=0 -fPIC
FLAGS_ACTORS = -O3

# # Debug runs
# FLAGS_NOAH = -pg -g -O0 -ffree-form -ffree-line-length-none -fmax-errors=0 -fbacktrace -Wno-unused -Wno-unused-dummy-argument -fPIC
# FLAGS_COMM = -pg -g -O0 -Wall -ffree-line-length-none -fmax-errors=0 -fbacktrace -fcheck=bounds -fPIC
# FLAGS_SUMMA = -pg -g -O0 -Wall -ffree-line-length-none -fmax-errors=0 -fbacktrace -fcheck=bounds -fPIC
# FLAGS_ACTORS = -pg -g -O0 -Wall


#========================================================================
# PART 1: Define directory paths
#========================================================================

# Core directory that contains source code
F_KORE_DIR = $(F_MASTER)/build/source

# Location of the compiled modules
MOD_PATH = $(F_MASTER)/build

# Define the directory for the executables
EXE_PATH = $(F_MASTER)/bin

#========================================================================
# PART 2: Assemble all of the SUMMA sub-routines
#========================================================================

# Define directories
DRIVER_DIR = $(F_KORE_DIR)/driver
HOOKUP_DIR = $(F_KORE_DIR)/hookup
NETCDF_DIR = $(F_KORE_DIR)/netcdf
DSHARE_DIR = $(F_KORE_DIR)/dshare
NUMREC_DIR = $(F_KORE_DIR)/numrec
NOAHMP_DIR = $(F_KORE_DIR)/noah-mp
ENGINE_DIR = $(F_KORE_DIR)/engine
INTERFACE_DIR = $(F_KORE_DIR)/interface
JOB_ACTOR_DIR = $(INTERFACE_DIR)/job_actor
FILE_ACCESS_DIR = $(INTERFACE_DIR)/file_access_actor
HRU_ACTOR_DIR = $(INTERFACE_DIR)/hru_actor

#  utilities
SUMMA_NRUTIL= \
		nrtype.f90 \
		f2008funcs.f90 \
		nr_utility.f90
NRUTIL = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_NRUTIL))

# Numerical recipes procedures
# NOTE: all numerical recipes procedures are now replaced with free versions
SUMMA_NRPROC= \
		expIntegral.f90 \
		spline_int.f90
NRPROC = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_NRPROC))

# Hook-up modules (set files and directory paths)
SUMMA_HOOKUP= \
		ascii_util.f90 \
		summaActors_FileManager.f90
HOOKUP = $(patsubst %, $(HOOKUP_DIR)/%, $(SUMMA_HOOKUP))

# Data modules
SUMMA_DATAMS= \
		multiconst.f90 \
		var_lookup.f90 \
		data_types.f90 \
		globalData.f90 \
		flxMapping.f90 \
		get_ixname.f90 \
		popMetadat.f90 \
		outpt_stat.f90
DATAMS = $(patsubst %, $(DSHARE_DIR)/%, $(SUMMA_DATAMS))

# utility modules
SUMMA_UTILMS= \
		time_utils.f90 \
		mDecisions.f90 \
		snow_utils.f90 \
		soil_utils.f90 \
		updatState.f90 \
		matrixOper.f90
UTILMS = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_UTILMS))

# Model guts
SUMMA_MODGUT= \
		MODGUT = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_MODGUT))

# Solver
SUMMA_SOLVER= \
		vegPhenlgy.f90 \
		diagn_evar.f90 \
		stomResist.f90 \
		groundwatr.f90 \
		vegSWavRad.f90 \
		vegNrgFlux.f90 \
		ssdNrgFlux.f90 \
		vegLiqFlux.f90 \
		snowLiqFlx.f90 \
		soilLiqFlx.f90 \
		bigAquifer.f90 \
		computFlux.f90 \
		computResid.f90 \
		computJacob.f90 \
		eval8summa.f90 \
		summaSolve.f90 \
		systemSolv.f90 \
		varSubstep.f90 \
		opSplittin.f90 \
		coupled_em.f90
		
SOLVER = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_SOLVER))

# Interface code for Fortran-C++
SUMMA_INTERFACE= \
		cppwrap_datatypes.f90 \
		cppwrap_auxiliary.f90 \
		cppwrap_metadata.f90 \

INTERFACE = $(patsubst %, $(INTERFACE_DIR)/%, $(SUMMA_INTERFACE))

SUMMA_FILEACCESS_INTERFACE = \
		initOutputStruc.f90 \
		deallocateOutputStruc.f90 \
		cppwrap_fileAccess.f90

FILEACCESS_INTERFACE = $(patsubst %, $(FILE_ACCESS_DIR)/%, $(SUMMA_FILEACCESS_INTERFACE))

SUMMA_JOB_INTERFACE = \
		cppwrap_job.f90

JOB_INTERFACE =  $(patsubst %, $(JOB_ACTOR_DIR)/%, $(SUMMA_JOB_INTERFACE))

SUMMA_HRU_INTERFACE = \
		cppwrap_hru.f90

HRU_INTERFACE = $(patsubst %, $(HRU_ACTOR_DIR)/%, $(SUMMA_HRU_INTERFACE))
		

# Define routines for SUMMA preliminaries
SUMMA_PRELIM= \
		conv_funcs.f90 \
		sunGeomtry.f90 \
		convE2Temp.f90 \
		allocspaceActors.f90 \
		alloc_file_access.f90\
		checkStruc.f90 \
		childStruc.f90 \
		ffile_info.f90 \
		read_attribute.f90 \
		read_pinit.f90 \
		pOverwrite.f90 \
		read_paramActors.f90 \
		paramCheck.f90 \
		check_icondActors.f90 \
		# allocspace.f90
PRELIM = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_PRELIM))

SUMMA_NOAHMP= \
		module_model_constants.F \
		module_sf_noahutl.F \
		module_sf_noahlsm.F \
		module_sf_noahmplsm.F

NOAHMP = $(patsubst %, $(NOAHMP_DIR)/%, $(SUMMA_NOAHMP))

# Define routines for the SUMMA model runs
SUMMA_MODRUN = \
		indexState.f90 \
		getVectorz.f90 \
		updateVars.f90 \
		var_derive.f90 \
		read_forcingActors.f90 \
		access_forcing.f90\
		access_write.f90 \
		derivforce.f90 \
		snowAlbedo.f90 \
		canopySnow.f90 \
		tempAdjust.f90 \
		snwCompact.f90 \
		layerMerge.f90 \
		layerDivide.f90 \
		volicePack.f90 \
		qTimeDelay.f90
MODRUN = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_MODRUN))

# Define NetCDF routines
# OutputStrucWrite is not a netcdf subroutine and should be
# moved
SUMMA_NETCDF = \
		netcdf_util.f90 \
		def_output.f90 \
		outputStrucWrite.f90 \
		writeOutput.f90	\
		read_icondActors.f90
NETCDF = $(patsubst %, $(NETCDF_DIR)/%, $(SUMMA_NETCDF))

# ... stitch together common programs
COMM_ALL = $(NRUTIL) $(NRPROC) $(HOOKUP) $(DATAMS) $(UTILMS)

# ... stitch together SUMMA programs
SUMMA_ALL = $(NETCDF) $(PRELIM) $(MODRUN) $(SOLVER) 

# Define the driver routine
SUMMA_DRIVER= \
		summaActors_type.f90 \
		summaActors_util.f90 \
		summaActors_globalData.f90 \
		summaActors_init.f90 \
		SummaActors_setup.f90 \
		summaActors_restart.f90 \
		summaActors_forcing.f90 \
		SummaActors_modelRun.f90 \
		summaActors_alarms.f90 \
		summaActors_wOutputStruc.f90
		
		
DRIVER = $(patsubst %, $(DRIVER_DIR)/%, $(SUMMA_DRIVER))

ACTORC = $(F_KORE_DIR)/actors/main.cc

ACTOR_TEST = $(F_KORE_DIR)/testing/testing_main.cc


#========================================================================
# PART 3: compilation
#======================================================================

# Compile
all: lib main

lib: compile_noah compile_comm compile_summa link clean

main: actors actorsLink actorsClean

test: actors_test actors_testLink actorsClean

# compile Noah-MP routines
compile_noah:
	$(FC) $(FLAGS_NOAH) -c $(NRUTIL) $(NOAHMP)

# compile common routines
compile_comm:
	$(FC) $(FLAGS_COMM) -c $(COMM_ALL) $(INCLUDES)

# compile SUMMA routines
compile_summa: 
	$(FC) $(FLAGS_SUMMA) -c $(SUMMA_ALL) $(DRIVER) $(INTERFACE) $(JOB_INTERFACE) $(FILEACCESS_INTERFACE) $(HRU_INTERFACE) $(INCLUDES)

# generate library
link:   
	$(FC) -shared *.o -o libsumma.so  
	mv libsumma.so $(F_MASTER)/bin

# compile the c++ portion of SummaActors
actors:
	$(CC) $(FLAGS_ACTORS) -c $(ACTORC) -std=c++17 $(ACTORS_INCLUDES)

actorsLink:
	$(CC) -o summaMain *.o $(ACTORS_LIBRARIES)
	mv summaMain $(F_MASTER)/bin

#### COMPILE TESTING FILES ####
actors_test:
	$(CC) $(FLAGS_ACTORS) -c $(ACTOR_TEST) -std=c++17 $(ACTORS_INCLUDES)

actors_testLink:
	$(CC) -o summaTest *.o $(ACTORS_LIBRARIES)

actorsClean:
	rm *.o
# Remove object files
clean:
	rm -f *.o *.mod soil_veg_gen_parm__genmod.f90

	
clean_lib:
	rm *.so




