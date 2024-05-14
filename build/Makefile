F_MASTER = /home/jovyan/Summa-Project/Summa-Actors

SUMMA_CORE = $(F_MASTER)/build/summa/build/source
ACTORS_SRC = $(F_MASTER)/build/source
ACTORS_INC = $(F_MASTER)/build/includes

FC = /usr/bin/gfortran

CXX = /usr/bin/g++

F_INCLUDES = -I/usr/include -I/usr/local/sundials/include -I/usr/local/sundials/fortran
F_LIBRARIES = -L/usr/lib/x86_64-linux-gnu -lnetcdff -lnetcdf \
							-L/usr/local/sundials/lib/ -lsundials_fcore_mod \
              -lsundials_fnvecserial_mod -lsundials_fsunlinsoldense_mod \
							-lsundials_fsunmatrixdense_mod -lsundials_fsunlinsolband_mod \
							-lsundials_fsunmatrixband_mod -lsundials_fsunnonlinsolnewton_mod \
						  -lsundials_fida_mod -lsundials_fkinsol_mod

C_INCLUDES = -I$(ACTORS_INC)/file_access_actor \
						 -I$(ACTORS_INC)/global \
						 -I$(ACTORS_INC)/gru_actor \
						 -I$(ACTORS_INC)/hru_actor \
						 -I$(ACTORS_INC)/job_actor \
						 -I$(ACTORS_INC)/summa_actor \
						 -I/usr/local/include
C_LIBRARIES = -L$(F_MASTER)/build -lsumma \
							-L/usr/local/lib -lcaf_core -lcaf_io \
							-L/usr/lib/x86_64-linux-gnu -lnetcdf -lnetcdff -lopenblas \
							-L/usr/local/sundials/lib/ -lsundials_fcore_mod \
              -lsundials_fnvecserial_mod -lsundials_fsunlinsoldense_mod \
							-lsundials_fsunmatrixdense_mod -lsundials_fsunlinsolband_mod \
							-lsundials_fsunmatrixband_mod -lsundials_fsunnonlinsolnewton_mod \
						  -lsundials_fida_mod -lsundials_fkinsol_mod

FLAGS_NOAH = -O3 -DSUNDIALS_ACTIVE -DV4_ACTIVE -DACTORS_ACTIVE -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors 
FLAGS_ALL = -O3 -DSUNDIALS_ACTIVE -DV4_ACTIVE -DACTORS_ACTIVE -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors -cpp
FLAGS_CXX = -O3 -DSUNDIALS_ACTIVE -DV4_ACTIVE -Wfatal-errors -std=c++17




HOOKUP_DIR = $(SUMMA_CORE)/hookup
NETCDF_DIR = $(SUMMA_CORE)/netcdf
DSHARE_DIR = $(SUMMA_CORE)/dshare
NUMREC_DIR = $(SUMMA_CORE)/numrec
NOAHMP_DIR = $(SUMMA_CORE)/noah-mp
ENGINE_DIR = $(SUMMA_CORE)/engine
DRIVER_DIR = $(SUMMA_CORE)/driver

SUMMA_NOAHMP= \
		module_model_constants.F \
		module_sf_noahutl.F \
		module_sf_noahlsm.F \
		module_sf_noahmplsm.F
NOAHMP = $(patsubst %, $(NOAHMP_DIR)/%, $(SUMMA_NOAHMP))

SUMMA_NRUTIL= \
		nrtype.f90 \
		f2008funcs.f90 \
		nr_utility.f90
NRUTIL = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_NRUTIL))

SUMMA_NRPROC= \
		expIntegral.f90 \
		spline_int.f90 \
		hyp_2F1.f90
NRPROC = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_NRPROC))

SUMMA_HOOKUP= \
		ascii_util.f90 \
		summaFileManager.f90
HOOKUP = $(patsubst %, $(HOOKUP_DIR)/%, $(SUMMA_HOOKUP))

# Data modules
SUMMA_DATAMS= \
		var_lookup.f90 \
		data_types.f90 \
		globalData.f90 \
		flxMapping.f90 \
		get_ixname.f90 \
		multiconst.f90 \
		outpt_stat.f90 \
		popMetadat.f90 \
		type4ida.f90   \
		type4kinsol.f90
DATAMS = $(patsubst %, $(DSHARE_DIR)/%, $(SUMMA_DATAMS))

ACTORS_DATAMS = \
	  actor_data_types.f90
DATAMS += $(patsubst %, $(ACTORS_SRC)/global/%, $(ACTORS_DATAMS))

SUMMA_UTILMS = \
		time_utils.f90 \
		matrixOper.f90 \
		mDecisions.f90 \
		snow_utils.f90 \
		soil_utils.f90 \
		updatState.f90 \
		soil_utilsAddPrime.f90 \
		updatStateWithPrime.f90
UTILMS = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_UTILMS))


SUMMA_NETCDF = \
		netcdf_util.f90 \
		def_output.f90 \
		modelwrite.f90 \
		read_icond.f90
NETCDF = $(patsubst %, $(NETCDF_DIR)/%, $(SUMMA_NETCDF))

SUMMA_PRELIM = \
		allocspace.f90  \
		enthalpyTemp.f90 \
		check_icond.f90 \
		checkStruc.f90 \
		childStruc.f90 \
		conv_funcs.f90 \
		ffile_info.f90 \
		read_pinit.f90 \
		read_attrb.f90 \
		paramCheck.f90 \
		pOverwrite.f90 \
		sunGeomtry.f90 \
		read_param.f90
PRELIM = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_PRELIM))

SUMMA_MODRUN = \
		canopySnow.f90 \
		derivforce.f90 \
		getVectorz.f90 \
		indexState.f90 \
		var_derive.f90 \
		layerMerge.f90 \
		layerDivide.f90 \
		qTimeDelay.f90 \
		snowAlbedo.f90 \
		snwCompact.f90 \
		tempAdjust.f90 \
		updateVars.f90 \
		volicePack.f90 \
		tol4ida.f90 \
		updateVarsWithPrime.f90
MODRUN = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_MODRUN))

SUMMA_SOLVER = \
		vegPhenlgy.f90 \
		bigAquifer.f90 \
		stomResist.f90 \
		vegNrgFlux.f90 \
		ssdNrgFlux.f90 \
		vegLiqFlux.f90 \
		vegSWavRad.f90 \
		snowLiqFlx.f90 \
		soilLiqFlx.f90 \
		groundwatr.f90 \
		computFlux.f90 \
		computHeatCap.f90 \
		computJacob.f90 \
		computResid.f90 \
		computSnowDepth.f90 \
		computThermConduct.f90 \
		computResidWithPrime.f90 \
		computJacobWithPrime.f90 \
		eval8summaWithPrime.f90 \
		eval8summa.f90 \
		summaSolve4kinsol.f90 \
		summaSolve4ida.f90 \
		summaSolve4numrec.f90 \
		diagn_evar.f90 \
		systemSolv.f90 \
		varSubstep.f90 \
		opSplittin.f90 \
		coupled_em.f90
SOLVER = $(patsubst %, $(ENGINE_DIR)/%, $(SUMMA_SOLVER))

SUMMA_DRIVER = \
		summa_type.f90 \
		summa_setup.f90 \
		summa_restart.f90 \
		summa_alarms.f90 \
		summa_globalData.f90
DRIVER = $(patsubst %, $(DRIVER_DIR)/%, $(SUMMA_DRIVER))

SUMMA_INTERFACE = \
		cppwrap_auxiliary.f90 \
		cppwrap_datatypes.f90 \
		cppwrap_metadata.f90 \
		c_interface_module.f90
INTERFACE = $(patsubst %, $(ACTORS_SRC)/global/%, $(SUMMA_INTERFACE))

SYS_INIT_INTERFACE = \
		batch_distributer_actor.f90
SYS_INIT = $(patsubst %, $(ACTORS_SRC)/system_initialization/%, $(SYS_INIT_INTERFACE))

FILE_ACCESS_INTERFACE = \
		output_structure.f90 \
		summa_init_struc.f90 \
		forcing_file_info.f90 \
		file_access_actor.f90 \
		fileAccess_writeOutput.f90
FILE_ACCESS = $(patsubst %, $(ACTORS_SRC)/file_access_actor/%, $(FILE_ACCESS_INTERFACE))

JOB_INTERFACE = \
		gru_struc.f90
JOB = $(patsubst %, $(ACTORS_SRC)/job_actor/%, $(JOB_INTERFACE))

GRU_INTERFACE = \
		gru_interface.f90
GRU = $(patsubst %, $(ACTORS_SRC)/gru_actor/%, $(GRU_INTERFACE))

HRU_INTERFACE = \
		hru_init.f90 \
		hru_read.f90 \
		hru_modelRun.f90 \
		hru_writeOutput.f90 \
		hru_interface.f90
HRU = $(patsubst %, $(ACTORS_SRC)/hru_actor/%, $(HRU_INTERFACE))

ACTORS_GLBOAL = \
		auxiliary.cpp \
		fileManager.cpp \
		settings_functions.cpp \
		timing_info.cpp
ACTORS_CPP = $(patsubst %, $(ACTORS_SRC)/global/%, $(ACTORS_GLBOAL))

SYS_INIT_CPP_FILES = \
		batch.cpp \
		batch_container.cpp \
		client.cpp \
		client_container.cpp \
		summa_global_data.cpp \
		summa_actor.cpp \
		summa_backup_server.cpp \
		summa_client.cpp \
		summa_server.cpp
SYS_INIT_CPP = $(patsubst %, $(ACTORS_SRC)/system_initialization/%, $(SYS_INIT_CPP_FILES))

FILE_ACCESS_CPP_FILES = \
		summa_init_struc.cpp \
		file_access_actor.cpp \
		forcing_file_info.cpp \
		output_container.cpp
FILE_ACCESS_CPP = $(patsubst %, $(ACTORS_SRC)/file_access_actor/%, $(FILE_ACCESS_CPP_FILES))

JOB_CPP_FILES = \
		GRU.cpp \
		gru_struc.cpp \
		job_actor.cpp \
		async_mode.cpp \
		data_assimilation_mode.cpp \
		job_utils.cpp \
		distributed_job_actor.cpp \
		node_actor.cpp
JOB_CPP = $(patsubst %, $(ACTORS_SRC)/job_actor/%, $(JOB_CPP_FILES))

GRU_CPP_FILES = \
		gru_actor.cpp
GRU_CPP = $(patsubst %, $(ACTORS_SRC)/gru_actor/%, $(GRU_CPP_FILES))

HRU_CPP_FILES = \
		hru_utils.cpp \
		hru_actor.cpp \
		hru_batch_actor.cpp
HRU_CPP = $(patsubst %, $(ACTORS_SRC)/hru_actor/%, $(HRU_CPP_FILES))

# Stich together
COMM_ALL = $(NRUTIL) $(NRPROC) $(HOOKUP) $(DATAMS) $(UTILMS) $(INTERFACE)

SUMMA_ALL = $(NETCDF) $(PRELIM) $(MODRUN) $(SOLVER) $(DRIVER) $(SYS_INIT) $(FILE_ACCESS) $(JOB) $(HRU) $(GRU)

CPP_ALL = $(ACTORS_SRC)/main.cpp $(ACTORS_CPP) $(SYS_INIT_CPP) $(FILE_ACCESS_CPP) $(JOB_CPP) $(GRU_CPP) $(HRU_CPP)

all: lib cpp

lib: noah comm summa_lib link f_clean

noah: $(NRUTIL) $(NOAHMP) 
	@echo "\n\nCOMPILING NOAH\n"
	$(FC) $(FLAGS_NOAH) -c $^ $(F_INCLUDES) $(F_LIBRARIES)

comm: $(COMM_ALL)
	@echo "\nCOMPILING COMM_ALL\n"
	$(FC) $(FLAGS_ALL) -c $^ $(F_INCLUDES) $(F_LIBRARIES)

summa_lib: $(SUMMA_ALL)
	@echo "\nCOMPILING SUMMA_ALL\n"
	$(FC) $(FLAGS_ALL) -c $^ $(F_INCLUDES) $(F_LIBRARIES)

link:
	$(FC) -shared *.o -o libsumma.so

f_clean:
	rm -f *.o *.mod soil_veg_gen_parm__genmod.f90

cpp: actors link_cpp c_clean

actors: $(CPP_ALL)
	$(CXX) $(FLAGS_CXX) -c $^ $(C_INCLUDES) $(C_LIBRARIES)

link_cpp:
	$(CXX) -o summa_actors *.o $(C_LIBRARIES)

c_clean:
	rm -f *.o