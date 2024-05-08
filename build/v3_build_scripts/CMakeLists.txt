cmake_minimum_required(VERSION 3.10 FATAL_ERROR)
project(summa_actors LANGUAGES CXX Fortran)
enable_language(C)
SET (CMAKE_Fortran_COMPILER  gfortran)
include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

get_filename_component(PARENT_DIR "${CMAKE_CURRENT_SOURCE_DIR}" DIRECTORY)
set(EXEC_DIR ${PARENT_DIR}/../bin) # set the output directory for executables
SET(F_MASTER ${PARENT_DIR}/summa)
# Add options for build type
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXEC_DIR})
set(CMAKE_CONFIGURATION_TYPES BE BE_Debug BE_Cluster BE_Cluster_Debug)

# Set Compiler Options
if(CMAKE_BUILD_TYPE MATCHES Debug)
    message("\nSetting Debug Options\n")
    add_compile_definitions(DEBUG)
    set(FLAGS_NOAH -g -O0 -fbacktrace -fbounds-check -ffree-form 
        -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors 
        ${FLAGS_OPT})
    set(FLAGS_ALL  -g -O0 -fbacktrace -fbounds-check -ffree-line-length-none 
        -fmax-errors=0 -fPIC -Wfatal-errors -cpp ${FLAGS_OPT})
    set(FLAGS_CXX  -g -O0 -fbounds-check -Wfatal-errors -std=c++17 ${FLAGS_OPT})
else()
    message("\nSetting Release Options")
    set(FLAGS_NOAH -O3 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors ${FLAGS_OPT})
    set(FLAGS_ALL  -O3 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors -cpp ${FLAGS_OPT})
    set(FLAGS_CXX  -O3 -Wfatal-errors -std=c++17 ${FLAGS_OPT})
endif()

find_package(CAF COMPONENTS core io REQUIRED)
set(CAF_INCLUDES ${CAF_INCLUDE_DIRS})
set(CAF_LIBRARIES CAF::core CAF::io)
set(EXEC_NAME summa_be)

if (CMAKE_BUILD_TYPE MATCHES Cluster)    
    find_package(OpenBLAS REQUIRED)
    # Set include directories
    set(INCLUDES $ENV{EBROOTNETCDFMINFORTRAN}/include ${netCDF_INCLUDES} ${OpenBLAS_INCLUDES})
    set(LIBRARIES SUMMA_NOAHMP  ${OpenBLAS_LIBRARIES} -lnetcdff)

    set(INC_ACTORS 
        ${CAF_INCLUDES} 
        ${PARENT_DIR}/build/includes/global 
        ${PARENT_DIR}/build/includes/summa_actor 
        ${PARENT_DIR}/build/includes/job_actor 
        ${PARENT_DIR}/build/includes/file_access_actor 
        ${PARENT_DIR}/build/includes/gru_actor
        ${PARENT_DIR}/build/includes/hru_actor)
    set(LIB_ACTORS 
        ${CAF_LIBRARIES}
        -lnetcdf 
        -lcaf_core 
        -lcaf_io)

else()
    # Set include and library for Fortran portion of the code
    set(INCLUDES "/usr/include" "/usr/local/include")
    set(LIBRARIES -lnetcdff -lopenblas SUMMA_NOAHMP)

    # Set include and library for C++ portion of the code
    set(INC_ACTORS CAF::core CAF::io ${INCLUDES}
        "${PARENT_DIR}/includes/global"
        "${PARENT_DIR}/includes/summa_actor"
        "${PARENT_DIR}/includes/job_actor"
        "${PARENT_DIR}/includes/file_access_actor"
        "${PARENT_DIR}/includes/gru_actor"
        "${PARENT_DIR}/includes/hru_actor")
    link_directories("/usr/local/lib")
    set(LIB_ACTORS CAF::core CAF::io -lopenblas -lnetcdff -lnetcdf)
endif()

# Define directories that contains source code
set(DRIVER_DIR ${F_MASTER}/build/source/driver)
set(DSHARE_DIR ${F_MASTER}/build/source/dshare)
set(ENGINE_DIR ${F_MASTER}/build/source/engine)
set(HOOKUP_DIR ${F_MASTER}/build/source/hookup)
set(NETCDF_DIR ${F_MASTER}/build/source/netcdf)
set(NOAHMP_DIR ${F_MASTER}/build/source/noah-mp)

# Define Actors specific directories
set(ACTORS_DIR      ${PARENT_DIR}/source)
set(SYS_INIT_DIR    ${ACTORS_DIR}/system_initialization)
set(FILE_ACCESS_DIR ${ACTORS_DIR}/file_access_actor)
set(JOB_ACTOR_DIR   ${ACTORS_DIR}/job_actor)
set(GRU_ACTOR_DIR   ${ACTORS_DIR}/gru_actor)
set(HRU_ACTOR_DIR   ${ACTORS_DIR}/hru_actor)

# NOAHMP modules
set(NOAHMP
    ${NOAHMP_DIR}/module_model_constants.F
    ${NOAHMP_DIR}/module_sf_noahutl.F
    ${NOAHMP_DIR}/module_sf_noahlsm.F
    ${NOAHMP_DIR}/module_sf_noahmplsm.F)

# Free versions of numerical recipes utilities for NOAH-MP modules
set(NRUTIL
    ${ENGINE_DIR}/f2008funcs.f90
    ${ENGINE_DIR}/nr_utility.f90
    ${ENGINE_DIR}/nrtype.f90)

# Free versions of numerical recipes procedures for SUMMA modules
set(NRPROC
    ${ENGINE_DIR}/expIntegral.f90
    ${ENGINE_DIR}/spline_int.f90)

# Hook-up modules
set(HOOKUP
    ${HOOKUP_DIR}/ascii_util.f90
    ${HOOKUP_DIR}/summaFileManager.f90)

# Data modules
set(DATAMS
    ${DSHARE_DIR}/data_types.f90
    ${ACTORS_DIR}/global/actor_data_types.f90
    ${DSHARE_DIR}/flxMapping.f90
    ${DSHARE_DIR}/get_ixname.f90
    ${DSHARE_DIR}/globalData.f90
    ${DSHARE_DIR}/multiconst.f90
    ${DSHARE_DIR}/outpt_stat.f90
    ${DSHARE_DIR}/popMetadat.f90
    ${DSHARE_DIR}/var_lookup.f90)

# Utility modules
set(UTILMS
    ${ENGINE_DIR}/matrixOper.f90
    ${ENGINE_DIR}/mDecisions.f90
    ${ENGINE_DIR}/snow_utils.f90
    ${ENGINE_DIR}/soil_utils.f90
    ${ENGINE_DIR}/time_utils.f90
    ${ENGINE_DIR}/updatState.f90)

# NetCDF routines
set(NETCDF
    ${NETCDF_DIR}/def_output.f90
    ${NETCDF_DIR}/netcdf_util.f90
    ${NETCDF_DIR}/read_icond.f90)

# Preliminary modules
set(PRELIM
    ${ENGINE_DIR}/allocspace.f90
    ${ENGINE_DIR}/check_icond.f90
    ${ENGINE_DIR}/checkStruc.f90
    ${ENGINE_DIR}/childStruc.f90
    ${ENGINE_DIR}/conv_funcs.f90
    ${ENGINE_DIR}/convE2Temp.f90
    ${ENGINE_DIR}/ffile_info.f90
    ${ENGINE_DIR}/read_pinit.f90
    ${ENGINE_DIR}/read_attrb.f90
    ${ENGINE_DIR}/paramCheck.f90
    ${ENGINE_DIR}/pOverwrite.f90
    ${ENGINE_DIR}/sunGeomtry.f90
    ${ENGINE_DIR}/read_param.f90)

    # Model run support modules
set(MODRUN
    ${ENGINE_DIR}/canopySnow.f90
    ${ENGINE_DIR}/derivforce.f90
    ${ENGINE_DIR}/getVectorz.f90
    ${ENGINE_DIR}/indexState.f90
    ${ENGINE_DIR}/layerMerge.f90
    ${ENGINE_DIR}/layerDivide.f90
    ${ENGINE_DIR}/qTimeDelay.f90
    ${ENGINE_DIR}/snowAlbedo.f90
    ${ENGINE_DIR}/snwCompact.f90
    ${ENGINE_DIR}/tempAdjust.f90
    ${ENGINE_DIR}/updateVars.f90
    ${ENGINE_DIR}/var_derive.f90
    ${ENGINE_DIR}/volicePack.f90)

# Solver main modules
set(SOLVER
    ${ENGINE_DIR}/bigAquifer.f90
    ${ENGINE_DIR}/computFlux.f90
    ${ENGINE_DIR}/computJacob.f90
    ${ENGINE_DIR}/computResid.f90
    ${ENGINE_DIR}/coupled_em.f90
    ${ENGINE_DIR}/diagn_evar.f90
    ${ENGINE_DIR}/eval8summa.f90
    ${ENGINE_DIR}/groundwatr.f90
    ${ENGINE_DIR}/opSplittin.f90
    ${ENGINE_DIR}/snowLiqFlx.f90
    ${ENGINE_DIR}/soilLiqFlx.f90
    ${ENGINE_DIR}/ssdNrgFlux.f90
    ${ENGINE_DIR}/stomResist.f90
    ${ENGINE_DIR}/summaSolve.f90
    ${ENGINE_DIR}/systemSolv.f90
    ${ENGINE_DIR}/varSubstep.f90
    ${ENGINE_DIR}/vegLiqFlux.f90
    ${ENGINE_DIR}/vegNrgFlux.f90
    ${ENGINE_DIR}/vegPhenlgy.f90
    ${ENGINE_DIR}/vegSWavRad.f90)

set(DRIVER
    ${DRIVER_DIR}/summa_type.f90
    ${DRIVER_DIR}/summa_setup.f90
    ${DRIVER_DIR}/summa_restart.f90
    ${DRIVER_DIR}/summa_alarms.f90
    ${DRIVER_DIR}/summa_globalData.f90)


# Actors interface modules
set(INTERFACE
    ${ACTORS_DIR}/global/cppwrap_auxiliary.f90
    ${ACTORS_DIR}/global/cppwrap_datatypes.f90
    ${ACTORS_DIR}/global/cppwrap_metadata.f90
    ${ACTORS_DIR}/global/c_interface_module.f90)
set(SYS_INIT_INTERFACE
    ${SYS_INIT_DIR}/batch_distributer_actor.f90)
set(FILE_ACCESS_INTERFACE
    ${FILE_ACCESS_DIR}/summa_init_struc.f90
    ${FILE_ACCESS_DIR}/forcing_file_info.f90
    ${FILE_ACCESS_DIR}/file_access_actor.f90
    ${FILE_ACCESS_DIR}/output_structure.f90
    ${FILE_ACCESS_DIR}/fileAccess_writeOutput.f90)
set(JOB_INTERFACE
    ${JOB_ACTOR_DIR}/gru_struc.f90)
set(GRU_INTERFACE
    ${GRU_ACTOR_DIR}/gru_interface.f90)
set(HRU_INTERFACE
    ${HRU_ACTOR_DIR}/hru_init.f90
    ${HRU_ACTOR_DIR}/hru_read.f90
    ${HRU_ACTOR_DIR}/hru_modelRun.f90
    ${HRU_ACTOR_DIR}/hru_writeOutput.f90
    ${HRU_ACTOR_DIR}/hru_interface.f90)

# Actors actual actor modules
set(ACTORS_GLOBAL
    ${ACTORS_DIR}/global/auxiliary.cpp
    ${ACTORS_DIR}/global/fileManager.cpp
    ${ACTORS_DIR}/global/settings_functions.cpp
    ${ACTORS_DIR}/global/timing_info.cpp)
set(SYS_INIT
    ${SYS_INIT_DIR}/batch.cpp
    ${SYS_INIT_DIR}/batch_container.cpp
    ${SYS_INIT_DIR}/client.cpp
    ${SYS_INIT_DIR}/client_container.cpp
    ${SYS_INIT_DIR}/summa_global_data.cpp
    ${SYS_INIT_DIR}/summa_actor.cpp
    ${SYS_INIT_DIR}/summa_backup_server.cpp
    ${SYS_INIT_DIR}/summa_client.cpp
    ${SYS_INIT_DIR}/summa_server.cpp)
set(FILE_ACCESS_ACTOR
    ${FILE_ACCESS_DIR}/summa_init_struc.cpp
    ${FILE_ACCESS_DIR}/file_access_actor.cpp
    ${FILE_ACCESS_DIR}/forcing_file_info.cpp
    ${FILE_ACCESS_DIR}/output_container.cpp)
set(JOB_ACTOR
    ${JOB_ACTOR_DIR}/GRU.cpp
    ${JOB_ACTOR_DIR}/gru_struc.cpp
    ${JOB_ACTOR_DIR}/job_actor.cpp
    ${JOB_ACTOR_DIR}/async_mode.cpp
    ${JOB_ACTOR_DIR}/data_assimilation_mode.cpp
    ${JOB_ACTOR_DIR}/job_utils.cpp
    ${JOB_ACTOR_DIR}/distributed_job_actor.cpp
    ${JOB_ACTOR_DIR}/node_actor.cpp)
set(GRU_ACTOR
    ${GRU_ACTOR_DIR}/gru_actor.cpp)
set(HRU_ACTOR
    ${HRU_ACTOR_DIR}/hru_utils.cpp
    ${HRU_ACTOR_DIR}/hru_actor.cpp
    ${HRU_ACTOR_DIR}/hru_batch_actor.cpp)
    

#=========================================================================================
# COMPILE PART 3: Collect the subroutines into build groups depending on build type
#=========================================================================================
set(COMM_ALL
    ${NRPROC}
    ${HOOKUP}
    ${DATAMS}
    ${UTILMS}
    ${INTERFACE})

set(SUMMA_ALL
    ${NETCDF}
    ${PRELIM}
    ${MODRUN}
    ${SOLVER}
    ${DRIVER})

set(SUMMA_ALL 
    ${SUMMA_ALL}
    ${SYS_INIT_INTERFACE}
    ${FILE_ACCESS_INTERFACE}
    ${JOB_INTERFACE}
    ${GRU_INTERFACE}
    ${HRU_INTERFACE})

set(MAIN_ACTOR ${ACTORS_DIR}/main.cpp)

# Define version number, not working correctly
set(VERSIONFILE     ${DRIVER_DIR}/summaversion.inc)
execute_process(COMMAND "    ${GIT_EXECUTABLE} tag | tail -n 1" OUTPUT_VARIABLE VERSION)
execute_process(COMMAND "date" OUTPUT_VARIABLE BULTTIM)
execute_process(COMMAND "    ${GIT_EXECUTABLE} describe --long --all --always | sed -e's/heads\///'" OUTPUT_VARIABLE GITBRCH)
execute_process(COMMAND "    ${GIT_EXECUTABLE} rev-parse HEAD" OUTPUT_VARIABLE GITHASH)


#=========================================================================================
# COMPILE PART 4: Do the compilation
#=========================================================================================
# update version information, not working correctly
file(WRITE  ${VERSIONFILE} "character(len=64), parameter     :: summaVersion = '${VERSION}'\n")
file(APPEND ${VERSIONFILE} "character(len=64), parameter     :: buildTime = ''\n")
file(APPEND ${VERSIONFILE} "character(len=64), parameter     :: gitBranch = '${GITBRCH}'\n")
file(APPEND ${VERSIONFILE} "character(len=64), parameter     :: gitHash = '${GITHASH}'")

# Build SUMMA_NOAHMP Object
add_library(SUMMA_NOAHMP OBJECT ${NOAHMP} ${NRUTIL})
target_compile_options(SUMMA_NOAHMP PRIVATE ${FLAGS_NOAH})

# Build SUMMA_COMM Object
add_library(SUMMA_COMM OBJECT ${COMM_ALL})
target_compile_options(SUMMA_COMM PRIVATE ${FLAGS_ALL})
target_include_directories(SUMMA_COMM PRIVATE ${INCLUDES})
target_link_libraries(SUMMA_COMM PUBLIC SUMMA_NOAHMP ${FLAGS_ALL}) # added flags to the link step

add_library(summaactors SHARED ${SUMMA_ALL})
target_compile_options(summaactors PRIVATE ${FLAGS_ALL})
target_include_directories(summaactors PUBLIC ${INCLUDES})
target_link_libraries(summaactors PUBLIC ${LIBRARIES} SUMMA_NOAHMP SUMMA_COMM)
add_executable(${EXEC_NAME} 
               ${MAIN_ACTOR}
               ${ACTORS_GLOBAL}
               ${FILE_ACCESS_ACTOR}
               ${JOB_ACTOR}
               ${GRU_ACTOR}
               ${HRU_ACTOR}
               ${SYS_INIT}
               ${SUMMA_CLIENT}
               ${SUMMA_SERVER})
set_property(TARGET ${EXEC_NAME} PROPERTY LINKER_LANGUAGE Fortran)
target_compile_options(${EXEC_NAME} PUBLIC ${FLAGS_CXX})
target_include_directories(${EXEC_NAME} PUBLIC ${INC_ACTORS})
target_link_libraries( ${EXEC_NAME} ${LIB_ACTORS} summaactors)