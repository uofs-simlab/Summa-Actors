cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

set(PARENT_DIR /Summa-Actors)
set(EXEC_DIR ${PARENT_DIR}/bin)

set(EXEC_NAME summa_actors)

project(summa_actors LANGUAGES CXX Fortran)
enable_language(C)
SET (CMAKE_Fortran_COMPILER  gfortran)
include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

set(ACTORS_DIR ${PARENT_DIR}/build/source/actors)
set(DRIVER_DIR ${PARENT_DIR}/build/source/driver)
set(DSHARE_DIR ${PARENT_DIR}/build/source/dshare)
set(ENGINE_DIR ${PARENT_DIR}/build/source/engine)
set(HOOKUP_DIR ${PARENT_DIR}/build/source/hookup)
set(NETCDF_DIR ${PARENT_DIR}/build/source/netcdf)
set(NOAHMP_DIR ${PARENT_DIR}/build/source/noah-mp)
set(FILE_ACCESS_DIR ${ACTORS_DIR}/file_acces_actor)
set(JOB_ACTOR_DIR ${ACTORS_DIR}/job_actor)
set(HRU_ACTOR_DIR ${ACTORS_DIR}/hru_actor)
set(GRU_ACTOR_DIR ${ACTORS_DIR}/gru_actor)

set(NRUTIL
    ${ENGINE_DIR}/nrtype.f90
    ${ENGINE_DIR}/f2008funcs.f90
    ${ENGINE_DIR}/nr_utility.f90)

set(NRPROC
    ${ENGINE_DIR}/expIntegral.f90
    ${ENGINE_DIR}/spline_int.f90)

SET(HOOKUP
    ${HOOKUP_DIR}/ascii_util.f90
    ${HOOKUP_DIR}/summaActors_FileManager.f90)

SET(DATAMS 
    ${DSHARE_DIR}/multiconst.f90
    ${DSHARE_DIR}/var_lookup.f90
    ${DSHARE_DIR}/data_types.f90
    ${DSHARE_DIR}/globalData.f90
    ${DSHARE_DIR}/flxMapping.f90
    ${DSHARE_DIR}/get_ixname.f90)

SET(DEPENDS_ON_FILEMANAGER
    ${DSHARE_DIR}/popMetadat.f90
    ${DSHARE_DIR}/outpt_stat.f90)

SET(UTILMS
    ${ENGINE_DIR}/time_utils.f90
    ${ENGINE_DIR}/mDecisions.f90
    ${ENGINE_DIR}/snow_utils.f90
    ${ENGINE_DIR}/soil_utils.f90
    ${ENGINE_DIR}/updatState.f90
    ${ENGINE_DIR}/matrixOper.f90)

set(SOLVER
    ${ENGINE_DIR}/vegPhenlgy.f90
    ${ENGINE_DIR}/diagn_evar.f90
    ${ENGINE_DIR}/stomResist.f90
    ${ENGINE_DIR}/groundwatr.f90
    ${ENGINE_DIR}/vegSWavRad.f90
    ${ENGINE_DIR}/vegNrgFlux.f90
    ${ENGINE_DIR}/ssdNrgFlux.f90
    ${ENGINE_DIR}/vegLiqFlux.f90
    ${ENGINE_DIR}/snowLiqFlx.f90
    ${ENGINE_DIR}/soilLiqFlx.f90
    ${ENGINE_DIR}/bigAquifer.f90
    ${ENGINE_DIR}/computFlux.f90
    ${ENGINE_DIR}/computResid.f90
    ${ENGINE_DIR}/computJacob.f90
    ${ENGINE_DIR}/eval8summa.f90
    ${ENGINE_DIR}/summaSolve.f90
    ${ENGINE_DIR}/systemSolv.f90
    ${ENGINE_DIR}/varSubstep.f90
    ${ENGINE_DIR}/opSplittin.f90
    ${ENGINE_DIR}/coupled_em.f90)

set(INTERFACE
    ${ACTORS_DIR}/global/cppwrap_datatypes.f90
    ${ACTORS_DIR}/global/cppwrap_auxiliary.f90
    ${ACTORS_DIR}/global/cppwrap_metadata.f90)

set(FILE_ACCESS_INTERFACE
    ${FILE_ACCESS_DIR}/initOutputStruc.f90
    ${FILE_ACCESS_DIR}/deallocateOutputStruc.f90
    ${FILE_ACCESS_DIR}/cppwrap_fileAccess.f90)

set(JOB_INTERFACE
    ${JOB_ACTOR_DIR}/job_actor.f90)

set(HRU_INTERFACE
    ${HRU_ACTOR_DIR}/cppwrap_hru.f90)

set(GRU_INTERFACE
    ${GRU_ACTOR_DIR}/gru_actor.f90)

set(PRELIM
    ${ENGINE_DIR}/conv_funcs.f90
    ${ENGINE_DIR}/sunGeomtry.f90
    ${ENGINE_DIR}/convE2Temp.f90
    ${ENGINE_DIR}/allocspaceActors.f90
    ${ENGINE_DIR}/alloc_file_access.f90
    ${ENGINE_DIR}/checkStruc.f90
    ${ENGINE_DIR}/childStruc.f90
    ${ENGINE_DIR}/ffile_info.f90
    ${ENGINE_DIR}/read_attribute.f90
    ${ENGINE_DIR}/read_pinit.f90
    ${ENGINE_DIR}/pOverwrite.f90
    ${ENGINE_DIR}/read_paramActors.f90
    ${ENGINE_DIR}/paramCheck.f90
    ${ENGINE_DIR}/check_icondActors.f90)

set(NOAHMP
    ${NOAHMP_DIR}/module_model_constants.F
    ${NOAHMP_DIR}/module_sf_noahutl.F
    ${NOAHMP_DIR}/module_sf_noahlsm.F
    ${NOAHMP_DIR}/module_sf_noahmplsm.F)

set(MODRUN
    ${ENGINE_DIR}/indexState.f90
    ${ENGINE_DIR}/getVectorz.f90
    ${ENGINE_DIR}/updateVars.f90
    ${ENGINE_DIR}/var_derive.f90
    ${ENGINE_DIR}/read_forcingActors.f90
    ${ENGINE_DIR}/access_forcing.f90
    ${ENGINE_DIR}/access_write.f90
    ${ENGINE_DIR}/derivforce.f90
    ${ENGINE_DIR}/snowAlbedo.f90
    ${ENGINE_DIR}/canopySnow.f90
    ${ENGINE_DIR}/tempAdjust.f90
    ${ENGINE_DIR}/snwCompact.f90
    ${ENGINE_DIR}/layerMerge.f90
    ${ENGINE_DIR}/layerDivide.f90
    ${ENGINE_DIR}/volicePack.f90
    ${ENGINE_DIR}/qTimeDelay.f90)

set(NETCDF
    ${NETCDF_DIR}/netcdf_util.f90
    ${NETCDF_DIR}/def_output.f90
    ${NETCDF_DIR}/outputStrucWrite.f90
    ${NETCDF_DIR}/writeOutput.f90
    ${NETCDF_DIR}/read_icondActors.f90)

set(DRIVER
    ${DRIVER_DIR}/summaActors_type.f90
    ${DRIVER_DIR}/summaActors_util.f90
    ${DRIVER_DIR}/summaActors_globalData.f90
    ${DRIVER_DIR}/summaActors_init.f90
    ${DRIVER_DIR}/SummaActors_setup.f90
    ${DRIVER_DIR}/summaActors_restart.f90
    ${DRIVER_DIR}/summaActors_forcing.f90
    ${DRIVER_DIR}/SummaActors_modelRun.f90
    ${DRIVER_DIR}/summaActors_alarms.f90
    ${DRIVER_DIR}/summaActors_wOutputStruc.f90)

set(COMM_ALL
    ${NRUTIL}
    ${NRPROC}
    ${DATAMS}
    ${INTERFACE}
    ${HOOKUP}
    ${DEPENDS_ON_FILEMANAGER}
    ${UTILMS})

set(SUMMA_ALL
    ${NETCDF}
    ${PRELIM}
    ${MODRUN}
    ${SOLVER}
    ${DRIVER}
    ${JOB_INTERFACE}
    ${HRU_INTERFACE}
    ${GRU_INTERFACE})

set(ACTORS_GLOBAL
    ${ACTORS_DIR}/global/global.cpp
    ${ACTORS_DIR}/global/timing_info.cpp)

set(HRU_ACTOR
    ${ACTORS_DIR}/hru_actor/hru_actor.cpp)

set(GRU_ACTOR
    ${ACTORS_DIR}/gru_actor/gru_actor.cpp)

set(FILE_ACCESS_ACTOR
    ${ACTORS_DIR}/file_access_actor/file_access_actor.cpp
    ${ACTORS_DIR}/file_access_actor/forcing_file_info.cpp
    ${ACTORS_DIR}/file_access_actor/output_manager.cpp)

set(JOB_ACTOR
    ${ACTORS_DIR}/job_actor/job_actor.cpp
    ${ACTORS_DIR}/job_actor/GRUinfo.cpp)

set(SUMMA_ACTOR
    ${ACTORS_DIR}/summa_actor/summa_actor.cpp)

set(SUMMA_CLIENT
    ${ACTORS_DIR}/summa_actor/summa_client.cpp)

set(SUMMA_SERVER
    ${ACTORS_DIR}/summa_actor/summa_server.cpp
    ${ACTORS_DIR}/summa_actor/batch_manager.cpp
    ${ACTORS_DIR}/summa_actor/client.cpp)

set(MAIN
    ${ACTORS_DIR}/main.cpp)




add_library(SUMMA_NOAHMP OBJECT
    ${NOAHMP}
    ${NRUTIL})
    target_compile_options(SUMMA_NOAHMP PRIVATE
        -g -O3 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)

add_library(SUMMA_COMM OBJECT
    ${COMM_ALL})
    target_compile_options(SUMMA_COMM PRIVATE
        -g -O3 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    target_include_directories(SUMMA_COMM PRIVATE
        "/usr/include"
        ${netCDF_INCLUDES}
        ${LAPACK_INCLUDES})
    target_link_libraries(SUMMA_COMM PUBLIC
        ${netCDF_LIBRARIES}
        ${LAPACK_LIBRARIES}
        SUMMA_NOAHMP)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXEC_DIR})

add_library(summa SHARED
    ${SUMMA_ALL})
    target_compile_options(summa PRIVATE
        -g -O3 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    target_include_directories(summa PRIVATE
        "/usr/include"
        ${netCDF_INCLUDES}
        ${LAPACK_INCLUDES})
    target_link_libraries(summa PUBLIC
        ${netCDF_LIBRARIES}
        ${LAPACK_LIBRARIES}
        SUMMA_COMM)

set(CMAKE_CXX_FLAGS "-g -O3 -Wfatal-errors -std=c++17")

find_package(CAF REQUIRED)
find_package(netCDF REQUIRED)
find_package(LAPACK REQUIRED)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXEC_DIR})

add_executable(${EXEC_NAME}
    ${ACTORS_GLOBAL}
    ${HRU_ACTOR}
    ${GRU_ACTOR}
    ${FILE_ACCESS_ACTOR}
    ${JOB_ACTOR}
    ${SUMMA_ACTOR}
    ${SUMMA_CLIENT}
    ${SUMMA_SERVER}
    ${MAIN})
    set_property(TARGET ${EXEC_NAME} PROPERTY LINKER_LANGUAGE Fortran)
    target_include_directories(${EXEC_NAME} PUBLIC
        ${CAF_INCLUDES}
        ${netCDF_INCLUDES}
        ${LAPACK_INCLUDES}
        "/Summa-Actors/build/includes/global"
        "/Summa-Actors/build/includes/summa_actor"
        "/Summa-Actors/build/includes/gru_actor"
        "/Summa-Actors/build/includes/job_actor"
        "/Summa-Actors/build/includes/file_access_actor"
        "/Summa-Actors/build/includes/hru_actor")
    target_link_libraries( ${EXEC_NAME}
       ${CAF_LIBRARIES}
       ${netCDF_LIBRARIES}
       ${LAPACK_LIBRARIES}
       -lcaf_core
       -lcaf_io
       summa
       -lsumma)