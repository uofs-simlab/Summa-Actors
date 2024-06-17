#===============================================================================
# List of Source Files for Summa-Actors
# All Variables are reachable from the parent scope
#===============================================================================

# Actors interface modules
set(INTERFACE
    ${ACTORS_DIR}/global/cppwrap_auxiliary.f90
    ${ACTORS_DIR}/global/cppwrap_datatypes.f90
    ${ACTORS_DIR}/global/cppwrap_metadata.f90
    ${ACTORS_DIR}/global/c_interface_module.f90
    CACHE INTERNAL "INTERFACE")
set(SYS_INIT_INTERFACE
    ${SYS_INIT_DIR}/batch_distributer_actor.f90
    CACHE INTERNAL "SYS_INIT_INTERFACE")
set(FILE_ACCESS_INTERFACE
    ${FILE_ACCESS_DIR}/summa_init_struc.f90
    ${FILE_ACCESS_DIR}/forcing_file_info.f90
    ${FILE_ACCESS_DIR}/file_access_actor.f90
    ${FILE_ACCESS_DIR}/output_structure.f90
    ${FILE_ACCESS_DIR}/fileAccess_writeOutput.f90
    CACHE INTERNAL "FILE_ACCESS_INTERFACE")
set(JOB_INTERFACE
    ${JOB_ACTOR_DIR}/gru_struc.f90
    CACHE INTERNAL "JOB_INTERFACE")
set(GRU_INTERFACE
    ${GRU_ACTOR_DIR}/hru_init.f90
    ${GRU_ACTOR_DIR}/hru_read.f90
    ${GRU_ACTOR_DIR}/hru_modelRun.f90
    ${GRU_ACTOR_DIR}/hru_writeOutput.f90
    ${GRU_ACTOR_DIR}/gru_interface.f90
    CACHE INTERNAL "GRU_INTERFACE")

# Actors actual actor modules
set(ACTORS_GLOBAL
    ${ACTORS_DIR}/global/auxiliary.cpp
    ${ACTORS_DIR}/global/fileManager.cpp
    ${ACTORS_DIR}/global/settings_functions.cpp
    ${ACTORS_DIR}/global/timing_info.cpp
    ${ACTORS_DIR}/global/logger.cpp
    CACHE INTERNAL "ACTORS_GLOBAL")
set(SYS_INIT
    ${SYS_INIT_DIR}/batch.cpp
    ${SYS_INIT_DIR}/batch_container.cpp
    # ${SYS_INIT_DIR}/client.cpp
    # ${SYS_INIT_DIR}/client_container.cpp
    ${SYS_INIT_DIR}/summa_global_data.cpp
    ${SYS_INIT_DIR}/summa_actor.cpp
    # ${SYS_INIT_DIR}/summa_backup_server.cpp
    # ${SYS_INIT_DIR}/summa_client.cpp
    # ${SYS_INIT_DIR}/summa_server.cpp
    CACHE INTERNAL "SYS_INIT")
set(FILE_ACCESS_ACTOR
    ${FILE_ACCESS_DIR}/summa_init_struc.cpp
    ${FILE_ACCESS_DIR}/file_access_actor.cpp
    ${FILE_ACCESS_DIR}/forcing_file_info.cpp
    ${FILE_ACCESS_DIR}/output_container.cpp
    CACHE INTERNAL "FILE_ACCESS_ACTOR")
set(JOB_ACTOR
    ${JOB_ACTOR_DIR}/gru_struc.cpp
    ${JOB_ACTOR_DIR}/job_actor.cpp
    # ${JOB_ACTOR_DIR}/distributed_job_actor.cpp
    # ${JOB_ACTOR_DIR}/node_actor.cpp
    CACHE INTERNAL "JOB_ACTOR")
set(GRU_ACTOR
    ${GRU_ACTOR_DIR}/gru_actor.cpp
    ${GRU_ACTOR_DIR}/gru_batch_actor.cpp
    CACHE INTERNAL "GRU_ACTOR")
set(HRU_ACTOR
    ${HRU_ACTOR_DIR}/hru_utils.cpp
    ${HRU_ACTOR_DIR}/hru_actor.cpp
    CACHE INTERNAL "HRU_ACTOR")