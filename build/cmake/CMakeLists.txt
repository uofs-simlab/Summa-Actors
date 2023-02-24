cmake_minimum_required(VERSION 3.10 FATAL_ERROR)

set(PARENT_DIR ../../)
set(EXEC_DIR ${PARENT_DIR}../bin)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXEC_DIR}) # set the output directory for executables
project(summa_actors LANGUAGES CXX Fortran)
enable_language(C)
SET (CMAKE_Fortran_COMPILER  gfortran)
include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

# SUMMA-Actors can be built in three configurations:
# 1. SUMMA-Actors with SUNDIALS (Kinsol) solver
# 2. SUMMA-Actors with SUNDIALS (IDA) solver
# 3. SUMMA-Actors without SUNDIALS solver, using the original SUMMA home coded solver
# --------------------------------------------------------------------------------------------
# Each configuration of Summa has a corresponding CMakeLists.txt file in the build directory
include(kinsol.cmake)
include(be.cmake)
include(ida.cmake)
# --------------------------------------------------------------------------------------------
# When compiling Summa-Actors each configuration can be compiled in a Cluster or Debug mode,
# which is set by the CMAKE_BUILD_TYPE variable. The default is Release, non-cluster mode.

######### SET THE PATHS TO THE SUNDIALS LIBRARIES AND INCLUDE FILES #########
#############################################################################
# set(DIR_SUNDIALS "/globalhome/kck540/HPC/Libraries/sundials/instdir")
set(DIR_SUNDIALS "/home/kklenk/projects/rpp-kshook/CompHydCore/SummaSundials/sundials/sundials/instdir")
#set(DIR_SUNDIALS "/usr/local/sundials")
#############################################################################

# Set default build type to Release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Add options for build type
set(CMAKE_CONFIGURATION_TYPES Debug Release Cluster Cluster_Debug Kinsol Kinsol_Debug IDA IDA_DEBUG)

# Set compiler flags
if(CMAKE_BUILD_TYPE MATCHES Debug OR CMAKE_BUILD_TYPE MATCHES Cluster_Debug OR CMAKE_BUILD_TYPE MATCHES Kinsol_Debug)
    message("Debug build.")
    add_definitions(-DDEBUG)
    set(SUMMA_NOAHMP_OPTIONS -g -O0 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(SUMMA_ALL_OPTIONS -g -O0 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(CMAKE_CXX_FLAGS "-g -O0 -Wfatal-errors -std=c++17")
else()
    message("Release build.")
    set(SUMMA_NOAHMP_OPTIONS -O3 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(SUMMA_ALL_OPTIONS -O3 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(CMAKE_CXX_FLAGS "-O3 -Wfatal-errors -std=c++17")
endif()


# Packages that are required by both sundials and non-sundials builds


# Set include directories
if (CMAKE_BUILD_TYPE MATCHES Cluster OR CMAKE_BUILD_TYPE MATCHES Cluster_Debug)

    find_package(CAF REQUIRED)
    find_package(netCDF REQUIRED)
    find_package(LAPACK REQUIRED)
    message("\nBuilding Cluster\n")

    compile_summa_original(PARENT_DIR, ${DIR_SUNDIALS})

elseif(CMAKE_BUILD_TYPE MATCHES IDA OR CMAKE_BUILD_TYPE MATCHES IDA_DEBUG)
    find_package(CAF REQUIRED)
    find_package(netCDF REQUIRED)
    find_package(LAPACK REQUIRED)
    message("\nBuilding IDA\n")

    compile_with_ida(PARENT_DIR, ${DIR_SUNDIALS})


elseif(CMAKE_BUILD_TYPE MATCHES Kinsol OR CMAKE_BUILD_TYPE MATCHES Kinsol_Debug)
    message("\nBuilding Kinsol\n")
    compile_with_kinsol(PARENT_DIR, ${DIR_SUNDIALS})

else()
    find_package(CAF REQUIRED)
    find_package(netCDF REQUIRED)
    find_package(LAPACK REQUIRED)
    if(SUNDIALS)
        # Building Summa with sundials on local machine
        link_directories(${DIR_SUNDIALS}/lib)
        set(CMAKE_BUILD_RPATH "${DIR_SUNDIALS}/lib:/usr/local/lib")
        set(SUMMA_INCLUDES
            "/usr/include"
            "${DIR_SUNDIALS}/include"
            "${DIR_SUNDIALS}/fortran"
            ${netCDF_INCLUDES}
            ${LAPACK_INCLUDES})

        set(SUMMA_LIBS
            -lsundials_fnvecmanyvector_mod 
            -lsundials_fida_mod 
            -lsundials_fnvecserial_mod 
            -lsundials_fsunlinsoldense_mod 
            -lsundials_fsunmatrixdense_mod 
            -lnetcdff
            -lopenblas
            SUMMA_NOAHMP)
        
        set(SUMMA_ACTORS_INCLUDES
            ${CAF_INCLUDES}
            ${netCDF_INCLUDES}
            ${LAPACK_INCLUDES}
            "${DIR_SUNDIALS}/include"
            "${PARENT_DIR}/build/includes/global"
            "${PARENT_DIR}/build/includes/summa_actor"
            "${PARENT_DIR}/build/includes/gru_actor"
            "${PARENT_DIR}/build/includes/job_actor"
            "${PARENT_DIR}/build/includes/file_access_actor"
            "${PARENT_DIR}/build/includes/hru_actor")
        
        set(SUMMA_ACTORS_LIBS   
            -lopenblas
            -lcaf_core
            -lcaf_io
            summa
            -lnetcdff
            -lsundials_fnvecmanyvector_mod 
            -lsundials_fida_mod 
            -lsundials_fnvecserial_mod 
            -lsundials_fsunlinsoldense_mod 
            -lsundials_fsunmatrixdense_mod)
    

    else()
        # Building Summa-Actors on local machine without sundials 
        set(CMAKE_BUILD_RPATH "/usr/local/lib")
        set(SUMMA_INCLUDES
            "/usr/include"
            ${netCDF_INCLUDES}
            ${LAPACK_INCLUDES})
        
        set(SUMMA_LIBS
            ${netCDF_LIBRARIES}
            ${LAPACK_LIBRARIES}
            SUMMA_NOAHMP)
        
        set(SUMMA_ACTORS_INCLUDES
            ${CAF_INCLUDES}
            ${netCDF_INCLUDES}
            ${LAPACK_INCLUDES}
            "${PARENT_DIR}/build/includes/global"
            "${PARENT_DIR}/build/includes/summa_actor"
            "${PARENT_DIR}/build/includes/gru_actor"
            "${PARENT_DIR}/build/includes/job_actor"
            "${PARENT_DIR}/build/includes/file_access_actor"
            "${PARENT_DIR}/build/includes/hru_actor")
        
        set(SUMMA_ACTORS_LIBS   
            ${CAF_LIBRARIES}
            ${netCDF_LIBRARIES}
            ${LAPACK_LIBRARIES}
            -lcaf_core
            -lcaf_io
            summa
            -lnetcdff)
    
    endif()
endif()