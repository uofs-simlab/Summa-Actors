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
set(DIR_SUNDIALS "")
foreach(dir IN ITEMS 
    "/globalhome/kck540/HPC/Libraries/sundials/instdir"
    "/home/kklenk/projects/rpp-kshook/CompHydCore/SummaSundials/sundials/sundials/instdir"
    "/usr/local/sundials-6.3.0")
    if(EXISTS ${dir})
        set(DIR_SUNDIALS ${dir})
        break()
    endif()
endforeach()
#############################################################################

# Set default build type to Release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# Add options for build type
set(CMAKE_CONFIGURATION_TYPES BE BE_Debug BE_Cluster BE_Cluster_Debug
                              Kinsol Kinsol_Debug Kinsol_Cluster Kinsol_Cluster_Debug
                              IDA IDA_Debug IDA_Cluster IDA_Cluster_Debug)
message("\nSelected Bulid Type: ${CMAKE_BUILD_TYPE}\n")


# Set Compiler Options
if(CMAKE_BUILD_TYPE MATCHES Debug)
    message("\nSetting Debug Options\n")
    add_definitions(-DDEBUG)
    set(SUMMA_NOAHMP_OPTIONS -g -O0 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(SUMMA_ALL_OPTIONS -g -O0 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(CMAKE_CXX_FLAGS "-g -O0 -Wfatal-errors -std=c++17")
else()
    message("\nSetting Release Options\n")
    set(SUMMA_NOAHMP_OPTIONS -O3 -ffree-form -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(SUMMA_ALL_OPTIONS -O3 -ffree-line-length-none -fmax-errors=0 -fPIC -Wfatal-errors)
    set(CMAKE_CXX_FLAGS "-O3 -Wfatal-errors -std=c++17")
endif()

# Build Correct Summa-Actors Configuration
if(CMAKE_BUILD_TYPE MATCHES BE)
    message("\nBuilding BE\n")
    compile_with_be(${PARENT_DIR})

elseif(CMAKE_BUILD_TYPE MATCHES IDA)
    message("\nBuilding IDA\n")
    compile_with_ida(PARENT_DIR, ${DIR_SUNDIALS})

elseif(CMAKE_BUILD_TYPE MATCHES Kinsol)
    message("\nBuilding Kinsol\n")
    compile_with_kinsol(PARENT_DIR, ${DIR_SUNDIALS})

else()
    message("Valid build types are: ${CMAKE_CONFIGURATION_TYPES}")
    message(FATAL_ERROR "Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()