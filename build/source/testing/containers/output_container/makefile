FC = gfortran
CC = g++	  # C++
INCLUDES=-I/usr/local/include 
LIBRARIES=-L/usr/local/lib -L/Summa-Actors/build/source/testing/containers/output_container -lcaf_core -lcaf_io -lcppwrap_datatypes
FLAGS = -g -O0 -Wfatal-errors -std=c++17


Fortran_Data_Types = /Summa-Actors/build/source/actors/global/cppwrap_datatypes.f90
DATA_TYPES = /Summa-Actors/build/source/dshare/data_types.f90
NR_TYPE = /Summa-Actors/build/source/engine/nrtype.f90
VAR_LOOKUP = /Summa-Actors/build/source/dshare/var_lookup.f90

SOURCE_DIR = /Summa-Actors/build/source/actors/file_access_actor/cpp_code

CONTAINER_INCLUDES = -I/Summa-Actors/build/includes/file_access_actor
GLOBAL_INCLUDES = -I/Summa-Actors/build/includes/global

Output_Container = $(SOURCE_DIR)/output_container.cpp


all: compile_fortran link_fortran clean_fortran compile_cpp link_cpp clean_cpp

compile_fortran:
	$(FC) -c $(NR_TYPE) $(VAR_LOOKUP) $(DATA_TYPES) $(Fortran_Data_Types) 

link_fortran:
	$(FC) -shared *.o -o libcppwrap_datatypes.so

clean_fortran:
	rm -f *.o *.mod 


compile_cpp:
	$(CC) $(FLAGS) -c main.cpp test.cpp $(Output_Container) $(CONTAINER_INCLUDES) $(GLOBAL_INCLUDES)
link_cpp:
	$(CC) $(FLAGS) -Wl,-rpath='/usr/local/lib:/Summa-Actors/build/source/testing/containers/output_container' -o main *.o $(LIBRARIES)

clean_cpp:
	rm *.o