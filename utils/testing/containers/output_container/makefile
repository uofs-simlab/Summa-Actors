
ROOT_DIR = /u1/kck540/Summa-Projects/Summa-Actors

FC = gfortran
CC = g++	  # C++
INCLUDES=-I/usr/local/actor-framework/debug/include
LIBRARIES=-L/usr/local/actor-framework/debug/lib -lcaf_core -lcaf_io
FLAGS = -g -O0 -Wfatal-errors -std=c++17



SOURCE_DIR = $(ROOT_DIR)/build/source/actors/file_access_actor/cpp_code

CONTAINER_INCLUDES = -I$(ROOT_DIR)/build/includes/file_access_actor
GLOBAL_INCLUDES = -I$(ROOT_DIR)/build/includes/global

Output_Container = $(SOURCE_DIR)/output_container.cpp


all: compile_cpp link_cpp clean_cpp


compile_cpp:
	$(CC) $(FLAGS) -c main.cpp test.cpp $(Output_Container) $(CONTAINER_INCLUDES) $(GLOBAL_INCLUDES)
link_cpp:
	$(CC) $(FLAGS) -Wl,-rpath='/usr/local/actor-framework/debug/lib' -o main *.o $(LIBRARIES)

clean_cpp:
	rm *.o