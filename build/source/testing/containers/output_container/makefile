CC = g++	  # C++
INCLUDES= -I$(EBROOTCAF)/include
LIBRARIES= -L$(EBROOTCAF)/lib -lcaf_core -lcaf_io
FLAGS = -g -O0 -Wfatal-errors -std=c++17

SOURCE_DIR = /home/kklenk/Summa-Projects/Summa-Actors/build/source/actors/file_access_actor/cpp_code

CONTAINER_INCLUDES = -I/home/kklenk/Summa-Projects/Summa-Actors/build/includes/file_access_actor

GLOBAL_INCLUDES = -I/home/kklenk/Summa-Projects/Summa-Actors/build/includes/global

Output_Container = $(SOURCE_DIR)/output_container.cpp



all: compile link clean

compile:
	$(CC) $(FLAGS) -c main.cpp test.cpp $(Output_Container) $(CONTAINER_INCLUDES) $(GLOBAL_INCLUDES)
link:
	$(CC) $(FLAGS) -Wl,-rpath='$(EBROOTCAF)/lib' -o main *.o $(LIBRARIES)

clean:
	rm *.o