CC = g++	  # C++

INCLUDES=-I/usr/local/include
LIBRARIES=-L/usr/local/lib
FLAGS = -g -O0 -Wfatal-errors -std=c++17

SOURCE_DIR = /Summa-Actors/build/source/actors
INCLUDE_DIR = /Summa-Actors/build/includes
SETTINGS_FILES = $(SOURCE_DIR)/global/settings_functions.cpp
GLOBAL_INCLUDES = -I$(INCLUDE_DIR)/global



all: compile link clean

compile:
	$(CC) $(FLAGS) -c main.cpp test.cpp $(SETTINGS_FILES) $(GLOBAL_INCLUDES) $(INCLUDES)

link:
	$(CC) $(FLAGS) -o main *.o $(LIBRARIES)

clean:
	rm *.o
