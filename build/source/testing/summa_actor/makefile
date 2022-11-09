CC = g++	  # C++

INCLUDES=-I/usr/local/include
LIBRARIES=-L/usr/local/lib -lcaf_core -lcaf_io
FLAGS = -g -O0 -Wfatal-errors -std=c++17

SOURCE_DIR = /Summa-Actors/build/source/actors
INCLUDE_DIR = /Summa-Actors/build/includes
SUMMA_ACTOR_INCLUDES = -I$(INCLUDE_DIR)/summa_actor
SETTINGS_FILES = $(SOURCE_DIR)/global/settings_functions.cpp
GLOBAL_INCLUDES = -I$(INCLUDE_DIR)/global

BATCH_MANGER = $(SOURCE_DIR)/summa_actor/batch_manager.cpp
CLIENT_MANAGER = $(SOURCE_DIR)/summa_actor/client.cpp



all: compile link clean

compile:
	g++ $(FLAGS) -c main.cpp test.cpp $(BATCH_MANGER) $(CLIENT_MANAGER) $(SETTINGS_FILES) \
		$(SUMMA_ACTOR_INCLUDES) $(GLOBAL_INCLUDES) $(INCLUDES)

link:
	g++ $(FLAGS) -Wl,-rpath='/usr/local/lib' -o main *.o $(LIBRARIES)

clean:
	rm *.o