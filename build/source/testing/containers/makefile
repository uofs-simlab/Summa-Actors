CC = g++	  # C++
INCLUDES=-I/usr/local/include 
LIBRARIES=-L/usr/local/lib -lcaf_core -lcaf_io
FLAGS = -g -O0 -Wfatal-errors -std=c++17

SOURCE_DIR = /Summa-Actors/build/source/actors/summa_actor

CONTAINER_INCLUDES = -I/Summa-Actors/build/includes/summa_actor

BATCH = $(SOURCE_DIR)/batch/batch.cpp
BATCH_CONTAINER = $(SOURCE_DIR)/batch/batch_container.cpp
CLIENT = $(SOURCE_DIR)/client/client.cpp
CLIENT_CONTAINER = $(SOURCE_DIR)/client/client_container.cpp



all: compile link clean

compile:
	$(CC) $(FLAGS) -c main.cpp test.cpp $(BATCH) $(BATCH_CONTAINER) $(CLIENT) $(CLIENT_CONTAINER) $(CONTAINER_INCLUDES) $(INCLUDES)

link:
	$(CC) $(FLAGS) -Wl,-rpath='/usr/local/lib' -o main *.o $(LIBRARIES)

clean:
	rm *.o