

CC = g++

INCLUDES = -I/usr/include -I/usr/local/include
ACTORS_LIBRARIES = -L/usr/lib -L/usr/local/lib -L/Summa-Actors/bin -lcaf_core -lcaf_io
FLAGS = -g -O0 -Wall -std=c++17


all: compile link

compile:
	$(CC) $(FLAGS) -c main.cpp $(INCLUDES)

link:
	$(CC) $(FLAGS) -o inspector_test *.o $(ACTORS_LIBRARIES)

clean:
	rm *.o 
	rm inspector_test
