
CC=g++


INCLUDES = -I/usr/include -I/usr/local/include
LIBRARIES = -L/usr/lib -L/usr/local/lib -lcaf_core -lcaf_io
FLAGS = -std=c++17


all: compile link

compile:
	$(CC) $(FLAGS) -c test.cpp $(INCLUDES) 

link:
	$(CC) $(FLAGS) -o main *.o $(LIBRARIES)