CC = g++	  # C++
INCLUDES=-I/usr/local/include 
LIBRARIES=-L/usr/local/lib -lcaf_core -lcaf_io
FLAGS = -g -O3 -Wfatal-errors -std=c++17

GRU_CLASS_INCLUDES = -I/u1/kck540/Projects/hydrology/Summa-Sundials-Actors/Summa-Actors/build/includes/job_actor/
GRU_CLASS_SOURCE =/u1/kck540/Projects/hydrology/Summa-Sundials-Actors/Summa-Actors/build/source/actors/job_actor/GRU.cpp


all: compile link clean

compile:
	$(CC) $(FLAGS) -c main.cpp $(INCLUDES) $(GRU_CLASS_INCLUDES) $(GRU_CLASS_SOURCE)

link:
	$(CC) $(FLAGS) -Wl,-rpath='/usr/local/lib' -o main *.o $(LIBRARIES)

clean:
	rm *.o