#!/bin/bash

if test -f "summa_actors.sif"; then
    echo "Conatiner exists"
else
    sudo apptainer build summa_actors.sif summa_actors.def
fi
