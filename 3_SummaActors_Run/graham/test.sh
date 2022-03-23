#!/bin/bash
gruMax=517315 # North America, Merit Hydro basins
gruCount=2000 # The number of GRUs you want to compute
jobID=0


for i in {0..258}
do
    gruStart=$(( 1 + gruCount*jobID))
    check=$(( $gruStart + $gruCount ))
    if [ $check -gt $gruMax ]
    then
        echo "HERe"
        gruCount=$(( gruMax-gruStart+1 ))
    fi
    echo "gruStart = $gruStart, gruCount = $gruCount"
    jobID=$(( 1 + jobID ))
done