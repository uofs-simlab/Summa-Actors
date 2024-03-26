#! /bin/bash



N=16
for i in $(seq 1 $N)
do
  ./summa_actors -c /code/settings/Sundials.json --caf.scheduler.max-threads=1 &
done
