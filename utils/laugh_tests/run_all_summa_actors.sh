#! /bin/bash

echo "Starting Celia"
cd celia1990
./run_test_summa_actors.sh
python3 verify_celia.py
cd ..
sleep 3

echo "Starting Colbeck"
cd colbeck1976
./run_test_summa_actors.sh
python3 verify_colbeck.py
cd ..
sleep 3

echo "Starting Miller"
cd miller1998
./run_test_summa_actors.sh
python3 verify_miller.py
cd ..
sleep 3

echo "Starting Mizoguchi"
cd mizoguchi1990
./run_test_summa_actors.sh
python3 verify_mizoguchi.py
cd ..
sleep 3

echo "Starting Vanderborght"
cd vanderborght2005
./run_test_summa_actors.sh
python3 verify_vanderborght.py
cd ..
sleep 3
