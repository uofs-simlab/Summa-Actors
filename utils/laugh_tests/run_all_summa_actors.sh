#! /bin/bash

cd celia1990
./run_test_summa_actors.sh
echo "Starting Celia" > ../verify.txt
python3 verify_celia.py >> ../verify.txt
cd ..
sleep 3

cd colbeck1976
./run_test_summa_actors.sh
echo "Starting Colbeck" >> ../verify.txt
python3 verify_colbeck.py >> ../verify.txt
cd ..
sleep 3

cd miller1998
./run_test_summa_actors.sh
echo "Starting Miller" >> ../verify.txt
python3 verify_miller.py >> ../verify.txt
cd ..
sleep 3

cd mizoguchi1990
./run_test_summa_actors.sh
echo "Starting Mizoguchi" >> ../verify.txt
python3 verify_mizoguchi.py >> ../verify.txt
cd ..
sleep 3

cd vanderborght2005
./run_test_summa_actors.sh
echo "Starting Vanderborght" >> ../verify.txt
python3 verify_vanderborght.py >> ../verify.txt
cd ..
sleep 3
