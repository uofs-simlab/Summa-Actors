#! /bin/bash

for dir in */; do
    mkdir -p $dir/config
    mkdir -p $dir/forcing_data
    mkdir -p $dir/output
    mkdir -p $dir/settings
    touch $dir/run_test.sh
done
