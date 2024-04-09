#!/bin/bash

for i in {0..47};
do
    ii=$(printf "%02d\n" $i);
    nohup nice -n 10 ./build/bin/regab -a solve -c cfg/regab_gve.cfg -e production -b 12 -y 25 -k gve -n 1000000 > ./build/out/regab.${ii}.log 2>&1 &
done
