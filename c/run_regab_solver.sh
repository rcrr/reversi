#!/bin/bash

for i in {0..31};
do
    ii=$(printf "%02d\n" $i);
    nohup ./build/bin/regab -a solve -c cfg/regab_gve.cfg -e production -b 9 -y 24 -k gve -n 2000000 > ./build/out/regab.${ii}.log 2>&1 &
done
