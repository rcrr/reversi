#!/bin/bash

for i in {0..31};
do
    ii=$(printf "%02d\n" $i);
    nohup ./build/bin/regab -a solve -c cfg/regab.cfg -e production -b 11 -y 23 -k rglm -n 2000000 > ./build/out/regab.${ii}.log 2>&1 &
done
