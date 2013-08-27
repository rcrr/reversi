#!/bin/bash

# Script to launch Edax as an engine for Quarry
#
# usage : gtp.sh

# change directory to from where Edax must be started
cd $(dirname $0)/4.3/bin

# launch edax 
./lEdax -gtp
