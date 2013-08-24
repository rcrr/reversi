#!/bin/bash

# Script to launch Edax as an engine for the Cassio protocol
#
# usage : engine.sh <n>
#         with <n> = number of processors for Edax

# change directory to from where Edax must be started
cd $(dirname $0)/4.3/bin

# launch edax 
./mEdax -cassio -n $1
