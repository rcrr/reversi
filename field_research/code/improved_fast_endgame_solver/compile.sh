#!/bin/bash

set -x

CFLAGS="-ansi -std=c99 -pedantic -Wall -O3"
LDFLAGS= 

echo compiling ....
gcc $CFLAGS -c improved_fast_endgame_solver.c -o improved_fast_endgame_solver.o

echo linking ....
gcc $LDFLAGS improved_fast_endgame_solver.o -o improved_fast_endgame_solver


