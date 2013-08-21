#!/bin/bash

set -x

CFLAGS="-ansi -std=c99 -pedantic -Wall -O3 -fomit-frame-pointer -funroll-loops"
LDFLAGS= 

echo compiling ....
gcc $CFLAGS -c solver.c -o solver.o

echo linking ....
gcc $LDFLAGS solver.o -o solver


