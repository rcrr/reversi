#!/bin/bash

PROG=./build/test/game_position_db_test
ARGS=

G_SLICE=always-malloc
G_DEBUG=gc-friendly 
valgrind -v --tool=memcheck --leak-check=full --num-callers=40 --log-file=valgrind.log --track-origins=yes --leak-check=full --show-reachable=yes $(which $PROG) $ARGS
