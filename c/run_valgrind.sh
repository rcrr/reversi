#!/bin/bash

PROG=./build/test/bin/game_position_db_test
ARGS=

G_SLICE=always-malloc
G_DEBUG=gc-friendly

OPTIONS_PLUS=--gen-suppressions=all

valgrind -v --tool=memcheck \
            --leak-check=full \
            --num-callers=40 \
            --log-file=valgrind.log \
            --track-origins=yes \
            --leak-check=full \
            --show-reachable=yes \
            --gen-suppressions=all \
            --suppressions=minimal.supp \
              $(which $PROG) $ARGS


# how to generate the suppression file ....
# cat ./minimalraw.log | ./parse_valgrind_suppressions.sh > minimal.supp
# valgrind --leak-check=full --show-reachable=yes --error-limit=no --gen-suppressions=all --log-file=minimalraw.log ./minimal
