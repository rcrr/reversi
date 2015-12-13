#!/bin/bash
#
# perf_test.sh
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
#
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
# @copyright 2015 Roberto Corradini. All rights reserved.
#
# License
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
# or visit the site <http://www.gnu.org/licenses/>.



#
# Runs: time ./perf_test.sh -j 4 --from 1 --to 30
#

TIME_CMD="/usr/bin/time -f 'Running %C, elapsed real time: %e'"
PROGRAM="./build/bin/endgame_solver"
ARG_F="-f db/gpdb-ffo.txt"
ARG_S="-s es"
ARGS="--pv-full-rec --pv-no-print"

MIN_FFO_INDEX=1
MAX_FFO_INDEX=79

launch_when_not_busy()
{
    while [ $(jobs -r | wc -l) -ge $CJOBS ]
    do
        # at least $MAX_JOBS are still running.
        sleep 1
    done
    eval "$@" &
}

is_an_integer()
{
    re='^[0-9]+$'
    if [[ $1 =~ $re ]] ; then
        return 0
    else
        return 1
    fi
}

#
# Concurrent jobs
#
CJOBS=1 # default value

#
# First FFO game index to solve
#
FROM=0 # default value

#
# Last FFO game index to solve
#
TO=0 # default value

# Use > 1 to consume two arguments per pass in the loop (e.g. each argument has a corresponding value to go with it).
# Use > 0 to consume one or more arguments per pass in the loop (e.g. some arguments don't have a corresponding value
# to go with it such as in the --default example).
while [[ $# > 1 ]]
do
    key="$1"

    case $key in
        -j|--concurrent-jobs)
            CJOBS="$2"
            shift # past argument
            ;;
        -f|--from)
            FROM="$2"
            shift # past argument
            ;;
        -t|--to)
            TO="$2"
            shift # past argument
            ;;
        *)
            # unknown option
            OPT="$1"
            echo "$0: option $1 is unknown."
            exit 1
            ;;
    esac
    shift # past argument or value
done
if [[ -n $1 ]]; then
    echo "$0: Command line options are not complete, or have errors."
    exit 1
fi

is_an_integer ${CJOBS};
if [ $? -eq 1 ]; then
    echo "$0: option -j must have an integer value."
    exit 1
fi
if [ $CJOBS -lt 1 ]; then
    echo "$0: option -j must be greater than zero."
    exit 1
fi

is_an_integer ${FROM};
if [ $? -eq 1 ]; then
    echo "$0: option -f must have an integer value."
    exit 1
fi
is_an_integer ${TO};
if [ $? -eq 1 ]; then
    echo "$0: option -t must have an integer value."
    exit 1
fi
if [ $FROM -gt $TO ]; then
    echo "$0: option -f cannot be greater than option -t."
    exit 1
fi
if [ $FROM -lt $MIN_FFO_INDEX ]; then
    echo "$0: option -f out of range. Range is [$MIN_FFO_INDEX..$MAX_FFO_INDEX]."
    exit 1
fi
if [ $TO -gt $MAX_FFO_INDEX ]; then
    echo "$0: option -t out of range. Range is [$MIN_FFO_INDEX..$MAX_FFO_INDEX]."
    exit 1
fi

echo Solving FFO entries from index $FROM to index $TO with $CJOBS concurrents jobs.

COUNTER=$FROM
while [ $COUNTER -le $TO ]; do
    COUNTER_AS_STRING="$COUNTER"
    COUNTER_AS_STRING_SIZE=${#COUNTER_AS_STRING}
    if [ $COUNTER_AS_STRING_SIZE -eq 1 ]; then
        COUNTER_AS_STRING="0$COUNTER_AS_STRING"
    fi
    ARG_Q="-q ffo-$COUNTER_AS_STRING"
    ARG_D="-d out/es-pve-ffo-$COUNTER_AS_STRING.dat"
    OUT_FILE="out/es-stdout-ffo-$COUNTER_AS_STRING.txt"
    
    launch_when_not_busy $TIME_CMD $PROGRAM $ARG_F $ARG_S $ARG_Q $ARG_D $ARGS > $OUT_FILE
    
    let COUNTER=COUNTER+1
done

wait
