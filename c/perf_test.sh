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

MAX_JOBS=8

TIME_CMD="/usr/bin/time -f 'Running %C, elapsed real time: %e'"
#TIME_CMD="/usr/bin/time -v"

launch_when_not_busy()
{
    while [ $(jobs | wc -l) -ge $MAX_JOBS ]
    do
        # at least $MAX_JOBS are still running.
        sleep 1
    done
    eval "$@" &
}

launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-01 > out/tmp-01.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-02 > out/tmp-02.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-03 > out/tmp-03.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-04 > out/tmp-04.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-05 > out/tmp-05.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-06 > out/tmp-06.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-07 > out/tmp-07.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-08 > out/tmp-08.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-09 > out/tmp-09.txt

launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-10 > out/tmp-10.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-11 > out/tmp-11.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-12 > out/tmp-12.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-13 > out/tmp-13.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-14 > out/tmp-14.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-15 > out/tmp-15.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-16 > out/tmp-16.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-17 > out/tmp-17.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-18 > out/tmp-18.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-19 > out/tmp-19.txt

launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-20 > out/tmp-20.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-21 > out/tmp-21.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-22 > out/tmp-22.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-23 > out/tmp-23.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-24 > out/tmp-24.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-25 > out/tmp-25.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-26 > out/tmp-26.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-27 > out/tmp-27.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-28 > out/tmp-28.txt
launch_when_not_busy  $TIME_CMD ./build/bin/endgame_solver -f db/gpdb-ffo.txt -s es -q ffo-29 > out/tmp-29.txt

wait
