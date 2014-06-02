--
-- load_game_tree_log_sets.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2014 Roberto Corradini. All rights reserved.
--
--
-- License:
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 3, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
-- or visit the site <http://www.gnu.org/licenses/>.
--
--
-- This script has been tested with PostgreSQL.
-- Start psql by running: psql -U reversi -w -d reversi -h localhost
-- Load the file by running the command: \i load_rab_solver.sql
--
--
-- This script loads game tree sets into the db, game_tree_log_header, and game_tree_log tables.
--

SET search_path TO reversi;



--
-- File ../out/rab_solver_log-ffo-01-simplified-4_n3.csv is obtained by running
-- the commands:
-- ./build/bin/endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s rab -l -n 3
-- mv out/rab_solver_log.csv out/rab_solver_log-ffo-01-simplified-4_n3.csv
--
\! ./gt_load_file.sh ../out/rab_solver_log-ffo-01-simplified-4_n3.csv;
SELECT gt_load_from_staging('C_RAB_SOLVER', 'Test data obtained by the C rab solver on position ffo-01-simplified-4.');



--
-- File ../out/minimax_log-ffo-01-simplified-4.csv is obtained by running
-- the commands:
-- ./build/bin/endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s minimax -l
-- mv out/minimax_log.csv out/minimax_log-ffo-01-simplified-4.csv
--
\! ./gt_load_file.sh ../out/minimax_log-ffo-01-simplified-4.csv;
SELECT gt_load_from_staging('C_MINIMAX_SOLVER', 'Test data obtained by the C minimax solver on position ffo-01-simplified-4.');
