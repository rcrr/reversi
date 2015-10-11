--
-- load_pv_sets.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2015 Roberto Corradini. All rights reserved.
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
-- Start psql by running: psql -U es -w -d es -h localhost
-- Load the file by running the command: \i load_pv_sets.sql
--
--
-- This script is work in progres ...
--

SET search_path TO reversi;

SELECT current_database() AS reversi_dbname
\gset
\setenv REVERSI_DBNAME :reversi_dbname
SELECT current_user AS reversi_username
\gset
\setenv REVERSI_USERNAME :reversi_username

--
-- File ../out/pve-ffo-01.dat is obtained by running
-- the command:
-- $ make ...
-- or directly calling:
-- $ ./build/bin/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-01 -d ./build/out/pve-ffo-01.dat
--
\! ./pv_load_file.sh $REVERSI_USERNAME $REVERSI_DBNAME ../build/out/pve-ffo-01.dat;


--
--
--
SELECT COUNT(gp_hash) AS row_count, COUNT(DISTINCT gp_hash) AS gp_hash_dist_count, COUNT(DISTINCT (gp_b, gp_w, gp_p)) AS gp_dist_count FROM principal_variation_staging;
