--
-- load_rab_solver.sql
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
-- This script load the rab_solver_log file into the appropriate table.
--

SET search_path TO reversi;

--TRUNCATE TABLE rab_solver_log;

--DROP INDEX IF EXISTS rab_solver_log_hash;
--DROP INDEX IF EXISTS rab_solver_log_parent_hash;

--\COPY rab_solver_log FROM '../out/rab_solver_log.csv' WITH (FORMAT CSV, DELIMITER ';', HEADER true);

--VACUUM (FULL, ANALYZE) rab_solver_log;

--CREATE INDEX rab_solver_log_hash ON rab_solver_log (hash);
--CREATE INDEX rab_solver_log_parent_hash ON rab_solver_log (parent_hash);

TRUNCATE TABLE game_tree_log_staging;

\COPY game_tree_log_staging FROM '../out/rab_solver_log.csv' WITH (FORMAT CSV, DELIMITER ';', HEADER true);

VACUUM (FULL, ANALYZE) game_tree_log_staging;

DO $$
DECLARE
  new_run_id INTEGER;
BEGIN
  INSERT INTO game_tree_log_header (engine_id, run_date, description) VALUES ('TEST', now(), 'Test run.') RETURNING run_id INTO new_run_id;
  INSERT INTO game_tree_log (run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player)
  SELECT new_run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player FROM game_tree_log_staging;
END $$;

VACUUM (FULL, ANALYZE) game_tree_log;

TRUNCATE TABLE game_tree_log_staging;
