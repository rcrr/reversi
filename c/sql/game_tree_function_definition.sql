--
-- game_tree_function_definition.sql
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
-- Load the file by running the command: \i function_definition.sql
--
--
-- This script creates the functions that works on game trees.
--

SET search_path TO reversi;



--
-- Loads everything from gtable game_tree_log_staging into game_tree_log under a freshly created new record in game_tree_log_header.
-- Returns the number of record loaded in game_tree_log and the run_id value inserted in game_tree_log_header.
--
CREATE OR REPLACE FUNCTION gt_load_from_staging (    run_label           CHAR(4),
                                                     engine_id           CHAR(20),
                                                     description         TEXT,
                                                 OUT new_run_id          INTEGER,
                                                 OUT record_loaded_count INTEGER)
AS $$
BEGIN
  INSERT INTO game_tree_log_header (run_label, engine_id, run_date, description)
    VALUES (run_label, engine_id, now(), description) RETURNING run_id INTO new_run_id;
  INSERT INTO game_tree_log (run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player)
    SELECT new_run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player FROM game_tree_log_staging;
  SELECT COUNT(*) INTO STRICT record_loaded_count FROM game_tree_log WHERE run_id = new_run_id;
END;
$$ LANGUAGE plpgsql VOLATILE;
