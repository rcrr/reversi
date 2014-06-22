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
CREATE OR REPLACE FUNCTION gt_load_from_staging(    run_label           CHAR(4),
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



--
-- Compares two game tree stored in the tables game_tree_log_header/game_tree_log for equality.
--
CREATE OR REPLACE FUNCTION gt_compare(    run_label_a           CHAR(4),
                                          run_label_b           CHAR(4),
                                      OUT are_equal             BOOLEAN,
                                      OUT run_log_count_a       INTEGER,
                                      OUT run_log_count_b       INTEGER,
                                      OUT full_outer_join_count INTEGER)
AS $$
DECLARE
  run_id_a INTEGER;
  run_id_b INTEGER;
BEGIN
  are_equal := TRUE;

  SELECT run_id INTO STRICT run_id_a FROM game_tree_log_header WHERE run_label = run_label_a;
  SELECT run_id INTO STRICT run_id_b FROM game_tree_log_header WHERE run_label = run_label_b;
  SELECT COUNT(*) INTO STRICT run_log_count_a FROM game_tree_log WHERE run_id = run_id_a;
  SELECT COUNT(*) INTO STRICT run_log_count_b FROM game_tree_log WHERE run_id = run_id_b;

  IF run_log_count_a <> run_log_count_b THEN
    are_equal := FALSE;
  END IF;

  --
  -- The query prepares two temporary tables gtl_a and gtl_b.
  -- Then performs a full outer join matching for equality all the game tree fields.
  -- The record count is then saved to the variable full_outer_join_count.
  -- This value is equal to the two specific count when the records are all equal one by one.
  --
  WITH gtl_a AS (
    SELECT
      gtl.run_id            AS run_id,
      gtl.sub_run_id        AS sub_run_id,
      gtl.call_id           AS call_id,
      gtl.hash              AS hash,
      gtl.parent_hash       AS parent_hash,
      gtl.blacks            AS blacks,
      gtl.whites            AS whites,
      gtl.player            AS player
    FROM
      game_tree_log AS gtl
    WHERE
      run_id = run_id_a
    ORDER BY
      sub_run_id ASC, call_id ASC
  ), gtl_b AS (
    SELECT
      gtl.run_id            AS run_id,
      gtl.sub_run_id        AS sub_run_id,
      gtl.call_id           AS call_id,
      gtl.hash              AS hash,
      gtl.parent_hash       AS parent_hash,
      gtl.blacks            AS blacks,
      gtl.whites            AS whites,
      gtl.player            AS player
    FROM
      game_tree_log AS gtl
    WHERE
      run_id = run_id_b
    ORDER BY
      sub_run_id ASC, call_id ASC
  )
  SELECT
    COUNT(*)
  INTO STRICT full_outer_join_count
  FROM
    gtl_a AS ta FULL OUTER JOIN gtl_b AS tb
  ON (ta.sub_run_id  = tb.sub_run_id  AND
      ta.call_id     = tb.call_id     AND
      ta.hash        = tb.hash        AND
      ta.parent_hash = tb.parent_hash AND
      ta.blacks      = tb.blacks      AND
      ta.whites      = tb.whites      AND
      ta.player      = tb.player);

  IF run_log_count_a <> full_outer_join_count THEN
    are_equal := FALSE;
  END IF;

END;
$$ LANGUAGE plpgsql VOLATILE;



--
-- Checks a game tree stored in the tables game_tree_log_header/game_tree_log for consistency.
--
CREATE OR REPLACE FUNCTION gt_check(run_label_in CHAR(4), sub_run_id_in INTEGER) RETURNS RECORD AS $$
DECLARE
  ret                     RECORD;
  run_id_in               INTEGER;
  game_tree_node_count    INTEGER;
  node                    RECORD;
  rel                     RECORD;
  collisions              INTEGER;
  duplicates              INTEGER;
  duplicate_count         INTEGER;
  collision_count         INTEGER;
  distinct_game_positions INTEGER;
  distinct_hashes         INTEGER;
  distinct_rels           INTEGER;
BEGIN

  RAISE NOTICE 'Checking records in table game_tree_log, run_label=%, sub_run_id=% ...', run_label_in, sub_run_id_in;

  SELECT gtlh.run_id INTO STRICT run_id_in FROM game_tree_log_header AS gtlh  WHERE gtlh.run_label = run_label_in;
  SELECT COUNT(*) INTO STRICT game_tree_node_count FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;

  SELECT COUNT(DISTINCT (blacks, whites, player)) INTO distinct_game_positions FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;
  SELECT COUNT(DISTINCT hash)                     INTO distinct_hashes         FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;
  SELECT COUNT(DISTINCT (hash, parent_hash))      INTO distinct_rels           FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;
  collisions := 0;
  duplicates := 0;
  FOR node IN SELECT DISTINCT hash FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in LOOP
    SELECT COUNT(hash) INTO duplicate_count FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in AND node.hash = gtl.hash;
    SELECT COUNT(DISTINCT (blacks, whites, player)) INTO collision_count FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in AND node.hash = gtl.hash;
    collisions := collisions + collision_count - 1;
    duplicates := duplicates + duplicate_count - collision_count;
  END LOOP;
  RAISE NOTICE 'Check completed: row count is %, distinct_game_positions are %, distinct_hashes are %, distinct_rels are %, collisions are %, duplicates are %.',
               game_tree_node_count,
               distinct_game_positions,
               distinct_hashes,
               distinct_rels,
               collisions,
               duplicates;
  
  SELECT game_tree_node_count, distinct_game_positions, distinct_hashes, distinct_rels, collisions, duplicates INTO ret;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;
