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
  DROP INDEX IF EXISTS game_tree_log_hash_idx;
  INSERT INTO game_tree_log (run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player, json_doc)
    SELECT new_run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player, json_doc FROM game_tree_log_staging;
  CREATE INDEX game_tree_log_hash_idx ON game_tree_log (hash);
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
CREATE OR REPLACE FUNCTION gt_check(    run_label_in            CHAR(4),
                                        sub_run_id_in           INTEGER,
                                    OUT game_tree_node_count    INTEGER,
                                    OUT distinct_game_positions INTEGER,
                                    OUT distinct_hashes         INTEGER,
                                    OUT distinct_rels           INTEGER,
                                    OUT collision_count         INTEGER,
                                    OUT duplicate_count         INTEGER)
RETURNS RECORD AS $$
DECLARE
  run_id_in               INTEGER;
  duplicate_partial_count INTEGER;
  collision_partial_count INTEGER;
  gt_exists               BOOLEAN;
  node                    RECORD;
BEGIN
  SELECT EXISTS (SELECT 1 FROM game_tree_log_header WHERE run_label = run_label_in) INTO STRICT gt_exists;
  IF gt_exists IS TRUE THEN
    SELECT gtlh.run_id INTO STRICT run_id_in FROM game_tree_log_header AS gtlh  WHERE gtlh.run_label = run_label_in;
  ELSE
    RETURN;
  END IF;

  SELECT EXISTS (SELECT 1 FROM game_tree_log WHERE run_id = run_id_in AND sub_run_id = sub_run_id_in) INTO STRICT gt_exists;
  IF gt_exists IS NOT TRUE THEN
    RETURN;
  END IF;

  SELECT COUNT(*) INTO STRICT game_tree_node_count FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;

  SELECT COUNT(DISTINCT (blacks, whites, player)) INTO distinct_game_positions FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;
  SELECT COUNT(DISTINCT hash)                     INTO distinct_hashes         FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;
  SELECT COUNT(DISTINCT (hash, parent_hash))      INTO distinct_rels           FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;

  collision_count := 0;
  duplicate_count := 0;
  FOR node IN SELECT DISTINCT hash FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in LOOP
    SELECT COUNT(hash) INTO duplicate_partial_count FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in AND node.hash = gtl.hash;
    SELECT COUNT(DISTINCT (blacks, whites, player)) INTO collision_partial_count FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in AND node.hash = gtl.hash;
    collision_count := collision_count + collision_partial_count - 1;
    duplicate_count := duplicate_count + duplicate_partial_count - collision_partial_count;
  END LOOP;
END;
$$ LANGUAGE plpgsql;



--
-- Checks a game tree of type C_RAB_SOLVER stored in the tables game_tree_log_header/game_tree_log for consistency.
--
CREATE OR REPLACE FUNCTION gt_check_rab(run_label_in  CHAR(4),
                                        sub_run_id_in INTEGER)
RETURNS RECORD AS $$
DECLARE
  rec RECORD;
BEGIN
  PERFORM p_assert('C_RAB_SOLVER' = (SELECT engine_id FROM game_tree_log_header WHERE run_label = run_label_in), 'Wrong game tree type.');
  SELECT (gt_check(run_label_in, sub_run_id_in)).* INTO STRICT rec;
  PERFORM p_assert(rec.distinct_game_positions = rec.distinct_hashes, 'Distinct game position count must be equal to distinct hash count.');
  PERFORM p_assert(rec.collision_count = 0, 'Collision count must be zero.');
  PERFORM p_assert(rec.duplicate_count + rec.distinct_game_positions = rec.game_tree_node_count, 'duplicate_count + distinct_game_positions must be equal to game_tree_node_count.');
  RETURN rec;
END
$$ LANGUAGE plpgsql;



--
-- SELECT array_agg(trim(elem::text, '"')::square) FROM json_array_elements('["A1", "B2", "C4", "H9"]'::json) AS elem;
--
-- Function that checks the json_doc ...
--
CREATE OR REPLACE FUNCTION gt_check_random(    run_label_in   CHAR(4),
                                           OUT repeat_count   INTEGER,
                                           OUT position_count INTEGER,
                                           OUT distinct_count INTEGER)
RETURNS RECORD AS $$
DECLARE
  rec                 RECORD;
  gt_exists           BOOLEAN;
  run_id_in           INTEGER;
BEGIN
  SELECT EXISTS (SELECT 1 FROM game_tree_log_header WHERE run_label = run_label_in) INTO STRICT gt_exists;
  IF gt_exists IS FALSE THEN
    RETURN;
  ELSE
    SELECT gtlh.run_id INTO STRICT run_id_in FROM game_tree_log_header AS gtlh  WHERE gtlh.run_label = run_label_in;
  END IF;
  PERFORM p_assert('C_RANDOM_SAMPLER' = (SELECT engine_id FROM game_tree_log_header WHERE run_label = run_label_in), 'Wrong game tree type.');
  SELECT COUNT(DISTINCT sub_run_id) INTO STRICT repeat_count FROM game_tree_log WHERE run_id = run_id_in;
  SELECT COUNT(*) INTO STRICT position_count FROM game_tree_log WHERE run_id = run_id_in;
  --
  SELECT COUNT(DISTINCT (hash, blacks, whites, player)) AS rel_distinct_count,
         COUNT(DISTINCT (blacks, whites, player))       AS gp_distinct_count,
         COUNT(DISTINCT hash)                           AS hash_distinct_count
    INTO STRICT rec
    FROM game_tree_log
    WHERE run_id = 6;
  PERFORM p_assert(rec.hash_distinct_count = rec.gp_distinct_count,  'Hash values and game positions must have the same count.');
  PERFORM p_assert(rec.hash_distinct_count = rec.rel_distinct_count, 'Hash values and game positions must be one-to-one.');
  distinct_count := rec.hash_distinct_count;
END
$$ LANGUAGE plpgsql;

