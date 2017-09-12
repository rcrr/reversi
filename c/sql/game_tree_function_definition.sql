--
-- game_tree_function_definition.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2014, 2015, 2017 Roberto Corradini. All rights reserved.
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
-- Load the file by running the command: \i function_definition.sql
--
--
-- This script creates the functions that works on game trees.
--

SET search_path TO reversi;



--
-- Loads everything from table game_tree_log_staging into game_tree_log under a freshly created new record in game_tree_log_header.
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
  DROP INDEX IF EXISTS game_tree_log_hash_idx;
  DROP INDEX IF EXISTS game_tree_log_001_idx;
  INSERT INTO game_tree_log (run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player, alpha, beta, call_level, empty_count, is_leaf, legal_move_count, legal_move_count_adjusted, legal_move_array)
    SELECT new_run_id, sub_run_id, call_id, hash, parent_hash, blacks, whites, player, alpha, beta, call_level, empty_count, is_leaf, legal_move_count, legal_move_count_adjusted, legal_move_array FROM game_tree_log_staging;
  CREATE INDEX game_tree_log_hash_idx ON game_tree_log (hash);
  CREATE INDEX game_tree_log_001_idx  ON game_tree_log (run_id, sub_run_id, hash);
  SELECT COUNT(*) INTO STRICT record_loaded_count FROM game_tree_log WHERE run_id = new_run_id;
END;
$$ LANGUAGE plpgsql VOLATILE;



--
-- Compares two game tree stored in the tables game_tree_log_header/game_tree_log for equality.
--
CREATE OR REPLACE FUNCTION gt_compare (    run_label_a           CHAR(4),
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
-- Node frequency analysis.
--
CREATE OR REPLACE FUNCTION gt_nfa ( run_label_in  CHAR(4),
                                    sub_run_id_in INTEGER)
RETURNS TABLE (cnt BIGINT,
               frq BIGINT)
AS $$
DECLARE
  run_id_in INTEGER;
  gt_exists BOOLEAN;
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

  RETURN QUERY WITH a AS (
    SELECT hash        AS a_hash,
           COUNT(hash) AS a_cnt
    FROM game_tree_log WHERE run_id = run_id_in AND sub_run_id = sub_run_id_in
    GROUP BY a_hash
    ORDER BY a_cnt
    DESC
  ) SELECT a_cnt AS cnt, COUNT(a_cnt) AS frq FROM a GROUP BY cnt ORDER BY frq ASC;

END
$$ LANGUAGE plpgsql;



--
-- Checks a game tree stored in the tables game_tree_log_header/game_tree_log for consistency.
--
CREATE OR REPLACE FUNCTION gt_check (    run_label_in            CHAR(4),
                                         sub_run_id_in           INTEGER,
                                     OUT game_tree_node_count    INTEGER,
                                     OUT distinct_game_positions INTEGER,
                                     OUT distinct_hashes         INTEGER,
                                     OUT distinct_rels           INTEGER,
                                     OUT collision_count         INTEGER,
                                     OUT duplicate_count         INTEGER,
                                     OUT hash_error_count        INTEGER,
                                     OUT root_count              INTEGER,
                                     OUT leaf_count              INTEGER)
RETURNS RECORD AS $$
DECLARE
  run_id_in               INTEGER;
  duplicate_partial_count INTEGER;
  collision_partial_count INTEGER;
  gt_exists               BOOLEAN;
  node                    RECORD;
  dkk                     INTEGER;
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

  duplicate_count := game_tree_node_count - distinct_game_positions;
  collision_count := distinct_game_positions - distinct_hashes;

  -- Equal game positions must have the same hash.
  SELECT COUNT(DISTINCT (hash, blacks, whites, player)) INTO dkk FROM game_tree_log AS gtl WHERE gtl.run_id = run_id_in AND gtl.sub_run_id = sub_run_id_in;
  hash_error_count := dkk - distinct_game_positions;

  WITH game_tree AS (
    SELECT hash, parent_hash
    FROM game_tree_log
    WHERE run_id = run_id_in AND sub_run_id = sub_run_id_in
    ), lone_parents AS (
    SELECT hash, parent_hash
    FROM game_tree
    WHERE parent_hash NOT IN (SELECT DISTINCT(hash) FROM game_tree)
    ) SELECT COUNT(*) INTO STRICT root_count FROM lone_parents;

  WITH game_tree AS (
    SELECT hash, parent_hash FROM game_tree_log WHERE run_id = run_id_in AND sub_run_id = sub_run_id_in
  ) SELECT COUNT(*) INTO STRICT leaf_count FROM game_tree WHERE hash NOT IN (SELECT DISTINCT(parent_hash) FROM game_tree);

END;
$$ LANGUAGE plpgsql;



--
-- Checks a game tree of type C_RAB_SOLVER stored in the tables game_tree_log_header/game_tree_log for consistency.
--
CREATE OR REPLACE FUNCTION gt_check_rab (run_label_in  CHAR(4),
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
-- Function that checks the random game tree.
--
CREATE OR REPLACE FUNCTION gt_check_random(    run_label_in   CHAR(4),
                                           OUT repeat_count   INTEGER,
                                           OUT position_count INTEGER,
                                           OUT distinct_count INTEGER)
RETURNS RECORD AS $$
DECLARE
  rec                                RECORD;
  gt_exists                          BOOLEAN;
  run_id_in                          INTEGER;
  legal_move_computation_error_count INTEGER;
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
    WHERE run_id = run_id_in;
  PERFORM p_assert(rec.hash_distinct_count = rec.gp_distinct_count,  'Hash values and game positions must have the same count.');
  PERFORM p_assert(rec.hash_distinct_count = rec.rel_distinct_count, 'Hash values and game positions must be one-to-one.');
  distinct_count := rec.hash_distinct_count;
  --
  -- This is very time consuming ...
  --SELECT COUNT(*) INTO STRICT legal_move_computation_error_count FROM game_tree_log
  --  WHERE
  --    run_id = run_id_in
  --    AND (legal_move_array = square_set_to_array(game_position_legal_moves((blacks, whites, player)))) = FALSE;
  --PERFORM p_assert(legal_move_computation_error_count = 0, 'Legal move computation error count must be equal to zero.');
END
$$ LANGUAGE plpgsql;



--
-- Function that computes statistics on mobility on a game tree generated by the RANDOM GAME SAMPLER.
--
CREATE OR REPLACE FUNCTION gt_mobility_statistics_on_random(run_label_in CHAR(4))
RETURNS TABLE (empty_square_count SMALLINT,
               average_mobility   NUMERIC,
               mobility_variance  NUMERIC,
               mobility_sd        NUMERIC)
AS $$
DECLARE
  gt_exists BOOLEAN;
  run_id_in INTEGER;
BEGIN
  SELECT EXISTS (SELECT 1 FROM game_tree_log_header WHERE run_label = run_label_in) INTO STRICT gt_exists;
  IF gt_exists IS FALSE THEN
    RETURN;
  ELSE
    SELECT gtlh.run_id INTO STRICT run_id_in FROM game_tree_log_header AS gtlh WHERE gtlh.run_label = run_label_in;
  END IF;
  PERFORM p_assert('C_RANDOM_SAMPLER' = (SELECT engine_id FROM game_tree_log_header WHERE run_label = run_label_in), 'Wrong game tree type.');

RETURN QUERY WITH empty_mobility_node_count AS (
  SELECT
    facts.empty_count      AS empty_count,
    facts.legal_move_count AS mobility,
    count(*)               AS node_count
  FROM
    game_tree_log AS facts
  WHERE
    run_id = run_id_in AND
    NOT (facts.legal_move_count = 0 AND facts.legal_move_count_adjusted = 1)
  GROUP BY
    empty_count, mobility
  ORDER BY
    empty_count DESC, mobility ASC
), empty_sum AS (
  SELECT
    a.empty_count     AS empty_count,
    sum(a.node_count) AS sum
  FROM
    empty_mobility_node_count AS a
  GROUP BY
    empty_count
), empty_mobility_frequency AS (
  SELECT
    b.empty_count         AS empty_count,
    b.mobility            AS mobility,
    b.node_count          AS node_count,
    b.node_count / c.sum  AS frequency
  FROM
    empty_mobility_node_count   AS b
  INNER JOIN
    empty_sum                   AS c
  ON (b.empty_count = c.empty_count)
), empty_average_mobility AS (
  SELECT
    d.empty_count                 AS empty_count,
    sum(d.frequency * d.mobility) AS average_mobility
  FROM
    empty_mobility_frequency AS d
  GROUP BY
    empty_count
), empty_mobility_variance AS (
  SELECT
    e.empty_count       AS empty_count,
    e.mobility          AS mobility,
    e.frequency         AS frequency,
    f.average_mobility  AS average_mobility
  FROM
    empty_mobility_frequency AS e
  INNER JOIN
    empty_average_mobility AS f
  ON (e.empty_count = f.empty_count)
)
SELECT
  t.empty_count                                                          AS empty_square_count,
  round(max(t.average_mobility), 3)                                      AS average_mobility,
  round(sum((t.average_mobility - t.mobility)^2 * t.frequency), 4)       AS mobility_variance,
  round(sqrt(sum((t.average_mobility - t.mobility)^2 * t.frequency)), 4) AS mobility_sd
FROM
  empty_mobility_variance AS t
GROUP BY
  empty_count
ORDER BY
  empty_count DESC;

END
$$ LANGUAGE plpgsql;
