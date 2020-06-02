 --
-- 0109_up_action_extract.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2018, 2019 Roberto Corradini. All rights reserved.
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
--
-- Functions for the REGAB extract action.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0109, now(), 'action_extract', 'creates functions used by the REGAB extract action');

--
--
--
CREATE TABLE regab_prng_gp_pattern_class_instance_names (ins_time      TIMESTAMP,
                                                         pattern_id    INTEGER REFERENCES regab_prng_patterns(pattern_id) ON DELETE CASCADE,
                                                         instance_id   INTEGER,
                                                         instance_name TEXT UNIQUE,
                                                         ---
                                                         CONSTRAINT rpgpcin_pk PRIMARY KEY (pattern_id, instance_id)
                                                         );

---
--- Populates the patter table with EDGE, CORNER, XEDGE, R2, R3, R4, DIAG4, DIAG5, DIAG6, DIAG7, DIAG8, 2X5COR, and DIAG3 patterns.
---
INSERT INTO regab_prng_gp_pattern_class_instance_names (ins_time, pattern_id, instance_id, instance_name)
  SELECT * FROM (VALUES
    (now(),  0, 0, 'i_edge_0'),
    (now(),  0, 1, 'i_edge_1'),
    (now(),  0, 2, 'i_edge_2'),
    (now(),  0, 3, 'i_edge_3'),
    --
    (now(),  1, 0, 'i_corner_0'),
    (now(),  1, 1, 'i_corner_1'),
    (now(),  1, 2, 'i_corner_2'),
    (now(),  1, 3, 'i_corner_3'),
    --
    (now(),  2, 0, 'i_xedge_0'),
    (now(),  2, 1, 'i_xedge_1'),
    (now(),  2, 2, 'i_xedge_2'),
    (now(),  2, 3, 'i_xedge_3'),
    --
    (now(),  3, 0, 'i_r2_0'),
    (now(),  3, 1, 'i_r2_1'),
    (now(),  3, 2, 'i_r2_2'),
    (now(),  3, 3, 'i_r2_3'),
    --
    (now(),  4, 0, 'i_r3_0'),
    (now(),  4, 1, 'i_r3_1'),
    (now(),  4, 2, 'i_r3_2'),
    (now(),  4, 3, 'i_r3_3'),
    --
    (now(),  5, 0, 'i_r4_0'),
    (now(),  5, 1, 'i_r4_1'),
    (now(),  5, 2, 'i_r4_2'),
    (now(),  5, 3, 'i_r4_3'),
    --
    (now(),  6, 0, 'i_diag4_0'),
    (now(),  6, 1, 'i_diag4_1'),
    (now(),  6, 2, 'i_diag4_2'),
    (now(),  6, 3, 'i_diag4_3'),
    --
    (now(),  7, 0, 'i_diag5_0'),
    (now(),  7, 1, 'i_diag5_1'),
    (now(),  7, 2, 'i_diag5_2'),
    (now(),  7, 3, 'i_diag5_3'),
    --
    (now(),  8, 0, 'i_diag6_0'),
    (now(),  8, 1, 'i_diag6_1'),
    (now(),  8, 2, 'i_diag6_2'),
    (now(),  8, 3, 'i_diag6_3'),
    --
    (now(),  9, 0, 'i_diag7_0'),
    (now(),  9, 1, 'i_diag7_1'),
    (now(),  9, 2, 'i_diag7_2'),
    (now(),  9, 3, 'i_diag7_3'),
    --
    (now(), 10, 0, 'i_diag8_0'),
    (now(), 10, 1, 'i_diag8_1'),
    --
    (now(), 11, 0, 'i_2x5cor_0'),
    (now(), 11, 1, 'i_2x5cor_1'),
    (now(), 11, 2, 'i_2x5cor_2'),
    (now(), 11, 3, 'i_2x5cor_3'),
    (now(), 11, 4, 'i_2x5cor_4'),
    (now(), 11, 5, 'i_2x5cor_5'),
    (now(), 11, 6, 'i_2x5cor_6'),
    (now(), 11, 7, 'i_2x5cor_7'),
    --
    (now(), 12, 0, 'i_diag3_0'),
    (now(), 12, 1, 'i_diag3_1'),
    (now(), 12, 2, 'i_diag3_2'),
    (now(), 12, 3, 'i_diag3_3')
  ) AS tmp_table(ins_time, pattern_id, instance_id, instance_name);

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_check_batches('{1, 3}');
--
CREATE FUNCTION regab_action_extract_check_batches (IN  batch_id_array_arg INTEGER[],
                                                    ---
                                                    OUT batch_id INTEGER,
                                                    OUT status   CHARACTER(3),
                                                    OUT ngames   INTEGER)
RETURNS SETOF record
AS $$
BEGIN
  RETURN QUERY SELECT ta.seq, ta.status, ta.ngames FROM regab_prng_gp_h AS ta WHERE ta.seq = ANY(batch_id_array_arg) ORDER BY ta.seq ASC;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_check_patterns('{0, 3, 4, 5}');
--
CREATE FUNCTION regab_action_extract_check_patterns (IN  pattern_id_array_arg INTEGER[],
                                                     ---
                                                     OUT pattern_id   SMALLINT,
                                                     OUT pattern_name CHARACTER(6),
                                                     OUT ninstances   SMALLINT,
                                                     OUT nsquares     SMALLINT)
RETURNS SETOF record
AS $$
BEGIN
  RETURN QUERY SELECT ta.pattern_id, ta.pattern_name, ta.ninstances, ta.nsquares FROM regab_prng_patterns AS ta WHERE ta.pattern_id = ANY(pattern_id_array_arg) ORDER BY ta.pattern_id ASC;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_count_positions(22, '{1, 7, 8}', '{"CMR", "CMS"}');
--
CREATE FUNCTION regab_action_extract_count_positions (IN empty_count_arg    INTEGER,
                                                      IN batch_id_array_arg INTEGER[],
                                                      IN status_array_arg   CHARACTER(3)[],
                                                      ---
                                                      OUT batch_id          INTEGER,
                                                      OUT status            CHARACTER(3),
                                                      OUT game_position_cnt BIGINT,
                                                      OUT classified_cnt    BIGINT)
RETURNS SETOF record
AS $$
BEGIN

  RETURN QUERY
  SELECT
    gp.batch_id AS batch_id,
    gp.status AS status,
    count(gp.seq) AS game_position_cnt,
    count(pc.gp_id) AS classified_cnt
  FROM
    regab_prng_gp AS gp
  LEFT JOIN
    regab_prng_gp_pattern_class AS pc
  ON
    gp.seq = pc.gp_id
  WHERE
    gp.empty_count = empty_count_arg AND
    gp.status = ANY (status_array_arg) AND
    gp.batch_id = ANY (batch_id_array_arg)
  GROUP BY
    gp.batch_id, gp.status
  ORDER BY
    gp.batch_id, gp.status ASC;

END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Cursor cb holds a table with these fields:
--  - glm_variable_id         INTEGER           - PRIMARY KEY - the generalized linear mmodel global variable id
--  - pattern_id              INTEGER           - |
--  - principal_index_value   INTEGER           - | UNIQUE - pattern_id identifies the pattern in the regab_prng_patterns table, principal_index_value is what it is
--  - total_cnt               BIGINT            - total count of occurences in the batch set of the pattern, and the mirror, counted for all the pattern instances
--  - relative_frequency      DOUBLE PRECISION  - frequency in the data
--  - theoretical_probability DOUBLE PRECISION  - expected frequency (computed using an huge set)
--
-- Usage example:
-- tst_regab=> BEGIN;
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs(0, 20, '{8}', '{"CMR", "CMS"}', '{6, 12}', 'ca', 'cb');
-- tst_regab=> FETCH ALL FROM ca;
-- tst_regab=> FETCH ALL FROM cb;
-- tst_regab=> END;
--
-- For debugging purposes the temporary tables are accessible within the transaction.
-- If no error occurs, and before sending the END command, data is available with commands like:
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs_tmp_table_a;
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs_tmp_table_b;
--
--
-- 2020-06-02 Adding the glm_variable_id_offset input argument.
--            glm_variable_id numbering start from glm_variable_id_offset instead of 0.
--
CREATE FUNCTION regab_action_extract_count_pattern_freqs (glm_variable_id_offset   INTEGER,
                                                          empty_count_arg          INTEGER,
                                                          batch_id_array_arg       INTEGER[],
                                                          status_array_arg         CHARACTER(3)[],
                                                          pattern_id_array_arg     INTEGER[],
                                                          cursor_a_arg             REFCURSOR,
                                                          cursor_b_arg             REFCURSOR)
RETURNS TABLE (cur_name REFCURSOR, row_cnt BIGINT)
AS $$
DECLARE
  pattern_name           CHARACTER(6);
  ninstances             SMALLINT;
  nsquares               SMALLINT;
  index_value_cnt        INTEGER;
  tmp_table_exists       BOOL;
  nsamples               BIGINT;
  pattern_id_current     INTEGER;
  pattern_id_cardinality INTEGER;
  status_cardinality     INTEGER;
BEGIN

  --RAISE NOTICE '--A-- timeofday: %', timeofday();

  SELECT EXISTS INTO tmp_table_exists (
    SELECT 1
    FROM   information_schema.tables 
    WHERE  table_schema LIKE 'pg_temp%'
    AND    table_name = 'regab_action_extract_count_pattern_freqs_tmp_table_a'
    );
  IF tmp_table_exists THEN
    RAISE EXCEPTION 'regab_action_extract_count_pattern_freqs_tmp_table_a: tmp_table_exists = %', tmp_table_exists;
  END IF;
  
  CREATE TEMPORARY TABLE regab_action_extract_count_pattern_freqs_tmp_table_a (pattern_id              INTEGER,
                                                                               index_value             INTEGER, 
                                                                               mirror_value            INTEGER,
                                                                               principal_index_value   INTEGER,
                                                                               cnt_0                   BIGINT,
                                                                               cnt_1                   BIGINT,
                                                                               cnt_2                   BIGINT,
                                                                               cnt_3                   BIGINT,
                                                                               cnt_4                   BIGINT,
                                                                               cnt_5                   BIGINT,
                                                                               cnt_6                   BIGINT,
                                                                               cnt_7                   BIGINT,
                                                                               total_cnt               BIGINT,
                                                                               relative_frequency      DOUBLE PRECISION,
                                                                               theoretical_probability DOUBLE PRECISION,
                                                                               PRIMARY KEY (pattern_id, index_value)
                                                                               ) ON COMMIT DROP;

  SELECT EXISTS INTO tmp_table_exists (
    SELECT 1
    FROM   information_schema.tables 
    WHERE  table_schema LIKE 'pg_temp%'
    AND    table_name = 'regab_action_extract_count_pattern_freqs_tmp_table_b'
    );
  IF tmp_table_exists THEN
    RAISE EXCEPTION 'regab_action_extract_count_pattern_freqs_tmp_table_b: tmp_table_exists = %', tmp_table_exists;
  END IF;
  
  CREATE TEMPORARY TABLE regab_action_extract_count_pattern_freqs_tmp_table_b (glm_variable_id         BIGINT,
                                                                               pattern_id              INTEGER,
                                                                               principal_index_value   INTEGER,
                                                                               total_cnt               BIGINT,
                                                                               relative_frequency      DOUBLE PRECISION,
                                                                               theoretical_probability DOUBLE PRECISION,
                                                                               PRIMARY KEY (glm_variable_id),
                                                                               UNIQUE (pattern_id, principal_index_value)
                                                                               ) ON COMMIT DROP;

  SELECT array_length(status_array_arg, 1) INTO status_cardinality;
  SELECT array_length(pattern_id_array_arg, 1) INTO pattern_id_cardinality;

  --RAISE NOTICE '--B-- timeofday: %', timeofday();

  IF (status_cardinality IS NOT NULL AND pattern_id_cardinality IS NOT NULL) THEN
    FOREACH pattern_id_current IN ARRAY pattern_id_array_arg LOOP

      --RAISE NOTICE '--C.1-- pattern_id_current=%, timeofday: %', pattern_id_current, timeofday();

      SELECT ta.pattern_name, ta.ninstances, ta.nsquares INTO pattern_name, ninstances, nsquares FROM regab_prng_patterns AS ta WHERE ta.pattern_id = pattern_id_current;
      index_value_cnt := 3^nsquares;

      WITH RECURSIVE index_value_range AS (
        SELECT
          0::INTEGER AS val
        UNION ALL SELECT val + 1::INTEGER AS val
        FROM index_value_range
        WHERE index_value_range.val < index_value_cnt - 1
      )
      INSERT INTO regab_action_extract_count_pattern_freqs_tmp_table_a
        (index_value, pattern_id, mirror_value, principal_index_value, theoretical_probability)
        SELECT ta.val, pattern_id_current, para.mirror_value, para.principal_index_value, papr.index_prob_given_ec
        FROM
          index_value_range AS ta
        LEFT JOIN
          regab_prng_pattern_ranges AS para ON ta.val = para.index_value
        LEFT JOIN
          regab_prng_pattern_probs AS papr ON para.seq = papr.range_id
        WHERE
          para.pattern_id = pattern_id_current AND papr.empty_count = empty_count_arg;

      --RAISE NOTICE '--C.2-- pattern_id_current=%, timeofday: %', pattern_id_current, timeofday();

      FOR i IN 0..(ninstances - 1) LOOP

        EXECUTE format(
        'WITH index_values AS (
          SELECT
            pc.i_%s_%s AS iv
          FROM
            regab_prng_gp AS gp
          RIGHT JOIN
            regab_prng_gp_pattern_class AS pc
          ON
            gp.seq = pc.gp_id
          WHERE
            gp.empty_count = $1 AND
            gp.status = ANY ($2) AND
            gp.batch_id = ANY ($3)
        ), grouped_values AS (
          SELECT iv AS iv, count(1) AS cnt FROM index_values GROUP BY iv ORDER BY iv
        )
        UPDATE regab_action_extract_count_pattern_freqs_tmp_table_a AS ta
          SET (cnt_%s) = (SELECT tb.cnt FROM grouped_values AS tb WHERE tb.iv = ta.index_value) WHERE ta.pattern_id = $4;',
          lower(pattern_name), i, i
        ) USING empty_count_arg, status_array_arg, batch_id_array_arg, pattern_id_current;
      
      END LOOP;

      --RAISE NOTICE '--C.3-- pattern_id_current=%, timeofday: %', pattern_id_current, timeofday();

      UPDATE regab_action_extract_count_pattern_freqs_tmp_table_a AS ta
        SET total_cnt = COALESCE(ta.cnt_0, 0) + COALESCE(ta.cnt_1, 0) + COALESCE(ta.cnt_2, 0) + COALESCE(ta.cnt_3, 0) +
          COALESCE(ta.cnt_4, 0) + COALESCE(ta.cnt_5, 0) + COALESCE(ta.cnt_6, 0) + COALESCE(ta.cnt_7, 0);

      --RAISE NOTICE '--C.4-- pattern_id_current=%, timeofday: %', pattern_id_current, timeofday();

      SELECT sum(ta.total_cnt) INTO nsamples FROM regab_action_extract_count_pattern_freqs_tmp_table_a AS ta WHERE ta.pattern_id = pattern_id_current;

      --RAISE NOTICE '--C.5-- pattern_id_current=%, timeofday: %', pattern_id_current, timeofday();

      UPDATE regab_action_extract_count_pattern_freqs_tmp_table_a AS ta
        SET relative_frequency = ta.total_cnt::DOUBLE PRECISION / nsamples::DOUBLE PRECISION
        WHERE ta.pattern_id = pattern_id_current;

      --RAISE NOTICE '--C.6-- pattern_id_current=%, timeofday: %', pattern_id_current, timeofday();

    END LOOP;
  END IF;

  INSERT INTO regab_action_extract_count_pattern_freqs_tmp_table_b
    (glm_variable_id, pattern_id, principal_index_value, total_cnt, relative_frequency, theoretical_probability)
  SELECT
    ROW_NUMBER () OVER (ORDER BY pattern_id, principal_index_value) - 1 + glm_variable_id_offset AS glm_variable_id,
    pattern_id,
    principal_index_value,
    total_cnt,
    relative_frequency,
    theoretical_probability
  FROM (SELECT
          pattern_id,
          principal_index_value,
          sum(total_cnt) AS total_cnt,
          sum(relative_frequency) AS relative_frequency,
          sum(theoretical_probability) AS theoretical_probability
        FROM
          regab_action_extract_count_pattern_freqs_tmp_table_a
        WHERE
          total_cnt > 0
        GROUP BY
          pattern_id, principal_index_value
       ) AS ta;
  
  OPEN cursor_a_arg FOR
  SELECT
    ta.pattern_id,
    ta.index_value,
    ta.mirror_value,
    ta.principal_index_value,
    ta.cnt_0, ta.cnt_1, ta.cnt_2, ta.cnt_3, ta.cnt_4, ta.cnt_5, ta.cnt_6, ta.cnt_7, ta.total_cnt,
    ta.relative_frequency,
    ta.theoretical_probability
  FROM regab_action_extract_count_pattern_freqs_tmp_table_a AS ta ORDER BY pattern_id, index_value;
  cur_name := cursor_a_arg;
  SELECT count(1) INTO row_cnt FROM regab_action_extract_count_pattern_freqs_tmp_table_a;
  RETURN NEXT;

  OPEN cursor_b_arg FOR
  SELECT
    tb.glm_variable_id,
    tb.pattern_id,
    tb.principal_index_value,
    tb.total_cnt,
    tb.relative_frequency,
    tb.theoretical_probability
  FROM regab_action_extract_count_pattern_freqs_tmp_table_b AS tb ORDER BY glm_variable_id;
  cur_name := cursor_b_arg;
  SELECT count(1) INTO row_cnt FROM regab_action_extract_count_pattern_freqs_tmp_table_b;
  RETURN NEXT;
  
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Creates a cursor over a table (SELECT) of solved and classified game positions.
-- The function receives as imput:
--  - empty_count_arg         empty square count in the board
--  - batch_id_array_arg      list of selected batch ids
--  - status_array_arg        list of selected statuses
--  - pattern_id_array_arg    list of selected pattern ids
--  - cursor_arg              the given name for the returned cursor
--
-- Returns the number of records collected by the query.
-- The cursor is generated having the row number field (row_n), some field belonging to the regab_prng_gp table (gp_id, mover, opponent, game_value)
-- and a list of column named i000, i001, i002, ... , i00n with the index values taken from the regab_prng_gp_pattern_class table and organized
-- column by column for each ordered (pattern, instance) value.
-- Index values are not transformed into the principal_index_value, so do not map directly to the value generated by the regab_action_extract_count_pattern_freqs function.
-- In order to generate the final table the following mapping has to be implemented by the C program:
--   -0- Column i00x belongs to one pattern_id (e.g. DIAG4)           : the pattern are ordered by id, each pattern has a fixed number of instances, so attribution is easy ...
--   -1- pattern_id|index_value ==> pattern_id|principal_index_value  : a function is needed for each pattern ...
--   -2- pattern_id|principal_index_value ==> glm_variable_id         : a reverse lookup table has to be prepared ( enumerate pattern|index_value and populate with the glm_variable_id )
--
-- Usage example:
-- tst_regab=> BEGIN;
-- tst_regab=> SELECT * FROM regab_action_extract_game_pos_prepare_cursor(20, '{1}', '{"CMR", "CMS"}', '{0,3}', 'ca');
-- tst_regab=> FETCH ALL FROM ca;
-- tst_regab=> END;
--
-- Results could be similar to:
--
--  regab_action_extract_game_pos_prepare_cursor 
-- ----------------------------------------------
--                                            2
-- (1 row)
--
--  row_n |  gp_id   |        mover        |      opponent      | game_value | i000 | i001 | i002 | i003 | i004 | i005 | i006 | i007 
-- -------+----------+---------------------+--------------------+------------+------+------+------+------+------+------+------+------
--      0 |       41 | 1160086296674579472 | 434046682006112392 |        -22 | 4509 |   20 | 1971 |  534 | 1845 |  402 | 4239 |  377
--      1 | 68236790 | 1159804822775804944 | 506385749934426248 |        -48 | 4509 |    2 | 6345 |  539 | 1845 |  366 | 6426 |  377
-- (2 rows)
--
--
CREATE FUNCTION regab_action_extract_game_pos_prepare_cursor (empty_count_arg      INTEGER,
                                                              batch_id_array_arg   INTEGER[],
                                                              status_array_arg     CHARACTER(3)[],
                                                              pattern_id_array_arg INTEGER[],
                                                              cursor_arg           REFCURSOR)
RETURNS BIGINT
AS $$
DECLARE
  pattern_name           CHARACTER(6);
  nsquares               SMALLINT;
  pattern_id_current     INTEGER;
  pattern_id_cardinality INTEGER;
  status_cardinality     INTEGER;
  ninstances             SMALLINT;
  i_name                 TEXT;
  i_name_alias           TEXT;
  query_command          TEXT;
  i_abs                  INTEGER;
  row_cnt                BIGINT;
BEGIN

  SELECT array_length(status_array_arg, 1) INTO status_cardinality;
  SELECT array_length(pattern_id_array_arg, 1) INTO pattern_id_cardinality;

  --IF (status_cardinality IS NOT NULL AND pattern_id_cardinality IS NOT NULL AND empty_count_arg >= 0 AND empty_count_arg <= 60) THEN
  IF (status_cardinality IS NOT NULL AND empty_count_arg >= 0 AND empty_count_arg <= 60) THEN

    i_abs := 0;
    query_command := format('SELECT ROW_NUMBER () OVER (ORDER BY gp_id) - 1 AS row_n, ', cursor_arg);
    query_command := query_command || 'gp.seq AS gp_id, gp.mover AS mover, gp.opponent AS opponent, gp.game_value AS game_value';
  
    FOREACH pattern_id_current IN ARRAY pattern_id_array_arg LOOP
      SELECT ta.pattern_name, ta.ninstances, ta.nsquares INTO pattern_name, ninstances, nsquares FROM regab_prng_patterns AS ta WHERE ta.pattern_id = pattern_id_current;
      FOR i IN 0..(ninstances - 1) LOOP
        SELECT ta.instance_name INTO i_name FROM regab_prng_gp_pattern_class_instance_names AS ta WHERE ta.pattern_id = pattern_id_current AND ta.instance_id = i;
        i_name_alias := 'i' || to_char(i_abs, 'fm000');
        query_command := format('%s, pc.%s AS %s', query_command, i_name, i_name_alias);
        i_abs := i_abs + 1;
      END LOOP;
    END LOOP;
    query_command := format('%s FROM regab_prng_gp AS gp RIGHT JOIN regab_prng_gp_pattern_class AS pc ON gp.seq = pc.gp_id WHERE gp.empty_count = %s', query_command, empty_count_arg);
    query_command := format('%s AND gp.status = ANY (%L) AND gp.batch_id = ANY (%L) ORDER BY gp_id;', query_command, status_array_arg, batch_id_array_arg);

    SELECT count(1) INTO row_cnt FROM regab_prng_gp AS gp RIGHT JOIN regab_prng_gp_pattern_class AS pc ON gp.seq = pc.gp_id
      WHERE gp.empty_count = empty_count_arg AND gp.status = ANY (status_array_arg) AND gp.batch_id = ANY (batch_id_array_arg);
    OPEN cursor_arg FOR EXECUTE query_command;
    RETURN row_cnt;
  
  ELSE

    OPEN cursor_arg FOR SELECT NULL AS no_data;
    RETURN 0;
    
  END IF;
  
END;
$$ LANGUAGE plpgsql VOLATILE;

COMMIT;
