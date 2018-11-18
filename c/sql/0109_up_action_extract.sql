--
-- 0109_up_action_extract.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2018 Roberto Corradini. All rights reserved.
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
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs(20, '{8}', '{"CMR", "CMS"}', '{6, 12}', 'ca', 'cb');
-- tst_regab=> FETCH ALL FROM ca;
-- tst_regab=> FETCH ALL FROM cb;
-- tst_regab=> END;
--
-- For debugging purposes the temporary tables are accessible within the transaction.
-- If no error occurs, and before sending the END command, data is available with commands like:
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs_tmp_table_a;
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs_tmp_table_b;
--
CREATE FUNCTION regab_action_extract_count_pattern_freqs (empty_count_arg          INTEGER,
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
    ROW_NUMBER () OVER (ORDER BY pattern_id, principal_index_value) - 1 AS glm_variable_id,
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
--
--
CREATE FUNCTION regab_action_extract_create_tmp_tables ()
RETURNS VOID
AS $$
BEGIN

  --
  -- TODO:
  -- The table shall be reduced to: global_variable_index, patter_id, principal_index_value, toatal_cnt.
  --
  -- In function regab_action_extract_count_pattern_freqs first:
  -- GROUP BY pattern_id, principal_index_value and SUM total_cnt ...
  -- then INSERT into this table before returning the results ...
  --
  -- Finally SORT and assign the global_variable_index.
  -- UPDATE ....
  -- ,rank() over (order by pattern_id, principal_index_value asc) as global_variable_index
  --
  -- The INDEX regab_action_extract_tmp_freqs_pid_piv is not needed.
  --
  CREATE TEMPORARY TABLE regab_action_extract_tmp_freqs (pattern_id              INTEGER,
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
                                                         ---
                                                         CONSTRAINT rae_tmp_freqs_pk PRIMARY KEY (pattern_id, index_value)
                                                         );

END;
$$ LANGUAGE plpgsql VOLATILE;

--
--
--
CREATE FUNCTION regab_action_extract_drop_tmp_tables ()
RETURNS VOID
AS $$
BEGIN
  DROP TABLE regab_action_extract_tmp_freqs;
END;
$$ LANGUAGE plpgsql VOLATILE;

COMMIT;
