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
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs(20, '{3}', '{"CMR", "CMS"}', 12);
--
CREATE FUNCTION regab_action_extract_count_pattern_freqs (IN empty_count_arg    INTEGER,
                                                          IN batch_id_array_arg INTEGER[],
                                                          IN status_array_arg   CHARACTER(3)[],
                                                          IN pattern_id_arg     INTEGER,
                                                          ---
                                                          OUT pattern_id              INTEGER,
                                                          OUT index_value             INTEGER,
                                                          OUT mirror_value            INTEGER,
                                                          OUT principal_index_value   INTEGER,
                                                          OUT cnt_0                   BIGINT,
                                                          OUT cnt_1                   BIGINT,
                                                          OUT cnt_2                   BIGINT,
                                                          OUT cnt_3                   BIGINT,
                                                          OUT cnt_4                   BIGINT,
                                                          OUT cnt_5                   BIGINT,
                                                          OUT cnt_6                   BIGINT,
                                                          OUT cnt_7                   BIGINT,
                                                          OUT total_cnt               BIGINT,
                                                          OUT relative_frequency      DOUBLE PRECISION,
                                                          OUT theoretical_probability DOUBLE PRECISION)
RETURNS SETOF record
AS $$
DECLARE
  pattern_name     CHARACTER(6);
  ninstances       SMALLINT;
  nsquares         SMALLINT;
  index_value_cnt  INTEGER;
  tmp_table_exists BOOL;
  nsamples         BIGINT;
BEGIN

  --RAISE NOTICE 'STEP -00- TRX - CLOCK: % - %', (SELECT transaction_timestamp()), (SELECT clock_timestamp());

  SELECT ta.pattern_name, ta.ninstances, ta.nsquares INTO pattern_name, ninstances, nsquares FROM regab_prng_patterns AS ta WHERE ta.pattern_id = pattern_id_arg;
  index_value_cnt := 3^nsquares;
  
  --RAISE NOTICE 'STEP -01- TRX - CLOCK: % - %', (SELECT transaction_timestamp()), (SELECT clock_timestamp());

  SELECT EXISTS INTO tmp_table_exists (
    SELECT 1
    FROM   information_schema.tables 
    WHERE  table_schema LIKE 'pg_temp%'
    AND    table_name = 'regab_action_extract_count_pattern_freqs_tmp_table_a'
    );
  IF tmp_table_exists THEN
    RAISE EXCEPTION 'tmp_table_exists = %', tmp_table_exists;
  END IF;
  
  CREATE TEMPORARY TABLE regab_action_extract_count_pattern_freqs_tmp_table_a (index_value             INTEGER PRIMARY KEY,
                                                                               pattern_id              INTEGER, 
                                                                               mirror_value            INTEGER,
                                                                               principal_index_value   INTEGER,
                                                                               cnt_0                   BIGINT DEFAULT 0,
                                                                               cnt_1                   BIGINT DEFAULT 0,
                                                                               cnt_2                   BIGINT DEFAULT 0,
                                                                               cnt_3                   BIGINT DEFAULT 0,
                                                                               cnt_4                   BIGINT DEFAULT 0,
                                                                               cnt_5                   BIGINT DEFAULT 0,
                                                                               cnt_6                   BIGINT DEFAULT 0,
                                                                               cnt_7                   BIGINT DEFAULT 0,
                                                                               total_cnt               BIGINT,
                                                                               relative_frequency      DOUBLE PRECISION,
                                                                               theoretical_probability DOUBLE PRECISION
                                                                               ) ON COMMIT DROP;
  
  --RAISE NOTICE 'STEP -02- TRX - CLOCK: % - %', (SELECT transaction_timestamp()), (SELECT clock_timestamp());

  WITH RECURSIVE index_value_range AS (
    SELECT
      0::INTEGER AS val
    UNION ALL SELECT val + 1::INTEGER AS val
    FROM index_value_range
    WHERE index_value_range.val < index_value_cnt - 1
  )
  INSERT INTO regab_action_extract_count_pattern_freqs_tmp_table_a
    (index_value, pattern_id, mirror_value, principal_index_value, theoretical_probability)
    SELECT ta.val, pattern_id_arg, para.mirror_value, para.principal_index_value, papr.index_prob_given_ec
      FROM
        index_value_range AS ta
      LEFT JOIN
        regab_prng_pattern_ranges AS para ON ta.val = para.index_value
      LEFT JOIN
        regab_prng_pattern_probs AS papr ON para.seq = papr.range_id
      WHERE
        para.pattern_id = pattern_id_arg AND papr.empty_count = empty_count_arg;
        
  --RAISE NOTICE 'STEP -03- TRX - CLOCK: % - %', (SELECT transaction_timestamp()), (SELECT clock_timestamp());

  FOR i IN 0..(ninstances - 1) LOOP
    --RAISE NOTICE '  Computing frequencies on pattern instance n. %', i;

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
      SET cnt_%s = tb.cnt FROM grouped_values AS tb WHERE tb.iv = ta.index_value;', lower(pattern_name), i, i
    ) USING empty_count_arg, status_array_arg, batch_id_array_arg;

    --RAISE NOTICE '  STEP -04.%- TRX - CLOCK: % - %', i, (SELECT transaction_timestamp()), (SELECT clock_timestamp());  
  END LOOP;

  UPDATE regab_action_extract_count_pattern_freqs_tmp_table_a AS ta
    SET total_cnt = ta.cnt_0 + ta.cnt_1 + ta.cnt_2 + ta.cnt_3 + ta.cnt_4 + ta.cnt_5 + ta.cnt_6 + ta.cnt_7;

  SELECT sum(ta.total_cnt) INTO nsamples FROM regab_action_extract_count_pattern_freqs_tmp_table_a AS ta;

  UPDATE regab_action_extract_count_pattern_freqs_tmp_table_a AS ta
    SET relative_frequency = ta.total_cnt::DOUBLE PRECISION / nsamples::DOUBLE PRECISION;
    
  --RAISE NOTICE 'STEP -02- TRX - CLOCK: % - %', (SELECT transaction_timestamp()), (SELECT clock_timestamp());

  RETURN QUERY
  SELECT
    ta.pattern_id,
    ta.index_value,
    ta.mirror_value,
    ta.principal_index_value,
    ta.cnt_0, ta.cnt_1, ta.cnt_2, ta.cnt_3, ta.cnt_4, ta.cnt_5, ta.cnt_6, ta.cnt_7, ta.total_cnt,
    ta.relative_frequency,
    ta.theoretical_probability
  FROM regab_action_extract_count_pattern_freqs_tmp_table_a AS ta ORDER BY index_value;
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
