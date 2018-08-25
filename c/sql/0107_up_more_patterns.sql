--
-- 0107_up_more_patterns.sql
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
-- Adds more patterns to the regab schema.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0107, now(), 'more_patterns', 'adds all patterns needed by the GLEM implementation as described by M. Buro in his papers');

--
-- Populates the index_prob_given_ec field in table regab_prng_pattern_ranges taking data from
-- the staging table regab_staging_ec_pidx_cnt_tmp (empty_count, index_value, frequency).
--
-- Checks that the pattern_name_in is an entry in the regab_prng_patterns table.
-- Checks that there are the expected number of records belonging to the given pattern in table regab_prng_pattern_ranges.
-- Checks that there are the expected number of records in the staging table.
--
CREATE FUNCTION regab_update_prob_into_pattern_ranges_from_staging (pattern_name_in CHARACTER(6))
RETURNS INTEGER
AS $$
DECLARE
  pid INTEGER;
  pnid INTEGER;
  ni SMALLINT;
  ns SMALLINT;
  nrec_expected BIGINT;
  nrec_counted_in_table BIGINT;
  nrec_counted_in_staging BIGINT;
BEGIN

  SELECT seq, pattern_name_id, ninstances, nsquares INTO pid, pnid, ni, ns
    FROM regab_prng_patterns WHERE pattern_name = pattern_name_in;
  IF pid IS NULL THEN
    RAISE EXCEPTION 'Record not found in table regab_prng_patterns matching pattern_name "%".',
      pattern_name_in;
  END IF;

  SELECT 3^ns*61 INTO nrec_expected;
  SELECT count(1) INTO nrec_counted_in_table FROM regab_prng_pattern_ranges WHERE pattern_id = pid;
  SELECT count(1) INTO nrec_counted_in_staging FROM regab_staging_ec_pidx_cnt_tmp;

  IF nrec_counted_in_table <> nrec_expected THEN
    RAISE EXCEPTION 'The number of record belonging to the "%" pattern must be %, found %.',
      pattern_name_in, nrec_expected, nrec_counted_in_table;
  END IF;

  IF nrec_counted_in_staging <> nrec_expected THEN
    RAISE EXCEPTION 'The number of record found in the staging table must be %, found %.',
      nrec_expected, nrec_counted_in_staging;
  END IF;
  
  WITH freq_totals_by_ec AS (
    SELECT empty_count, sum(frequency) AS cnt
    FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count
  ), frequencies AS (
    SELECT empty_count, index_value, sum(frequency) AS cnt
    FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count, index_value ORDER BY empty_count
  ), probabilities AS (
    SELECT
      f.empty_count AS empty_count,
      f.index_value AS index_value,
      f.cnt / ft.cnt AS probability
    FROM
      freq_totals_by_ec AS ft
    LEFT JOIN
      frequencies AS f ON f.empty_count = ft.empty_count
    ORDER BY
      empty_count, index_value
  ) UPDATE regab_prng_pattern_ranges AS ta
  SET
    index_prob_given_ec = probability,
    cst_time = now(),
    status = 'CMP'
  FROM probabilities AS tb
  WHERE
    ta.pattern_id = pid AND
    ta.index_value = tb.index_value AND
    ta.empty_count = tb.empty_count;
  
  RETURN pnid;
END;
$$ LANGUAGE plpgsql VOLATILE;

---
--- Creates entries in regab_prng_pattern_ranges.
--- Populates the mirror_value and principal_index_value field in table regab_prng_pattern_ranges.
---
DO $$
DECLARE
  pn CHAR(6);
  pid INTEGER;
BEGIN
  pn := 'EDGE';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_edge_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'CORNER';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_corner_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'XEDGE';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_xedge_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R2';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r2_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R3';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r3_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r4_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag4_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG5';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag5_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG6';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag6_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG7';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag7_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG8';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag8_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := '2X5COR';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_2x5cor_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
END $$;

--- To be extended on all pattern, this is EDGE, but a where condition is missing ....
--- Tests.
--DO $$
--DECLARE
--  computed INTEGER;
--BEGIN
--  SELECT count(distinct(index_value)) INTO computed FROM regab_prng_pattern_ranges;
--  PERFORM p_assert(computed = 6561, 'Expected value is 6561.');
--  --
--  SELECT count(distinct(principal_index_value)) INTO computed FROM regab_prng_pattern_ranges;
--  PERFORM p_assert(computed = 3321, 'Expected value is 3321, ((6561 - 3^4) / 2) + 3^4.');
--END $$;



COMMIT;

--
-- Loads probabilities.
-- Populates the index_prob_given_ec field.
--
-- EDGE
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_EDGE_826_1billion.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('EDGE');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- CORNER
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_CORNER_112_116_292_298_372_378_973_977_32000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('CORNER');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- XEDGE
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_XEDGE_112_116_292_298_372_378_973_977_80000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('XEDGE');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R2
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R2_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('R2');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R3
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R3_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('R3');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R4
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R4_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('R4');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG4
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG4_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG4');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG5
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG5_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG5');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG6
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG6_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG6');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG7
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG7_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG7');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG8
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG8_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG8');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- 2X5COR
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_2X5COR_112_116_292_298_372_378_973_977_80000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('2X5COR');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
