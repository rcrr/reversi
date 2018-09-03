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
VALUES (0107, now(), 'load_patterns', 'loads patterns, pattern ranges, and pattern statistics');

---
--- Populates the patter table with EDGE, CORNER, XEDGE, R2, R3, R4, DIAG4, DIAG5, DIAG6, DIAG7, DIAG8, and 2X5COR patterns.
---
INSERT INTO regab_prng_patterns (ins_time, pattern_id, pattern_name, ninstances, nsquares, description)
  SELECT * FROM (VALUES
    (now(),  0, 'EDGE',    4,  8, 'The edge of the board'),
    (now(),  1, 'CORNER',  4,  9, 'The 3x3 corner'),
    (now(),  2, 'XEDGE',   4, 10, 'The edge of the board plus X squares'),
    (now(),  3, 'R2',      4,  8, 'Second row, A2-B2-C2-D2-E2-F2-G2-H2'),
    (now(),  4, 'R3',      4,  8, 'Third row, A3-B3-C3-D3-E3-F3-G3-H3'),
    (now(),  5, 'R4',      4,  8, 'Fourth row, A4-B4-C4-D4-E4-F4-G4-H4'),
    (now(),  6, 'DIAG4',   4,  4, 'Four square diagonal, D1-C2-B3-A4'),
    (now(),  7, 'DIAG5',   4,  5, 'Five square diagonal, E1-D2-C3-B4-A5'),
    (now(),  8, 'DIAG6',   4,  6, 'Six square diagonal, F1-E2-D3-C4-B5-A6'),
    (now(),  9, 'DIAG7',   4,  7, 'Seven square diagonal, G1-F2-E3-D4-C5-B6-A7'),
    (now(), 10, 'DIAG8',   2,  8, 'Eight square diagonal, H1-G2-F3-E4-D5-C6-B7-A8'),
    (now(), 11, '2X5COR',  8, 10, 'Ten square, asymmetric corner')
  ) AS tmp_table(ins_time, pattern_id, pattern_name, ninstances, nsquares, description);

---
--- Creates entries in regab_prng_pattern_ranges.
--- Populates the mirror_value and principal_index_value field in table regab_prng_pattern_ranges.
---
DO $$
DECLARE
  pn CHAR(6);
  pid SMALLINT;
BEGIN
  pn := 'EDGE';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_edge_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'CORNER';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_corner_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'XEDGE';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_xedge_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R2';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r2_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R3';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r3_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r4_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag4_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG5';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag5_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG6';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag6_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG7';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag7_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG8';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag8_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := '2X5COR';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_2x5cor_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
END $$;

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
