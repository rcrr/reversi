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
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_edge_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'CORNER';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_corner_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'XEDGE';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_xedge_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'R2';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r2_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'R3';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r3_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'R4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r4_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'DIAG4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag4_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'DIAG5';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag5_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'DIAG6';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag6_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'DIAG7';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag7_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := 'DIAG8';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag8_pattern(index_value)  WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
  pn := '2X5COR';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_2x5cor_pattern(index_value) WHERE pattern_id = pid;
  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  PERFORM ragab_populate_pattern_probs(pn);
  --
END $$;

--
-- This is a functional thest that cannot be run before having populated the regab_prng_pattern_probs table.
-- Calling the function regab_gp_compute_pattern_indexes(mo, op, is_principal) with the boolena parameter is_principal
-- being TRUE require the described data being loaded.
--
-- Tests.
DO $$
DECLARE
  mo square_set := 2337477075470322191::BIGINT;
  op square_set := 6633411755847992320::BIGINT;
  black player := 0::player;
  pattern_index_values RECORD;
  --
  is_principal BOOL := TRUE; -- could not be true before loading the regab_prng_pattern_probs table.
BEGIN
  SELECT * INTO pattern_index_values FROM regab_gp_compute_pattern_indexes(mo, op, is_principal);
  ---
  PERFORM p_assert(pattern_index_values.i_edge_0 =   40, 'Expected value for i_edge_0 is   40.');
  PERFORM p_assert(pattern_index_values.i_edge_1 =  234, 'Expected value for i_edge_1 is  234.');
  PERFORM p_assert(pattern_index_values.i_edge_2 =  717, 'Expected value for i_edge_2 is  717.');
  PERFORM p_assert(pattern_index_values.i_edge_3 =  253, 'Expected value for i_edge_3 is  253.');
  ---
  PERFORM p_assert(pattern_index_values.i_corner_0 = 10057, 'Expected value for i_corner_0 is 10057.');
  PERFORM p_assert(pattern_index_values.i_corner_1 = 13284, 'Expected value for i_corner_1 is 13284.');
  PERFORM p_assert(pattern_index_values.i_corner_2 = 9873, 'Expected value for i_corner_2 is 9873.');
  PERFORM p_assert(pattern_index_values.i_corner_3 = 16704, 'Expected value for i_corner_3 is 16704.');
  ---
  PERFORM p_assert(pattern_index_values.i_xedge_0 = 36045, 'Expected value for i_xedge_0 is 36045.');
  PERFORM p_assert(pattern_index_values.i_xedge_1 = 33507, 'Expected value for i_xedge_1 is 33507.');
  PERFORM p_assert(pattern_index_values.i_xedge_2 = 34740, 'Expected value for i_xedge_2 is 34740.');
  PERFORM p_assert(pattern_index_values.i_xedge_3 = 35244, 'Expected value for i_xedge_3 is 35244.');
  ---
  PERFORM p_assert(pattern_index_values.i_r2_0 = 1302, 'Expected value for i_r2_0 is 1302.');
  PERFORM p_assert(pattern_index_values.i_r2_1 = 1526, 'Expected value for i_r2_1 is 1526.');
  PERFORM p_assert(pattern_index_values.i_r2_2 = 1131, 'Expected value for i_r2_2 is 1131.');
  PERFORM p_assert(pattern_index_values.i_r2_3 = 1741, 'Expected value for i_r2_3 is 1741.');
  --- 
  PERFORM p_assert(pattern_index_values.i_r3_0 =  715, 'Expected value for i_r3_0 is  715.');
  PERFORM p_assert(pattern_index_values.i_r3_1 =  661, 'Expected value for i_r3_1 is  661.');
  PERFORM p_assert(pattern_index_values.i_r3_2 = 3632, 'Expected value for i_r3_2 is 3632.');
  PERFORM p_assert(pattern_index_values.i_r3_3 = 4103, 'Expected value for i_r3_3 is 4103.');
  ---
  PERFORM p_assert(pattern_index_values.i_r4_0 = 1451, 'Expected value for i_r4_0 is 1451.');
  PERFORM p_assert(pattern_index_values.i_r4_1 =  725, 'Expected value for i_r4_1 is  725.');
  PERFORM p_assert(pattern_index_values.i_r4_2 =  467, 'Expected value for i_r4_2 is  467.');
  PERFORM p_assert(pattern_index_values.i_r4_3 = 3644, 'Expected value for i_r4_3 is 3644.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag4_0 = 16, 'Expected value for i_diag4_0 is 16.');
  PERFORM p_assert(pattern_index_values.i_diag4_1 =  2, 'Expected value for i_diag4_1 is  2.');
  PERFORM p_assert(pattern_index_values.i_diag4_2 = 68, 'Expected value for i_diag4_2 is 68.');
  PERFORM p_assert(pattern_index_values.i_diag4_3 = 17, 'Expected value for i_diag4_3 is 17.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag5_0 =  39, 'Expected value for i_diag5_0 is  39.');
  PERFORM p_assert(pattern_index_values.i_diag5_1 = 101, 'Expected value for i_diag5_1 is 101.');
  PERFORM p_assert(pattern_index_values.i_diag5_2 = 206, 'Expected value for i_diag5_2 is 206.');
  PERFORM p_assert(pattern_index_values.i_diag5_3 =  26, 'Expected value for i_diag5_3 is  26.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag6_0 =  73, 'Expected value for i_diag6_0 is  73.');
  PERFORM p_assert(pattern_index_values.i_diag6_1 = 404, 'Expected value for i_diag6_1 is 404.');
  PERFORM p_assert(pattern_index_values.i_diag6_2 =  26, 'Expected value for i_diag6_2 is  26.');
  PERFORM p_assert(pattern_index_values.i_diag6_3 = 373, 'Expected value for i_diag6_3 is 373.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag7_0 = 228, 'Expected value for i_diag7_0 is 228.');
  PERFORM p_assert(pattern_index_values.i_diag7_1 = 322, 'Expected value for i_diag7_1 is 322.');
  PERFORM p_assert(pattern_index_values.i_diag7_2 = 240, 'Expected value for i_diag7_2 is 240.');
  PERFORM p_assert(pattern_index_values.i_diag7_3 = 482, 'Expected value for i_diag7_3 is 482.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag8_0 = 2184, 'Expected value for i_diag8_0 is 2184.');
  PERFORM p_assert(pattern_index_values.i_diag8_1 = 1201, 'Expected value for i_diag8_1 is 1201.');
  ---
  PERFORM p_assert(pattern_index_values.i_2x5cor_0 = 11704, 'Expected value for i_2x5cor_0 is 11704.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_1 = 41040, 'Expected value for i_2x5cor_1 is 41040.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_2 = 49074, 'Expected value for i_2x5cor_2 is 49074.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_3 = 23337, 'Expected value for i_2x5cor_3 is 23337.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_4 = 21222, 'Expected value for i_2x5cor_4 is 21222.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_5 = 16758, 'Expected value for i_2x5cor_5 is 16758.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_6 = 38871, 'Expected value for i_2x5cor_6 is 38871.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_7 =  9730, 'Expected value for i_2x5cor_7 is  9730.');
END $$;

COMMIT;

--
-- Loads probabilities.
-- Populates the index_prob_given_ec field.
VACUUM ANALYZE regab_prng_pattern_ranges;
--
-- EDGE
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_EDGE_826_1billion.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('EDGE');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- CORNER
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_CORNER_112_116_292_298_372_378_973_977_32000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('CORNER');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- XEDGE
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_XEDGE_112_116_292_298_372_378_973_977_80000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('XEDGE');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R2
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R2_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('R2');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R3
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R3_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('R3');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R4
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R4_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('R4');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG4
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG4_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('DIAG4');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG5
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG5_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('DIAG5');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG6
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG6_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('DIAG6');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG7
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG7_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('DIAG7');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG8
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG8_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('DIAG8');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- 2X5COR
--
VACUUM ANALYZE regab_prng_pattern_probs;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_2X5COR_112_116_292_298_372_378_973_977_80000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('2X5COR');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
