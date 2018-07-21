--
-- 0107_down_more_patterns.sql
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
-- Removes more_patterns.
--

SET search_path TO reversi;

BEGIN;
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = '2X5COR');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'DIAG8');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'DIAG7');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'DIAG6');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'DIAG5');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'DIAG4');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'R4');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'R3');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'R2');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'XEDGE');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'CORNER');

DROP FUNCTION regab_mirror_value_2x5cor_pattern;
DROP FUNCTION regab_mirror_value_diag8_pattern;
DROP FUNCTION regab_mirror_value_diag7_pattern;
DROP FUNCTION regab_mirror_value_diag6_pattern;
DROP FUNCTION regab_mirror_value_diag5_pattern;
DROP FUNCTION regab_mirror_value_diag4_pattern;
DROP FUNCTION regab_mirror_value_xedge_pattern;
DROP FUNCTION regab_mirror_value_corner_pattern;
DROP FUNCTION regab_mirror_value_r4_pattern;
DROP FUNCTION regab_mirror_value_r3_pattern;
DROP FUNCTION regab_mirror_value_r2_pattern;

DROP FUNCTION square_set_flip_diag_a1h8;
DROP FUNCTION square_set_flip_vertical;

DROP FUNCTION square_set_pattern_unpack_2x5cor;
DROP FUNCTION square_set_pattern_pack_2x5cor;
DROP FUNCTION square_set_pattern_unpack_diag8;
DROP FUNCTION square_set_pattern_pack_diag8;
DROP FUNCTION square_set_pattern_unpack_diag7;
DROP FUNCTION square_set_pattern_pack_diag7;
DROP FUNCTION square_set_pattern_unpack_diag6;
DROP FUNCTION square_set_pattern_pack_diag6;
DROP FUNCTION square_set_pattern_unpack_diag5;
DROP FUNCTION square_set_pattern_pack_diag5;
DROP FUNCTION square_set_pattern_unpack_diag4;
DROP FUNCTION square_set_pattern_pack_diag4;
DROP FUNCTION square_set_pattern_unpack_xedge;
DROP FUNCTION square_set_pattern_pack_xedge;
DROP FUNCTION square_set_pattern_unpack_corner;
DROP FUNCTION square_set_pattern_pack_corner;

DROP FUNCTION regab_update_prob_into_pattern_ranges_from_staging;

ALTER FUNCTION ragab_populate_pattern_ranges RENAME TO populate_regab_prng_pattern_ranges;

ALTER TABLE regab_prng_patterns DROP CONSTRAINT regab_prng_patterns_name_id_key;

ALTER TABLE regab_prng_patterns DROP COLUMN pattern_name_id;

DELETE FROM regab_prng_patterns WHERE pattern_name = '2X5COR';

DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG8';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG7';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG6';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG5';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG4';

DELETE FROM regab_prng_patterns WHERE pattern_name = 'R4';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'R3';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'R2';

DELETE FROM migrations WHERE migration_id = 107;

COMMIT;
