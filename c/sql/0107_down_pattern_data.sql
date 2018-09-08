--
-- 0107_down_pattern_data.sql
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
-- Removes pattern_data.
--

SET search_path TO reversi;

BEGIN;
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'DIAG3');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = '2X5COR');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'DIAG8');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'DIAG7');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'DIAG6');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'DIAG5');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'DIAG4');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'R4');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'R3');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'R2');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'XEDGE');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'CORNER');
DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = (SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = 'EDGE');

DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG3';
DELETE FROM regab_prng_patterns WHERE pattern_name = '2X5COR';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG8';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG7';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG6';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG5';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'DIAG4';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'R4';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'R3';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'R2';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'XEDGE';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'CORNER';
DELETE FROM regab_prng_patterns WHERE pattern_name = 'EDGE';

DELETE FROM migrations WHERE migration_id = 107;

COMMIT;

VACUUM ANALYZE regab_prng_pattern_ranges;
VACUUM ANALYZE regab_prng_pattern_probs;
