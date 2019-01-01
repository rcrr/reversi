--
-- 0109_down_action_extract.sql
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
-- Removes migration 0109.
--

SET search_path TO reversi;

BEGIN;

DROP FUNCTION regab_action_extract_game_pos_prepare_cursor;
DROP FUNCTION regab_action_extract_count_pattern_freqs;
DROP FUNCTION regab_action_extract_count_positions;
DROP FUNCTION regab_action_extract_check_patterns;
DROP FUNCTION regab_action_extract_check_batches;

DROP TABLE regab_prng_gp_pattern_class_instance_names;

DELETE FROM migrations WHERE migration_id = 109;

COMMIT;
