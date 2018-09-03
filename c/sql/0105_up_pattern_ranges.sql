--
-- 0105_up_pattern_ranges.sql
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
-- Creates a new table regab_prng_pattern_ranges
--

SET search_path TO reversi;

BEGIN;

-- Migration set-up
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0105, now(), 'pattern_ranges', 'adds a new table regab_prng_pattern_ranges that contains pattern indexes and their properties.');


-- End of migration
COMMIT;

VACUUM ANALYZE regab_prng_pattern_ranges;
