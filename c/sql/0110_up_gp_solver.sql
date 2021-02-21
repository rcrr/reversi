--
-- 0110_up_gp_solver.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2021 Roberto Corradini. All rights reserved.
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
-- Adds the solver field to the regab_prng_gp table.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0110, now(), 'gp_solver', 'adds the solver field to the game position table');

--
-- The new field solver can take any value, it is set by the ragab program when it solves the position.
--
ALTER TABLE regab_prng_gp ADD COLUMN solver CHAR(4) DEFAULT NULL;

--
-- Sets the value of the solver field to 'es' for all the solved positions.
--
-- SELECT count(1) FROM regab_prng_gp WHERE status IN ('CMP','CMQ','CMS') AND solver IS NULL;
UPDATE regab_prng_gp SET solver = 'es' WHERE status IN ('CMP','CMQ','CMS') AND solver IS NULL;

COMMIT;
