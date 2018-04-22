--
-- 0106_down_cmp_cmq_cmr.sql
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
-- Removes migration 0106.
--

SET search_path TO reversi;

BEGIN;

DROP INDEX regab_prng_gp_parent_gp_id_idx;
DROP INDEX regab_prng_gp_classification_gp_id_idx;

ALTER TABLE regab_prng_gp DROP COLUMN parent_gp_id;

ALTER TABLE regab_prng_gp DROP CONSTRAINT status_chk;

DELETE FROM migrations WHERE migration_id = 106;

COMMIT;
