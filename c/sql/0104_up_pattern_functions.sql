--
-- 0104_up_pattern_functions.sql
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
-- Creates functions that works on patterns.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0104, now(), 'pattern_functionss', 'adds square set flips and mirrors, as well as pack and unpack functions');

--
-- Mirrors a square set horizontally.
-- Column A is mapped to column H and vice versa.
--
CREATE FUNCTION square_set_flip_vertical (s square_set)
RETURNS square_set
AS $$
DECLARE
  k1 square_set := (x'5555555555555555')::BIGINT;
  k2 square_set := (x'3333333333333333')::BIGINT;
  k4 square_set := (x'0f0f0f0f0f0f0f0f')::BIGINT;
  r square_set := s;
BEGIN
  r := ((r >> 1) & k1) | ((r & k1) << 1);
  r := ((r >> 2) & k2) | ((r & k2) << 2);
  r := ((r >> 4) & k4) | ((r & k4) << 4);
  RETURN r;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

COMMIT;
