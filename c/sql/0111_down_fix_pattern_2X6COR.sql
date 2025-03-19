--
-- 0111_down_fix_pattern_2X6COR.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2025 Roberto Corradini. All rights reserved.
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
-- Removes migration 0111.
--

SET search_path TO reversi;

BEGIN;

DO $a$
<<block_a>>

DECLARE

  do_clean_everything BOOLEAN := True;
  
BEGIN

  IF do_clean_everything THEN
    DELETE FROM regab_prng_pattern_probs AS p
      USING regab_prng_pattern_ranges AS r
      WHERE p.range_id = r.seq AND r.pattern_id = 13;
    DELETE FROM regab_prng_pattern_ranges WHERE pattern_id = 13;
    DELETE FROM regab_prng_patterns WHERE pattern_id = 13;
    
    DROP FUNCTION IF EXISTS square_set_pattern_pack_2x6cor;
    DROP FUNCTION IF EXISTS square_set_pattern_unpack_2x6cor;
    DROP FUNCTION IF EXISTS regab_mirror_value_2x6cor_pattern;
  END IF;

END block_a $a$;

DELETE FROM migrations WHERE migration_id = 111;

COMMIT;
