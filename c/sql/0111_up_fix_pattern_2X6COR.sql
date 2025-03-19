--
-- 0111_up_fix_pattern_2X6COR.sql
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
-- Fixes pattern 2X6COR if needed.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0111, now(), 'fix_2X6COR', 'fixes pattern 2X6COR if needed');

--
-- Creates or re-creates the two functions that pack and unpack the 2X6COR pattern.
--

DO $a$
<<block_a>>

DECLARE
  fun_oid OID := NULL;
  fun_name VARCHAR := 'square_set_pattern_pack_2x6cor';
  command VARCHAR;

BEGIN

  SELECT oid FROM pg_proc WHERE proname = fun_name INTO fun_oid;

  IF fun_oid IS NOT NULL THEN
    RAISE NOTICE 'FUNCTION % EXISTS, DROPPING IT ...', fun_name;
    command := CONCAT('DROP FUNCTION IF EXISTS ', fun_name, ';');
    --RAISE NOTICE 'command = %', command;
    EXECUTE format(command);
  ELSE
    RAISE NOTICE 'FUNCTION % DOES NOT EXIST', fun_name;
  END IF;

  --
  -- Packs 2X6COR pattern.
  -- Transforms instance zero pattern to packed pattern.
  --
  CREATE FUNCTION square_set_pattern_pack_2x6cor (s square_set)
  RETURNS square_set
  AS $fun_def$
  DECLARE
    s1 square_set := (x'000000000000003f')::BIGINT;
    s2 square_set := (x'0000000000003f00')::BIGINT;
  BEGIN
    RETURN (s & s1) | ((s & s2) >> 2);
  END;
  $fun_def$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

  --- Tests ...
  DO $fun_test$
  DECLARE
    full_              square_set := (x'ffffffffffffffff')::BIGINT;
    empty_             square_set := (x'0000000000000000')::BIGINT;
    nw_2x6cor          square_set := (x'0000000000003f3f')::BIGINT;
    packed_2x6cor_mask square_set := (x'0000000000000fff')::BIGINT;
  BEGIN
    PERFORM p_assert(square_set_pattern_pack_2x6cor(empty_) = empty_, 'Expected result is empty_.');
    PERFORM p_assert(square_set_pattern_pack_2x6cor(full_) = packed_2x6cor_mask, 'Expected result is packed_2x6cor_mask.');
    PERFORM p_assert(square_set_pattern_pack_2x6cor(nw_2x6cor) = packed_2x6cor_mask, 'Expected result is packed_2x6cor_mask.');
    PERFORM p_assert(square_set_pattern_pack_2x6cor((x'000000000000003f')::square_set) = (x'000000000000003f')::square_set, 'Expected result is 000000000000003f.');
    PERFORM p_assert(square_set_pattern_pack_2x6cor((x'0000000000003f00')::square_set) = (x'0000000000000fc0')::square_set, 'Expected result is 0000000000000fc0.');
    PERFORM p_assert(square_set_pattern_pack_2x6cor((x'ffffffffffffc0c0')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
  END $fun_test$;

END block_a $a$;

DO $b$
<<block_b>>

DECLARE
  fun_oid OID := NULL;
  fun_name VARCHAR := 'square_set_pattern_unpack_2x6cor';
  command VARCHAR;

BEGIN

  SELECT oid FROM pg_proc WHERE proname = fun_name INTO fun_oid;

  IF fun_oid IS NOT NULL THEN
    RAISE NOTICE 'FUNCTION % EXISTS, DROPPING IT ...', fun_name;
    command := CONCAT('DROP FUNCTION IF EXISTS ', fun_name, ';');
    --RAISE NOTICE 'command = %', command;
    EXECUTE format(command);
  ELSE
    RAISE NOTICE 'FUNCTION % DOES NOT EXIST', fun_name;
  END IF;

  --
  -- Un-packs 2X6COR pattern.
  -- Transforms packed pattern to instance zero pattern.
  --
  CREATE FUNCTION square_set_pattern_unpack_2x6cor (s square_set)
  RETURNS square_set
  AS $fun_def$
  DECLARE
    s1 square_set := (x'000000000000003f')::BIGINT;
    s2 square_set := (x'0000000000000fc0')::BIGINT;
  BEGIN
    RETURN (s & s1) | ((s & s2) << 2);
  END;
  $fun_def$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
  --- Tests ...
  DO $fun_test$
  DECLARE
    full_              square_set := (x'ffffffffffffffff')::BIGINT;
    empty_             square_set := (x'0000000000000000')::BIGINT;
    nw_2x6cor          square_set := (x'0000000000003f3f')::BIGINT;
    packed_2x6cor_mask square_set := (x'0000000000000fff')::BIGINT;
  BEGIN
    PERFORM p_assert(square_set_pattern_unpack_2x6cor(empty_) = empty_, 'Expected result is empty_.');
    PERFORM p_assert(square_set_pattern_unpack_2x6cor(packed_2x6cor_mask) = nw_2x6cor, 'Expected result is nw_2x6cor.');
    PERFORM p_assert(square_set_pattern_unpack_2x6cor((x'000000000000003f')::square_set) = (x'000000000000003f')::square_set, 'Expected result is 000000000000003f.');
    PERFORM p_assert(square_set_pattern_unpack_2x6cor((x'0000000000000fc0')::square_set) = (x'0000000000003f00')::square_set, 'Expected result is 0000000000003f00.');
  END $fun_test$;

END block_b $b$;


DO $c$
<<block_c>>

DECLARE
  fun_oid OID := NULL;
  fun_name VARCHAR := 'regab_mirror_value_2x6cor_pattern';
  command VARCHAR;

BEGIN

  SELECT oid FROM pg_proc WHERE proname = fun_name INTO fun_oid;

  IF fun_oid IS NOT NULL THEN
    RAISE NOTICE 'FUNCTION % EXISTS, DROPPING IT ...', fun_name;
    command := CONCAT('DROP FUNCTION IF EXISTS ', fun_name, ';');
    --RAISE NOTICE 'command = %', command;
    EXECUTE format(command);
  ELSE
    RAISE NOTICE 'FUNCTION % DOES NOT EXIST', fun_name;
  END IF;

  --
  -- Computes the mirror value for the given index, for the 2X6COR pattern.
  --
  CREATE FUNCTION regab_mirror_value_2x6cor_pattern (index_value INTEGER)
  RETURNS INTEGER
  AS $fun_def$
  BEGIN
    RETURN NULL;
  END;
  $fun_def$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
  --- Tests.
  DO $fun_test$
  BEGIN
    PERFORM p_assert(regab_mirror_value_2x6cor_pattern(0) IS NULL, 'Expected value is NULL.');
  END $fun_test$;

END block_c $c$;

--
--
--

DO $d$
<<block_d>>

DECLARE
  pattern_id_2x6cor INTEGER;

BEGIN

  SELECT pattern_id FROM regab_prng_patterns WHERE pattern_name = '2X6COR' INTO pattern_id_2x6cor;

  IF pattern_id_2x6cor IS NULL THEN
    INSERT INTO regab_prng_patterns (ins_time, pattern_id, pattern_name, ninstances, nsquares, description)
      VALUES (now(), 13, '2X6COR',  8, 12, 'Twelve square, asymmetric corner');
  END IF;

END block_d $d$;

---
--- Creates entries in regab_prng_pattern_ranges.
--- Populates the mirror_value and principal_index_value field in table regab_prng_pattern_ranges.
---
DO $e$
<<block_e>>
DECLARE
  pn CHAR(6);
  pid SMALLINT;
  p_count INTEGER;
BEGIN
  pn := '2X6COR';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT pattern_id INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  SELECT count(1) FROM regab_prng_pattern_ranges WHERE pattern_id = pid INTO p_count;
  IF p_count = 0 AND pid IS NOT NULL THEN
    PERFORM ragab_populate_pattern_ranges(pn);
    UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_2x6cor_pattern(index_value) WHERE pattern_id = pid;
    UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
    PERFORM ragab_populate_pattern_probs(pn);
  END IF;

END block_e $e$;

--
-- 2X6COR
--
SELECT 'Loading frequencies for the 2X6COR pattern ...' AS message;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_2X6COR_762291_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_probs_from_staging('2X6COR');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;

COMMIT;

VACUUM ANALYZE regab_prng_pattern_probs;
