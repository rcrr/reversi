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
VALUES (0107, now(), 'more_patterns', 'adds all patterns needed by the GLEM implementation as described by M. Buro in his papers');

--
-- Populates the index_prob_given_ec field in table regab_prng_pattern_ranges taking data from
-- the staging table regab_staging_ec_pidx_cnt_tmp (empty_count, index_value, frequency).
--
-- Checks that the pattern_name_in is an entry in the regab_prng_patterns table.
-- Checks that there are the expected number of records belonging to the given pattern in table regab_prng_pattern_ranges.
-- Checks that there are the expected number of records in the staging table.
--
CREATE FUNCTION regab_update_prob_into_pattern_ranges_from_staging (pattern_name_in CHARACTER(6))
RETURNS INTEGER
AS $$
DECLARE
  pid INTEGER;
  pnid INTEGER;
  ni SMALLINT;
  ns SMALLINT;
  nrec_expected BIGINT;
  nrec_counted_in_table BIGINT;
  nrec_counted_in_staging BIGINT;
BEGIN

  SELECT seq, pattern_name_id, ninstances, nsquares INTO pid, pnid, ni, ns
    FROM regab_prng_patterns WHERE pattern_name = pattern_name_in;
  IF pid IS NULL THEN
    RAISE EXCEPTION 'Record not found in table regab_prng_patterns matching pattern_name "%".',
      pattern_name_in;
  END IF;

  SELECT 3^ns*61 INTO nrec_expected;
  SELECT count(1) INTO nrec_counted_in_table FROM regab_prng_pattern_ranges WHERE pattern_id = pid;
  SELECT count(1) INTO nrec_counted_in_staging FROM regab_staging_ec_pidx_cnt_tmp;

  IF nrec_counted_in_table <> nrec_expected THEN
    RAISE EXCEPTION 'The number of record belonging to the "%" pattern must be %, found %.',
      pattern_name_in, nrec_expected, nrec_counted_in_table;
  END IF;

  IF nrec_counted_in_staging <> nrec_expected THEN
    RAISE EXCEPTION 'The number of record found in the staging table must be %, found %.',
      nrec_expected, nrec_counted_in_staging;
  END IF;
  
  WITH freq_totals_by_ec AS (
    SELECT empty_count, sum(frequency) AS cnt
    FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count
  ), frequencies AS (
    SELECT empty_count, index_value, sum(frequency) AS cnt
    FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count, index_value ORDER BY empty_count
  ), probabilities AS (
    SELECT
      f.empty_count AS empty_count,
      f.index_value AS index_value,
      f.cnt / ft.cnt AS probability
    FROM
      freq_totals_by_ec AS ft
    LEFT JOIN
      frequencies AS f ON f.empty_count = ft.empty_count
    ORDER BY
      empty_count, index_value
  ) UPDATE regab_prng_pattern_ranges AS ta
  SET
    index_prob_given_ec = probability,
    cst_time = now(),
    status = 'CMP'
  FROM probabilities AS tb
  WHERE
    ta.pattern_id = pid AND
    ta.index_value = tb.index_value AND
    ta.empty_count = tb.empty_count;
  
  RETURN pnid;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Packs CORNER pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_corner (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000000000007')::BIGINT;
  s2 square_set := (x'0000000000000700')::BIGINT;
  s3 square_set := (x'0000000000070000')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) >> 5) | ((s & s3) >> 10);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs CORNER pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_corner (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000000000007')::BIGINT;
  s2 square_set := (x'0000000000000038')::BIGINT;
  s3 square_set := (x'00000000000001c0')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) << 5) | ((s & s3) << 10);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs XEDGE pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_xedge (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
  s2 square_set := (x'0000000000000200')::BIGINT;
  s3 square_set := (x'0000000000004000')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) >> 1) | ((s & s3) >> 5);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs XEDGE pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_xedge (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
  s2 square_set := (x'0000000000000100')::BIGINT;
  s3 square_set := (x'0000000000000200')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) << 1) | ((s & s3) << 5);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs DIAG4 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_diag4 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000001020408')::BIGINT;
  s2 square_set := (x'000000000000000f')::BIGINT;
  r0 square_set;
  r1 square_set;
  r2 square_set;
BEGIN
  r0 := s & s1;
  r1 := (r0 >> 16) | r0;
  r2 := (r1 >>  8) | r1;
  RETURN r2 & s2;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs DIAG4 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_diag4 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000001020408')::BIGINT;
  r square_set;
BEGIN
  r := s | (s <<  8);
  r := r | (r << 16);
  RETURN r & s1;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs DIAG5 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_diag5 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000102040810')::BIGINT;
  s2 square_set := (x'000000000000001f')::BIGINT;
  r0 square_set;
  r1 square_set;
  r2 square_set;
  r3 square_set;
BEGIN
  r0 := s & s1;
  r1 := (r0 >> 32) | r0;
  r2 := (r1 >> 16) | r1;
  r3 := (r2 >>  8) | r2;
  RETURN r3 & s2;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs DIAG5 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_diag5 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000102040810')::BIGINT;
  r square_set;
BEGIN
  r := s | (s <<  8);
  r := r | (r << 16);
  r := r | (r << 32);
  RETURN r & s1;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs DIAG6 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_diag6 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000010204081020')::BIGINT;
  s2 square_set := (x'000000000000003f')::BIGINT;
  r0 square_set;
  r1 square_set;
  r2 square_set;
  r3 square_set;
BEGIN
  r0 := s & s1;
  r1 := (r0 >> 32) | r0;
  r2 := (r1 >> 16) | r1;
  r3 := (r2 >>  8) | r2;
  RETURN r3 & s2;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs DIAG6 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_diag6 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000010204081020')::BIGINT;
  r square_set;
BEGIN
  r := s | (s <<  8);
  r := r | (r << 16);
  r := r | (r << 32);
  RETURN r & s1;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs DIAG7 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_diag7 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0001020408102040')::BIGINT;
  s2 square_set := (x'000000000000007f')::BIGINT;
  r0 square_set;
  r1 square_set;
  r2 square_set;
  r3 square_set;
BEGIN
  r0 := s & s1;
  r1 := (r0 >> 32) | r0;
  r2 := (r1 >> 16) | r1;
  r3 := (r2 >>  8) | r2;
  RETURN r3 & s2;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs DIAG7 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_diag7 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0001020408102040')::BIGINT;
  r square_set;
BEGIN
  r := s | (s <<  8);
  r := r | (r << 16);
  r := r | (r << 32);
  RETURN r & s1;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs DIAG8 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_diag8 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0102040810204080')::BIGINT;
  s2 square_set := (x'00000000000000ff')::BIGINT;
  r0 square_set;
  r1 square_set;
  r2 square_set;
  r3 square_set;
BEGIN
  r0 := s & s1;
  r1 := (r0 >> 32) | r0;
  r2 := (r1 >> 16) | r1;
  r3 := (r2 >>  8) | r2;
  RETURN r3 & s2;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs DIAG8 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_diag8 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0102040810204080')::BIGINT;
  r square_set;
BEGIN
  r := s | (s <<  8);
  r := r | (r << 16);
  r := r | (r << 32);
  RETURN r & s1;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs 2X5COR pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_2x5cor (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'000000000000001f')::BIGINT;
  s2 square_set := (x'0000000000001f00')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) >> 3);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs 2X5COR pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_2x5cor (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'000000000000001f')::BIGINT;
  s2 square_set := (x'0000000000001f00')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) << 3);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Flips a square set about the diagonal A1-H8.
-- Square H1 is mapped to A8 and vice versa.
--
CREATE FUNCTION square_set_flip_diag_a1h8 (s square_set)
RETURNS square_set
AS $$
DECLARE
  k1 square_set := (x'5500550055005500')::BIGINT;
  k2 square_set := (x'3333000033330000')::BIGINT;
  k4 square_set := (x'0f0f0f0f00000000')::BIGINT;
  r  square_set := s;
  q  square_set;
BEGIN
  q := k4 & (r # (r << 28));
  r :=  r # (q # (q >> 28));
  q := k2 & (r # (r << 14));
  r :=  r # (q # (q >> 14));
  q := k1 & (r # (r <<  7));
  r :=  r # (q # (q >>  7));
  RETURN r;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the CORNER pattern.
--
CREATE FUNCTION regab_mirror_value_corner_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_corner(mo);
  op := square_set_pattern_unpack_corner(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_corner(mo);
  op := square_set_pattern_pack_corner(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the XEDGE pattern.
--
CREATE FUNCTION regab_mirror_value_xedge_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_xedge(mo);
  op := square_set_pattern_unpack_xedge(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_vertical(mo); op := square_set_flip_vertical(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_xedge(mo);
  op := square_set_pattern_pack_xedge(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the R2 pattern.
-- Patterns EDGE, R2, R3, and R4 have the same mapping.
--
CREATE FUNCTION regab_mirror_value_r2_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := mo << 8; op := op << 8;
  
  -- step 2: mirror transformation
  mo := square_set_flip_vertical(mo); op := square_set_flip_vertical(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := mo >> 8; op := op >> 8;
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- See function documenattion for R2 pattern.
--
CREATE FUNCTION regab_mirror_value_r3_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
BEGIN
  RETURN regab_mirror_value_r2_pattern(index_value);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- See function documenattion for R2 pattern.
--
CREATE FUNCTION regab_mirror_value_r4_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
BEGIN
  RETURN regab_mirror_value_r2_pattern(index_value);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the DIAG4 pattern.
--
CREATE FUNCTION regab_mirror_value_diag4_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_diag4(mo);
  op := square_set_pattern_unpack_diag4(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_diag4(mo);
  op := square_set_pattern_pack_diag4(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the DIAG5 pattern.
--
CREATE FUNCTION regab_mirror_value_diag5_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_diag5(mo);
  op := square_set_pattern_unpack_diag5(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_diag5(mo);
  op := square_set_pattern_pack_diag5(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the DIAG6 pattern.
--
CREATE FUNCTION regab_mirror_value_diag6_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_diag6(mo);
  op := square_set_pattern_unpack_diag6(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_diag6(mo);
  op := square_set_pattern_pack_diag6(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the DIAG7 pattern.
--
CREATE FUNCTION regab_mirror_value_diag7_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_diag7(mo);
  op := square_set_pattern_unpack_diag7(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_diag7(mo);
  op := square_set_pattern_pack_diag7(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the DIAG8 pattern.
--
CREATE FUNCTION regab_mirror_value_diag8_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  mo square_set;
  op square_set;
  mirror_value INTEGER;
BEGIN
  -- step 0: index to transformed patterrn
  SELECT mover, opponent INTO mo, op FROM regab_index_to_transformed_pattern(index_value);
    
  -- step 1: transformed pattern to instance zero pattern
  mo := square_set_pattern_unpack_diag8(mo);
  op := square_set_pattern_unpack_diag8(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_diag8(mo);
  op := square_set_pattern_pack_diag8(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Computes the mirror value for the given index, for the 2X5COR pattern.
--
CREATE FUNCTION regab_mirror_value_2x5cor_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
BEGIN
  RETURN NULL;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;


---
--- Creates entries in regab_prng_pattern_ranges.
--- Populates the mirror_value and principal_index_value field in table regab_prng_pattern_ranges.
---
DO $$
DECLARE
  pn CHAR(6);
  pid INTEGER;
BEGIN
  pn := 'CORNER';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_corner_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'XEDGE';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_xedge_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R2';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r2_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R3';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r3_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'R4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_r4_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG4';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag4_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG5';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag5_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG6';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag6_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG7';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag7_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := 'DIAG8';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_diag8_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
  pn := '2X5COR';
  RAISE NOTICE 'Pattern %: loading pattern ranges.', pn;
  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
  PERFORM ragab_populate_pattern_ranges(pn);
  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_2x5cor_pattern(index_value),
    principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
  --
END $$;



COMMIT;

--
-- Loads probabilities.
-- Populates the index_prob_given_ec field.
--
-- CORNER
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_CORNER_112_116_292_298_372_378_973_977_32000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('CORNER');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- XEDGE
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_XEDGE_112_116_292_298_372_378_973_977_80000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('XEDGE');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R2
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R2_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('R2');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R3
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R3_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('R3');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- R4
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_R4_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('R4');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG4
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG4_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG4');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG5
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG5_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG5');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG6
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG6_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG6');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG7
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG7_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG7');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- DIAG8
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_DIAG8_628_1000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('DIAG8');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
--
-- 2X5COR
--
VACUUM ANALYZE regab_prng_pattern_ranges;
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0107_data_pattern_index_frequencies_2X5COR_112_116_292_298_372_378_973_977_80000000000.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);
SELECT regab_update_prob_into_pattern_ranges_from_staging('2X5COR');
TRUNCATE regab_staging_ec_pidx_cnt_tmp;
