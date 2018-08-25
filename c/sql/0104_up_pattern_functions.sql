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
-- Identity transformation.
-- It returns the square set as it is.
--
CREATE FUNCTION square_set_identity_trans (s square_set)
RETURNS square_set
AS $$
BEGIN
  RETURN s;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Mirrors a square set vertically.
-- Row 1 is mapped to row 8 and vice versa.
--
CREATE FUNCTION square_set_flip_horizontal (s square_set)
RETURNS square_set
AS $$
DECLARE
  k1 square_set := (x'ff00000000000000')::BIGINT;
  k2 square_set := (x'00ff000000000000')::BIGINT;
  k3 square_set := (x'0000ff0000000000')::BIGINT;
  k4 square_set := (x'000000ff00000000')::BIGINT;
  k5 square_set := (x'00000000ff000000')::BIGINT;
  k6 square_set := (x'0000000000ff0000')::BIGINT;
  k7 square_set := (x'000000000000ff00')::BIGINT;
  k8 square_set := (x'00000000000000ff')::BIGINT;
  ---
  r square_set;
BEGIN
  r :=     ((s << 56) & k1);
  r := r | ((s << 40) & k2);
  r := r | ((s << 24) & k3);
  r := r | ((s <<  8) & k4);
  r := r | ((s >>  8) & k5);
  r := r | ((s >> 24) & k6);
  r := r | ((s >> 40) & k7);
  r := r | ((s >> 56) & k8);
  RETURN r;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

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

--
-- Flips a square set along the diagonal a1-h8.
-- Square h1 is mapped to a8 and vice versa.
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
-- Flips a square set about the diagonal h1-a8
-- Square a1 is mapped to h8 and vice versa.
--
CREATE FUNCTION square_set_flip_diag_h1a8 (s square_set)
RETURNS square_set
AS $$
DECLARE
  k1 square_set := (x'aa00aa00aa00aa00')::BIGINT;
  k2 square_set := (x'cccc0000cccc0000')::BIGINT;
  k4 square_set := (x'f0f0f0f00f0f0f0f')::BIGINT;
  ---
  r square_set := s;
  u square_set;
BEGIN
  u :=            r # (r::BIT(64) << 36)::BIGINT  ;
  r := r # (k4 & (u # (r::BIT(64) >> 36)::BIGINT));
  u :=      k2 & (r # (r::BIT(64) << 18)::BIGINT) ;
  r := r # (      u # (u::BIT(64) >> 18)::BIGINT );
  u :=      k1 & (r # (r::BIT(64) <<  9)::BIGINT) ;
  r := r # (      u # (u::BIT(64) >>  9)::BIGINT );
  RETURN r;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Rotates 90 degrees anti-clockwise
--
CREATE FUNCTION square_set_rotate_90a (s square_set)
RETURNS square_set
AS $$
BEGIN
  RETURN square_set_flip_diag_h1a8(square_set_flip_horizontal(s));
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Rotates 90 degrees clockwise
--
CREATE FUNCTION square_set_rotate_90c (s square_set)
RETURNS square_set
AS $$
BEGIN
  RETURN square_set_flip_horizontal(square_set_flip_diag_h1a8(s));
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Rotates 180 degrees
--
CREATE FUNCTION square_set_rotate_180 (s square_set)
RETURNS square_set
AS $$
BEGIN
  RETURN square_set_flip_vertical(square_set_flip_horizontal(s));
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- PACK and UNPACK functions.
--

--
-- Packs EDGE pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_edge (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  RETURN (s & s1);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs EDGE pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_edge (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  RETURN s & s1;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

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
-- Packs R2 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_r2 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'000000000000ff00')::BIGINT;
BEGIN
  RETURN (s & s1) >> 8;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs R2 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_r2 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  RETURN (s & s1) << 8;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs R3 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_r3 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000000ff0000')::BIGINT;
BEGIN
  RETURN (s & s1) >> 16;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs R3 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_r3 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  RETURN (s & s1) << 16;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Packs R4 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_r4 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000ff000000')::BIGINT;
BEGIN
  RETURN (s & s1) >> 24;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--
-- Un-packs R4 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_r4 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  RETURN (s & s1) << 24;
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
--
--

--
-- Computes the mover and opponent configurations translated at
-- the origin of the board from the index value.
--
CREATE FUNCTION regab_index_to_transformed_pattern (index_value  INTEGER,
                                                    OUT mover    square_set,
                                                    OUT opponent square_set)
AS $$
DECLARE
  max_pattern_size INTEGER := 12;
  ---
  tmp  INTEGER;
  reminder INTEGER;
  quotient INTEGER;
  cur square_set;
BEGIN
  mover    := 0;
  opponent := 0;
  tmp := index_value;

  FOR i IN 0..max_pattern_size LOOP
    reminder := tmp % 3;
    quotient := tmp / 3;
    cur := 1::square_set << i;
    IF reminder = 2 THEN
      opponent := opponent | cur;
    ELSIF reminder = 1 THEN
      mover := mover | cur;
    ELSE
      NULL;
    END IF;
    tmp := quotient;
  END LOOP;
  
  RETURN;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--- Tests.
DO $$
BEGIN
  PERFORM p_assert(regab_index_to_transformed_pattern(960) = (68::square_set, 26::square_set), 'Expected value is (68, 26).');
  PERFORM p_assert(regab_index_to_transformed_pattern(3280) = (255::square_set, 0::square_set), 'Expected value is (255, 0).');
  PERFORM p_assert(regab_index_to_transformed_pattern(6560) = (0::square_set, 255::square_set), 'Expected value is (0, 255).');
END $$;

--
-- Computes the index value from the mover and opponent configurations translated at
-- the origin of the board.
--
CREATE FUNCTION regab_transformed_pattern_to_index (mover    square_set,
                                                    opponent square_set)
RETURNS INTEGER
AS $$
DECLARE
  max_pattern_size INTEGER := 12; -- !!! Increase the value if a pattern has more than 12 squares.
  index_value      INTEGER :=  0;
  ---
  is_m BOOLEAN;
  is_o BOOLEAN;
  cur  square_set;
  inc  INTEGER;
BEGIN

  FOR i IN 0..max_pattern_size LOOP
    cur := 1::square_set << i;
    is_m := cur & mover    <> 0;
    is_o := cur & opponent <> 0;
    IF is_m AND is_o THEN
      RAISE EXCEPTION 'Mover and opponent have overlapping squares.';
    END IF;
    IF is_m OR is_o THEN
      inc := 3^i;
      IF is_o THEN inc := inc * 2; END IF;
      index_value := index_value + inc;
    END IF;
  END LOOP;
  
  RETURN index_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--- Tests.
DO $$
BEGIN
  PERFORM p_assert(regab_transformed_pattern_to_index(0, 0) = 0, 'Expected value is 0.');
  PERFORM p_assert(regab_transformed_pattern_to_index(255, 0) = 3280, 'Expected value is 3280.');
  PERFORM p_assert(regab_transformed_pattern_to_index(0, 255) = 6560, 'Expected value is 6560.');
END $$;

--
--
--

--
-- Computes the mirror value for the given index, for the EDGE pattern.
-- Patterns EDGE, R2, R3, and R4 have the same mapping.
--
CREATE FUNCTION regab_mirror_value_edge_pattern (index_value INTEGER)
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
  -- not required.
  
  -- step 2: mirror transformation
  mo := square_set_flip_vertical(mo); op := square_set_flip_vertical(op);
  
  -- step 3: instance zero pattern to transformed pattern
  -- not required
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--- Tests.
DO $$
BEGIN
  PERFORM p_assert(regab_mirror_value_edge_pattern(125) = 6183, 'Expected value is 6183.');
END $$;

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

COMMIT;
