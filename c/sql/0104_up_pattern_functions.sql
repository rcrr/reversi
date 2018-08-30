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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_flipped_v  square_set := (x'1e2222120e0a1222')::BIGINT;
  ar_mirror_h   square_set := (x'4448507048444478')::BIGINT;
  ar_flip_h1a8  square_set := (x'00ff888c92610000')::BIGINT;
  ar_flip_a1h8  square_set := (x'000086493111ff00')::BIGINT;
  ar_rotate_180 square_set := (x'7844444870504844')::BIGINT;
  ar_rotate_90c square_set := (x'000061928c88ff00')::BIGINT;
  ar_rotate_90a square_set := (x'00ff113149860000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_identity_trans((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_identity_trans((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_identity_trans((x'0000000000000001')::square_set) = (x'0000000000000001')::square_set, '0000000000000001');
  PERFORM p_assert(square_set_identity_trans((x'8000000000000000')::square_set) = (x'8000000000000000')::square_set, '8000000000000000');
  PERFORM p_assert(square_set_identity_trans(ar) = ar, 'Expected result is ar.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_flipped_v  square_set := (x'1e2222120e0a1222')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_flip_horizontal((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_flip_horizontal((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_flip_horizontal((x'00000000000000ff')::square_set) = (x'ff00000000000000')::square_set, 'ff00000000000000');
  PERFORM p_assert(square_set_flip_horizontal((x'ffffffff00000000')::square_set) = (x'00000000ffffffff')::square_set, '00000000ffffffff');
  PERFORM p_assert(square_set_flip_horizontal((x'0101010101010101')::square_set) = (x'0101010101010101')::square_set, '0101010101010101');
  PERFORM p_assert(square_set_flip_horizontal(ar) = ar_flipped_v, 'Expected result is ar_flipped_v.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_mirror_h   square_set := (x'4448507048444478')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_flip_vertical((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_flip_vertical((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_flip_vertical((x'0000000000000001')::square_set) = (x'0000000000000080')::square_set, '0000000000000080');
  PERFORM p_assert(square_set_flip_vertical(ar) = ar_mirror_h, 'Expected result is ar_mirror_h.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_flip_a1h8  square_set := (x'000086493111ff00')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_flip_diag_a1h8((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_flip_diag_a1h8((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_flip_diag_a1h8((x'0000000000000080')::square_set) = (x'0100000000000000')::square_set, '0100000000000000');
  PERFORM p_assert(square_set_flip_diag_a1h8((x'0100000000000000')::square_set) = (x'0000000000000080')::square_set, '0000000000000080');
  PERFORM p_assert(square_set_flip_diag_a1h8((x'0100000000000080')::square_set) = (x'0100000000000080')::square_set, '0100000000000080');
  PERFORM p_assert(square_set_flip_diag_a1h8(ar) = ar_flip_a1h8, 'Expected result is ar_flip_a1h8.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_flip_h1a8  square_set := (x'00ff888c92610000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_flip_diag_h1a8((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_flip_diag_h1a8((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_flip_diag_h1a8((x'0000000000000001')::square_set) = (x'8000000000000000')::square_set, '8000000000000000');
  PERFORM p_assert(square_set_flip_diag_h1a8((x'8000000000000000')::square_set) = (x'0000000000000001')::square_set, '0000000000000001');
  PERFORM p_assert(square_set_flip_diag_h1a8((x'8000000000000001')::square_set) = (x'8000000000000001')::square_set, '8000000000000001');
  PERFORM p_assert(square_set_flip_diag_h1a8(ar) = ar_flip_h1a8, 'Expected result is ar_flip_h1a8.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_rotate_90a square_set := (x'00ff113149860000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_rotate_90a((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_rotate_90a((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_rotate_90a(ar) = ar_rotate_90a, 'Expected result is ar_rotate_90a.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_rotate_90c square_set := (x'000061928c88ff00')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_rotate_90c((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_rotate_90c((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_rotate_90c(ar) = ar_rotate_90c, 'Expected result is ar_rotate_90c.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  ar            square_set := (x'22120a0e1222221e')::BIGINT;
  ar_rotate_180 square_set := (x'7844444870504844')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_rotate_180((x'0000000000000000')::square_set) = (x'0000000000000000')::square_set, '0000000000000000');
  PERFORM p_assert(square_set_rotate_180((x'ffffffffffffffff')::square_set) = (x'ffffffffffffffff')::square_set, 'ffffffffffffffff');
  PERFORM p_assert(square_set_rotate_180(ar) = ar_rotate_180, 'Expected result is ar_rotate_180.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_      square_set := (x'ffffffffffffffff')::BIGINT;
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_edge(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_edge(full_) = first_row, 'Expected result is first_row.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_edge(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_edge(first_row) = first_row, 'Expected result is first_row.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_              square_set := (x'ffffffffffffffff')::BIGINT;
  empty_             square_set := (x'0000000000000000')::BIGINT;
  nw_corner          square_set := (x'0000000000070707')::BIGINT;
  packed_corner_mask square_set := (x'00000000000001ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_corner(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_corner(full_) = packed_corner_mask, 'Expected result is packed_corner_mask.');
  PERFORM p_assert(square_set_pattern_pack_corner(nw_corner) = packed_corner_mask, 'Expected result is packed_corner_mask.');
  PERFORM p_assert(square_set_pattern_pack_corner((x'0000000000000007')::square_set) = (x'0000000000000007')::square_set, 'Expected result is 0000000000000007.');
  PERFORM p_assert(square_set_pattern_pack_corner((x'0000000000000700')::square_set) = (x'0000000000000038')::square_set, 'Expected result is 0000000000000038.');
  PERFORM p_assert(square_set_pattern_pack_corner((x'0000000000070000')::square_set) = (x'00000000000001c0')::square_set, 'Expected result is 00000000000001c0.');
  PERFORM p_assert(square_set_pattern_pack_corner((x'0000000000040404')::square_set) = (x'0000000000000124')::square_set, 'Expected result is 0000000000000124.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_              square_set := (x'ffffffffffffffff')::BIGINT;
  empty_             square_set := (x'0000000000000000')::BIGINT;
  nw_corner          square_set := (x'0000000000070707')::BIGINT;
  packed_corner_mask square_set := (x'00000000000001ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_corner(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_corner(packed_corner_mask) = nw_corner, 'Expected result is nw_corner.');
  PERFORM p_assert(square_set_pattern_unpack_corner((x'0000000000000007')::square_set) = (x'0000000000000007')::square_set, 'Expected result is 0000000000000007.');
  PERFORM p_assert(square_set_pattern_unpack_corner((x'0000000000000038')::square_set) = (x'0000000000000700')::square_set, 'Expected result is 0000000000000700.');
  PERFORM p_assert(square_set_pattern_unpack_corner((x'00000000000001c0')::square_set) = (x'0000000000070000')::square_set, 'Expected result is 0000000000070000.');
  PERFORM p_assert(square_set_pattern_unpack_corner((x'0000000000000124')::square_set) = (x'0000000000040404')::square_set, 'Expected result is 0000000000040404.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  n_xedge           square_set := (x'00000000000042ff')::BIGINT;
  packed_xedge_mask square_set := (x'00000000000003ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_xedge(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_xedge(full_) = packed_xedge_mask, 'Expected result is packed_xedge_mask.');
  PERFORM p_assert(square_set_pattern_pack_xedge(n_xedge) = packed_xedge_mask, 'Expected result is packed_xedge_mask.');
  PERFORM p_assert(square_set_pattern_pack_xedge((x'00000000000000ff')::square_set) = (x'00000000000000ff')::square_set, 'Expected result is 00000000000000ff.');
  PERFORM p_assert(square_set_pattern_pack_xedge((x'000000000000ff00')::square_set) = (x'0000000000000300')::square_set, 'Expected result is 0000000000000300.');
  PERFORM p_assert(square_set_pattern_pack_xedge((x'0000000000000200')::square_set) = (x'0000000000000100')::square_set, 'Expected result is 0000000000000100.');
  PERFORM p_assert(square_set_pattern_pack_xedge((x'0000000000004000')::square_set) = (x'0000000000000200')::square_set, 'Expected result is 0000000000000200.');
  PERFORM p_assert(square_set_pattern_pack_xedge((x'ffffffffffffbd00')::square_set) = empty_, 'Expected result is empty_');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  n_xedge           square_set := (x'00000000000042ff')::BIGINT;
  packed_xedge_mask square_set := (x'00000000000003ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_xedge(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_xedge(packed_xedge_mask) = n_xedge, 'Expected result is n_xedge.');
  PERFORM p_assert(square_set_pattern_unpack_xedge((x'00000000000000ff')::square_set) = (x'00000000000000ff')::square_set, 'Expected result is 00000000000000ff.');
  PERFORM p_assert(square_set_pattern_unpack_xedge((x'0000000000000300')::square_set) = (x'0000000000004200')::square_set, 'Expected result is 0000000000004200.');
  PERFORM p_assert(square_set_pattern_unpack_xedge((x'0000000000000100')::square_set) = (x'0000000000000200')::square_set, 'Expected result is 0000000000000200.');
  PERFORM p_assert(square_set_pattern_unpack_xedge((x'0000000000000200')::square_set) = (x'0000000000004000')::square_set, 'Expected result is 0000000000004000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_      square_set := (x'ffffffffffffffff')::BIGINT;
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
  second_row square_set := (x'000000000000ff00')::BIGINT;
  third_row  square_set := (x'0000000000ff0000')::BIGINT;
  fourth_row square_set := (x'00000000ff000000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_r2(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r2(full_) = first_row, 'Expected result is first_row.');
  PERFORM p_assert(square_set_pattern_pack_r2(first_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r2(second_row) = first_row, 'Expected result is first_row.');
  PERFORM p_assert(square_set_pattern_pack_r2(third_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r2(fourth_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r2((x'0101010101010101')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_r2((x'8080808080808080')::square_set) = (x'0000000000000080')::square_set, 'Expected result is 0000000000000080.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
  second_row square_set := (x'000000000000ff00')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_r2(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_r2(first_row) = second_row, 'Expected result is second_row.');
  PERFORM p_assert(square_set_pattern_unpack_r2((x'0000000000000001')::square_set) = (x'0000000000000100')::square_set, 'Expected result is 0000000000000100.');
  PERFORM p_assert(square_set_pattern_unpack_r2((x'0000000000000080')::square_set) = (x'0000000000008000')::square_set, 'Expected result is 0000000000008000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_      square_set := (x'ffffffffffffffff')::BIGINT;
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
  second_row square_set := (x'000000000000ff00')::BIGINT;
  third_row  square_set := (x'0000000000ff0000')::BIGINT;
  fourth_row square_set := (x'00000000ff000000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_r3(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r3(full_) = first_row, 'Expected result is first_row.');
  PERFORM p_assert(square_set_pattern_pack_r3(first_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r3(second_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r3(third_row) = first_row, 'Expected result is first_row.');
  PERFORM p_assert(square_set_pattern_pack_r3(fourth_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r3((x'0101010101010101')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_r3((x'8080808080808080')::square_set) = (x'0000000000000080')::square_set, 'Expected result is 0000000000000080.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
  third_row  square_set := (x'0000000000ff0000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_r3(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_r3(first_row) = third_row, 'Expected result is third_row.');
  PERFORM p_assert(square_set_pattern_unpack_r3((x'0000000000000001')::square_set) = (x'0000000000010000')::square_set, 'Expected result is 0000000000010000.');
  PERFORM p_assert(square_set_pattern_unpack_r3((x'0000000000000080')::square_set) = (x'0000000000800000')::square_set, 'Expected result is 0000000000800000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_      square_set := (x'ffffffffffffffff')::BIGINT;
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
  second_row square_set := (x'000000000000ff00')::BIGINT;
  third_row  square_set := (x'0000000000ff0000')::BIGINT;
  fourth_row square_set := (x'00000000ff000000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_r4(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r4(full_) = first_row, 'Expected result is first_row.');
  PERFORM p_assert(square_set_pattern_pack_r4(first_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r4(second_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r4(third_row) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_r4(fourth_row) = first_row, 'Expected result is first_row.');
  PERFORM p_assert(square_set_pattern_pack_r4((x'0101010101010101')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_r4((x'8080808080808080')::square_set) = (x'0000000000000080')::square_set, 'Expected result is 0000000000000080.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  empty_     square_set := (x'0000000000000000')::BIGINT;
  first_row  square_set := (x'00000000000000ff')::BIGINT;
  fourth_row square_set := (x'00000000ff000000')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_r4(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_r4(first_row) = fourth_row, 'Expected result is fourth_row.');
  PERFORM p_assert(square_set_pattern_unpack_r4((x'0000000000000001')::square_set) = (x'0000000001000000')::square_set, 'Expected result is 0000000001000000.');
  PERFORM p_assert(square_set_pattern_unpack_r4((x'0000000000000080')::square_set) = (x'0000000080000000')::square_set, 'Expected result is 0000000080000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag4_mask square_set := (x'000000000000000f')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_diag4(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_diag4(full_) = packed_diag4_mask, 'Expected result is packed_diag4_mask.');
  PERFORM p_assert(square_set_pattern_pack_diag4((x'0000000000000008')::square_set) = (x'0000000000000008')::square_set, 'Expected result is 0000000000000008.');
  PERFORM p_assert(square_set_pattern_pack_diag4((x'0000000000000400')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_pack_diag4((x'0000000000020000')::square_set) = (x'0000000000000002')::square_set, 'Expected result is 0000000000000002.');
  PERFORM p_assert(square_set_pattern_pack_diag4((x'0000000001000000')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_diag4((x'fffffffffefdfbf7')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag4_mask square_set := (x'000000000000000f')::BIGINT;
  diag4             square_set := (x'0000000001020408')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_diag4(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_diag4(packed_diag4_mask) = diag4, 'Expected result is diag4.');
  PERFORM p_assert(square_set_pattern_unpack_diag4((x'0000000000000008')::square_set) = (x'0000000000000008')::square_set, 'Expected result is 0000000000000008.');
  PERFORM p_assert(square_set_pattern_unpack_diag4((x'0000000000000004')::square_set) = (x'0000000000000400')::square_set, 'Expected result is 0000000000000400.');
  PERFORM p_assert(square_set_pattern_unpack_diag4((x'0000000000000002')::square_set) = (x'0000000000020000')::square_set, 'Expected result is 0000000000020000.');
  PERFORM p_assert(square_set_pattern_unpack_diag4((x'0000000000000001')::square_set) = (x'0000000001000000')::square_set, 'Expected result is 0000000001000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag5_mask square_set := (x'000000000000001f')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_diag5(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_diag5(full_) = packed_diag5_mask, 'Expected result is packed_diag5_mask.');
  PERFORM p_assert(square_set_pattern_pack_diag5((x'0000000000000010')::square_set) = (x'0000000000000010')::square_set, 'Expected result is 0000000000000010.');
  PERFORM p_assert(square_set_pattern_pack_diag5((x'0000000000000800')::square_set) = (x'0000000000000008')::square_set, 'Expected result is 0000000000000008.');
  PERFORM p_assert(square_set_pattern_pack_diag5((x'0000000000040000')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_pack_diag5((x'0000000002000000')::square_set) = (x'0000000000000002')::square_set, 'Expected result is 0000000000000002.');
  PERFORM p_assert(square_set_pattern_pack_diag5((x'0000000100000000')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_diag5((x'fffffffefdfbf7ef')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag5_mask square_set := (x'000000000000001f')::BIGINT;
  diag5             square_set := (x'0000000102040810')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_diag5(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_diag5(packed_diag5_mask) = diag5, 'Expected result is diag5.');
  PERFORM p_assert(square_set_pattern_unpack_diag5((x'0000000000000010')::square_set) = (x'0000000000000010')::square_set, 'Expected result is 0000000000000010.');
  PERFORM p_assert(square_set_pattern_unpack_diag5((x'0000000000000008')::square_set) = (x'0000000000000800')::square_set, 'Expected result is 0000000000000800.');
  PERFORM p_assert(square_set_pattern_unpack_diag5((x'0000000000000004')::square_set) = (x'0000000000040000')::square_set, 'Expected result is 0000000000040000.');
  PERFORM p_assert(square_set_pattern_unpack_diag5((x'0000000000000002')::square_set) = (x'0000000002000000')::square_set, 'Expected result is 0000000002000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag5((x'0000000000000001')::square_set) = (x'0000000100000000')::square_set, 'Expected result is 0000000100000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag6_mask square_set := (x'000000000000003f')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_diag6(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_diag6(full_) = packed_diag6_mask, 'Expected result is packed_diag6_mask.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'0000000000000020')::square_set) = (x'0000000000000020')::square_set, 'Expected result is 0000000000000020.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'0000000000001000')::square_set) = (x'0000000000000010')::square_set, 'Expected result is 0000000000000010.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'0000000000080000')::square_set) = (x'0000000000000008')::square_set, 'Expected result is 0000000000000008.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'0000000004000000')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'0000000200000000')::square_set) = (x'0000000000000002')::square_set, 'Expected result is 0000000000000002.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'0000010000000000')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_diag6((x'fffffefdfbf7efdf')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag6_mask square_set := (x'000000000000003f')::BIGINT;
  diag6             square_set := (x'0000010204081020')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_diag6(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_diag6(packed_diag6_mask) = diag6, 'Expected result is diag6.');
  PERFORM p_assert(square_set_pattern_unpack_diag6((x'0000000000000020')::square_set) = (x'0000000000000020')::square_set, 'Expected result is 0000000000000020.');
  PERFORM p_assert(square_set_pattern_unpack_diag6((x'0000000000000010')::square_set) = (x'0000000000001000')::square_set, 'Expected result is 0000000000001000.');
  PERFORM p_assert(square_set_pattern_unpack_diag6((x'0000000000000008')::square_set) = (x'0000000000080000')::square_set, 'Expected result is 0000000000080000.');
  PERFORM p_assert(square_set_pattern_unpack_diag6((x'0000000000000004')::square_set) = (x'0000000004000000')::square_set, 'Expected result is 0000000004000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag6((x'0000000000000002')::square_set) = (x'0000000200000000')::square_set, 'Expected result is 0000000200000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag6((x'0000000000000001')::square_set) = (x'0000010000000000')::square_set, 'Expected result is 0000010000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag7_mask square_set := (x'000000000000007f')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_diag7(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_diag7(full_) = packed_diag7_mask, 'Expected result is packed_diag7_mask.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0000000000000040')::square_set) = (x'0000000000000040')::square_set, 'Expected result is 0000000000000040.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0000000000002000')::square_set) = (x'0000000000000020')::square_set, 'Expected result is 0000000000000020.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0000000000100000')::square_set) = (x'0000000000000010')::square_set, 'Expected result is 0000000000000010.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0000000008000000')::square_set) = (x'0000000000000008')::square_set, 'Expected result is 0000000000000008.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0000000400000000')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0000020000000000')::square_set) = (x'0000000000000002')::square_set, 'Expected result is 0000000000000002.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'0001000000000000')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_diag7((x'fffefdfbf7efdfbf')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag7_mask square_set := (x'000000000000007f')::BIGINT;
  diag7             square_set := (x'0001020408102040')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_diag7(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_diag7(packed_diag7_mask) = diag7, 'Expected result is diag7.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000040')::square_set) = (x'0000000000000040')::square_set, 'Expected result is 0000000000000040.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000020')::square_set) = (x'0000000000002000')::square_set, 'Expected result is 0000000000002000.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000010')::square_set) = (x'0000000000100000')::square_set, 'Expected result is 0000000000100000.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000008')::square_set) = (x'0000000008000000')::square_set, 'Expected result is 0000000008000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000004')::square_set) = (x'0000000400000000')::square_set, 'Expected result is 0000000400000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000002')::square_set) = (x'0000020000000000')::square_set, 'Expected result is 0000020000000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag7((x'0000000000000001')::square_set) = (x'0001000000000000')::square_set, 'Expected result is 0001000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag8_mask square_set := (x'00000000000000ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_diag8(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_diag8(full_) = packed_diag8_mask, 'Expected result is packed_diag8_mask.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0000000000000080')::square_set) = (x'0000000000000080')::square_set, 'Expected result is 0000000000000080.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0000000000004000')::square_set) = (x'0000000000000040')::square_set, 'Expected result is 0000000000000040.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0000000000200000')::square_set) = (x'0000000000000020')::square_set, 'Expected result is 0000000000000020.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0000000010000000')::square_set) = (x'0000000000000010')::square_set, 'Expected result is 0000000000000010.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0000000800000000')::square_set) = (x'0000000000000008')::square_set, 'Expected result is 0000000000000008.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0000040000000000')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0002000000000000')::square_set) = (x'0000000000000002')::square_set, 'Expected result is 0000000000000002.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'0100000000000000')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_diag8((x'fefdfbf7efdfbf7f')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_             square_set := (x'ffffffffffffffff')::BIGINT;
  empty_            square_set := (x'0000000000000000')::BIGINT;
  packed_diag8_mask square_set := (x'00000000000000ff')::BIGINT;
  diag8             square_set := (x'0102040810204080')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_diag8(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_diag8(packed_diag8_mask) = diag8, 'Expected result is diag8.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000080')::square_set) = (x'0000000000000080')::square_set, 'Expected result is 0000000000000080.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000040')::square_set) = (x'0000000000004000')::square_set, 'Expected result is 0000000000004000.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000020')::square_set) = (x'0000000000200000')::square_set, 'Expected result is 0000000000200000.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000010')::square_set) = (x'0000000010000000')::square_set, 'Expected result is 0000000010000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000008')::square_set) = (x'0000000800000000')::square_set, 'Expected result is 0000000800000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000004')::square_set) = (x'0000040000000000')::square_set, 'Expected result is 0000040000000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000002')::square_set) = (x'0002000000000000')::square_set, 'Expected result is 0002000000000000.');
  PERFORM p_assert(square_set_pattern_unpack_diag8((x'0000000000000001')::square_set) = (x'0100000000000000')::square_set, 'Expected result is 0100000000000000.');
END $$;

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
--- Tests ...
DO $$
DECLARE
  full_              square_set := (x'ffffffffffffffff')::BIGINT;
  empty_             square_set := (x'0000000000000000')::BIGINT;
  nw_2x5cor          square_set := (x'0000000000001f1f')::BIGINT;
  packed_2x5cor_mask square_set := (x'00000000000003ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_2x5cor(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_2x5cor(full_) = packed_2x5cor_mask, 'Expected result is packed_2x5cor_mask.');
  PERFORM p_assert(square_set_pattern_pack_2x5cor(nw_2x5cor) = packed_2x5cor_mask, 'Expected result is packed_2x5cor_mask.');
  PERFORM p_assert(square_set_pattern_pack_2x5cor((x'000000000000001f')::square_set) = (x'000000000000001f')::square_set, 'Expected result is 000000000000001f.');
  PERFORM p_assert(square_set_pattern_pack_2x5cor((x'0000000000001f00')::square_set) = (x'00000000000003e0')::square_set, 'Expected result is 00000000000003e0.');
  PERFORM p_assert(square_set_pattern_pack_2x5cor((x'ffffffffffffe0e0')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

--
-- Un-packs 2X5COR pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_2x5cor (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'000000000000001f')::BIGINT;
  s2 square_set := (x'00000000000003e0')::BIGINT;
BEGIN
  RETURN (s & s1) | ((s & s2) << 3);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
--- Tests ...
DO $$
DECLARE
  full_              square_set := (x'ffffffffffffffff')::BIGINT;
  empty_             square_set := (x'0000000000000000')::BIGINT;
  nw_2x5cor          square_set := (x'0000000000001f1f')::BIGINT;
  packed_2x5cor_mask square_set := (x'00000000000003ff')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_2x5cor(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_2x5cor(packed_2x5cor_mask) = nw_2x5cor, 'Expected result is nw_2x5cor.');
  PERFORM p_assert(square_set_pattern_unpack_2x5cor((x'000000000000001f')::square_set) = (x'000000000000001f')::square_set, 'Expected result is 000000000000001f.');
  PERFORM p_assert(square_set_pattern_unpack_2x5cor((x'00000000000003e0')::square_set) = (x'0000000000001f00')::square_set, 'Expected result is 0000000000001f00.');
END $$;

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
-- MIRROR functions.
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
DECLARE
  n INTEGER := 3^8 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_edge_pattern( 125) = 6183, 'Expected value is 6183.');
  PERFORM p_assert(regab_mirror_value_edge_pattern(6183) =  125, 'Expected value is  125.');
  PERFORM p_assert(regab_mirror_value_edge_pattern( 702) =  234, 'Expected value is  234.');
  PERFORM p_assert(regab_mirror_value_edge_pattern( 234) =  702, 'Expected value is  702.');
  PERFORM p_assert(regab_mirror_value_edge_pattern(   0) =    0, 'Expected value is    0.');
  PERFORM p_assert(regab_mirror_value_edge_pattern(3280) = 3280, 'Expected value is 3280.');
  PERFORM p_assert(regab_mirror_value_edge_pattern(6560) = 6560, 'Expected value is 6560.');
  PERFORM p_assert(regab_mirror_value_edge_pattern(4372) = 4372, 'Expected value is 4372.');
  PERFORM p_assert(regab_mirror_value_edge_pattern(5602) = 3182, 'Expected value is 3182.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_edge_pattern(regab_mirror_value_edge_pattern(i)) = i, 'Comuputing mirror of mirror of an edge pattern index should return itself');
  END LOOP;
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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^9 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_corner_pattern(    0) =     0, 'Expected value is     0.');
  PERFORM p_assert(regab_mirror_value_corner_pattern( 9841) =  9841, 'Expected value is  9841.');
  PERFORM p_assert(regab_mirror_value_corner_pattern(19682) = 19682, 'Expected value is 19682.');
  PERFORM p_assert(regab_mirror_value_corner_pattern( 7522) =  7522, 'Expected value is  7522.');
  PERFORM p_assert(regab_mirror_value_corner_pattern(    9) =   729, 'Expected value is   729.');
  PERFORM p_assert(regab_mirror_value_corner_pattern(   18) =  1458, 'Expected value is  1458.');
  PERFORM p_assert(regab_mirror_value_corner_pattern(    3) =    27, 'Expected value is    27.');
  PERFORM p_assert(regab_mirror_value_corner_pattern(  243) =  2187, 'Expected value is  2187.');
  PERFORM p_assert(regab_mirror_value_corner_pattern(13796) = 19172, 'Expected value is 19172.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_corner_pattern(regab_mirror_value_corner_pattern(i)) = i, 'Comuputing mirror of mirror of a corner pattern index should return itself');
  END LOOP;
END $$;

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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^10 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_xedge_pattern(    0) =     0, 'Expected value is     0.');
  PERFORM p_assert(regab_mirror_value_xedge_pattern(29524) = 29524, 'Expected value is 29524.');
  PERFORM p_assert(regab_mirror_value_xedge_pattern(59048) = 59048, 'Expected value is 59048.');
  PERFORM p_assert(regab_mirror_value_xedge_pattern(52447) = 36125, 'Expected value is 36125.');
  PERFORM p_assert(regab_mirror_value_xedge_pattern( 6561) = 19683, 'Expected value is 19683.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_xedge_pattern(regab_mirror_value_xedge_pattern(i)) = i, 'Comuputing mirror of mirror of a xedge pattern index should return itself');
  END LOOP;
END $$;

--
-- Computes the mirror value for the given index, for the R2 pattern.
--
CREATE FUNCTION regab_mirror_value_r2_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
BEGIN
  RETURN regab_mirror_value_edge_pattern(index_value);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^8 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_r2_pattern( 125) = 6183, 'Expected value is 6183.');
  PERFORM p_assert(regab_mirror_value_r2_pattern(6183) =  125, 'Expected value is  125.');
  PERFORM p_assert(regab_mirror_value_r2_pattern( 702) =  234, 'Expected value is  234.');
  PERFORM p_assert(regab_mirror_value_r2_pattern( 234) =  702, 'Expected value is  702.');
  PERFORM p_assert(regab_mirror_value_r2_pattern(   0) =    0, 'Expected value is    0.');
  PERFORM p_assert(regab_mirror_value_r2_pattern(3280) = 3280, 'Expected value is 3280.');
  PERFORM p_assert(regab_mirror_value_r2_pattern(6560) = 6560, 'Expected value is 6560.');
  PERFORM p_assert(regab_mirror_value_r2_pattern(4372) = 4372, 'Expected value is 4372.');
  PERFORM p_assert(regab_mirror_value_r2_pattern(5602) = 3182, 'Expected value is 3182.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_r2_pattern(regab_mirror_value_r2_pattern(i)) = i, 'Comuputing mirror of mirror of an r2 pattern index should return itself');
  END LOOP;
END $$;

--
-- See function documenattion for EDGE pattern.
--
CREATE FUNCTION regab_mirror_value_r3_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
BEGIN
  RETURN regab_mirror_value_edge_pattern(index_value);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
--- Tests.
DO $$
BEGIN
  PERFORM p_assert(regab_mirror_value_r3_pattern(64) = 2592, 'Expected value is 2592.');
END $$;

--
-- See function documenattion for EDGE pattern.
--
CREATE FUNCTION regab_mirror_value_r4_pattern (index_value INTEGER)
RETURNS INTEGER
AS $$
BEGIN
  RETURN regab_mirror_value_edge_pattern(index_value);
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
--- Tests.
DO $$
BEGIN
  PERFORM p_assert(regab_mirror_value_r4_pattern(2244) = 892, 'Expected value is 892.');
END $$;

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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^4 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_diag4_pattern( 0) =  0, 'Expected value is  0.');
  PERFORM p_assert(regab_mirror_value_diag4_pattern(40) = 40, 'Expected value is 40.');
  PERFORM p_assert(regab_mirror_value_diag4_pattern(80) = 80, 'Expected value is 80.');
  PERFORM p_assert(regab_mirror_value_diag4_pattern(53) = 79, 'Expected value is 79.');
  PERFORM p_assert(regab_mirror_value_diag4_pattern( 7) = 45, 'Expected value is 45.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_diag4_pattern(regab_mirror_value_diag4_pattern(i)) = i, 'Comuputing mirror of mirror of an diag4 pattern index should return itself');
  END LOOP;
END $$;

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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^5 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_diag5_pattern(  0) =   0, 'Expected value is   0.');
  PERFORM p_assert(regab_mirror_value_diag5_pattern(121) = 121, 'Expected value is 121.');
  PERFORM p_assert(regab_mirror_value_diag5_pattern(242) = 242, 'Expected value is 242.');
  PERFORM p_assert(regab_mirror_value_diag5_pattern( 15) =  63, 'Expected value is  63.');
  PERFORM p_assert(regab_mirror_value_diag5_pattern(  7) = 135, 'Expected value is 135.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_diag5_pattern(regab_mirror_value_diag5_pattern(i)) = i, 'Comuputing mirror of mirror of an diag5 pattern index should return itself');
  END LOOP;
END $$;

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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^6 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_diag6_pattern(  0) =   0, 'Expected value is   0.');
  PERFORM p_assert(regab_mirror_value_diag6_pattern(364) = 364, 'Expected value is 364.');
  PERFORM p_assert(regab_mirror_value_diag6_pattern(728) = 728, 'Expected value is 728.');
  PERFORM p_assert(regab_mirror_value_diag6_pattern(715) = 377, 'Expected value is 377.');
  PERFORM p_assert(regab_mirror_value_diag6_pattern(  7) = 405, 'Expected value is 405.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_diag6_pattern(regab_mirror_value_diag6_pattern(i)) = i, 'Comuputing mirror of mirror of an diag6 pattern index should return itself');
  END LOOP;
END $$;

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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^7 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_diag7_pattern(   0) =    0, 'Expected value is    0.');
  PERFORM p_assert(regab_mirror_value_diag7_pattern(1093) = 1093, 'Expected value is 1093.');
  PERFORM p_assert(regab_mirror_value_diag7_pattern(2186) = 2186, 'Expected value is 2186.');
  PERFORM p_assert(regab_mirror_value_diag7_pattern(2173) = 1133, 'Expected value is 1133.');
  PERFORM p_assert(regab_mirror_value_diag7_pattern(   7) = 1215, 'Expected value is 1215.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_diag7_pattern(regab_mirror_value_diag7_pattern(i)) = i, 'Comuputing mirror of mirror of an diag7 pattern index should return itself');
  END LOOP;
END $$;

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
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^8 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_diag8_pattern(   0) =    0, 'Expected value is    0.');
  PERFORM p_assert(regab_mirror_value_diag8_pattern(3280) = 3280, 'Expected value is 3280.');
  PERFORM p_assert(regab_mirror_value_diag8_pattern(6560) = 6560, 'Expected value is 6560.');
  PERFORM p_assert(regab_mirror_value_diag8_pattern(6520) = 3320, 'Expected value is 3320.');
  PERFORM p_assert(regab_mirror_value_diag8_pattern(   7) = 3645, 'Expected value is 3645.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_diag8_pattern(regab_mirror_value_diag8_pattern(i)) = i, 'Comuputing mirror of mirror of an diag8 pattern index should return itself');
  END LOOP;
END $$;

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
--- Tests.
DO $$
BEGIN
  PERFORM p_assert(regab_mirror_value_2x5cor_pattern(0) IS NULL, 'Expected value is NULL.');
END $$;

COMMIT;
