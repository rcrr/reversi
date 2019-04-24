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
VALUES (0104, now(), 'pattern_functions', 'adds square set flips and mirrors, as well as pack and unpack functions');

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
-- Packs DIAG3 pattern.
-- Transforms instance zero pattern to packed pattern.
--
CREATE FUNCTION square_set_pattern_pack_diag3 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000000010204')::BIGINT;
  s2 square_set := (x'0000000000000007')::BIGINT;
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
  packed_diag3_mask square_set := (x'0000000000000007')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_pack_diag3(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_pack_diag3(full_) = packed_diag3_mask, 'Expected result is packed_diag4_mask.');
  PERFORM p_assert(square_set_pattern_pack_diag3((x'0000000000000004')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_pack_diag3((x'0000000000000200')::square_set) = (x'0000000000000002')::square_set, 'Expected result is 0000000000000002.');
  PERFORM p_assert(square_set_pattern_pack_diag3((x'0000000000010000')::square_set) = (x'0000000000000001')::square_set, 'Expected result is 0000000000000001.');
  PERFORM p_assert(square_set_pattern_pack_diag3((x'fffffffffffefdfb')::square_set) = (x'0000000000000000')::square_set, 'Expected result is 0000000000000000.');
END $$;

--
-- Un-packs DIAG3 pattern.
-- Transforms packed pattern to instance zero pattern.
--
CREATE FUNCTION square_set_pattern_unpack_diag3 (s square_set)
RETURNS square_set
AS $$
DECLARE
  s1 square_set := (x'0000000000010204')::BIGINT;
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
  packed_diag3_mask square_set := (x'0000000000000007')::BIGINT;
  diag3             square_set := (x'0000000000010204')::BIGINT;
BEGIN
  PERFORM p_assert(square_set_pattern_unpack_diag3(empty_) = empty_, 'Expected result is empty_.');
  PERFORM p_assert(square_set_pattern_unpack_diag3(packed_diag3_mask) = diag3, 'Expected result is diag3.');
  PERFORM p_assert(square_set_pattern_unpack_diag3((x'0000000000000004')::square_set) = (x'0000000000000004')::square_set, 'Expected result is 0000000000000004.');
  PERFORM p_assert(square_set_pattern_unpack_diag3((x'0000000000000002')::square_set) = (x'0000000000000200')::square_set, 'Expected result is 0000000000000200.');
  PERFORM p_assert(square_set_pattern_unpack_diag3((x'0000000000000001')::square_set) = (x'0000000000010000')::square_set, 'Expected result is 0000000000010000.');
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

--
-- Computes the mirror value for the given index, for the DIAG3 pattern.
--
CREATE FUNCTION regab_mirror_value_diag3_pattern (index_value INTEGER)
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
  mo := square_set_pattern_unpack_diag3(mo);
  op := square_set_pattern_unpack_diag3(op);
  
  -- step 2: mirror transformation
  mo := square_set_flip_diag_a1h8(mo); op := square_set_flip_diag_a1h8(op);
  
  -- step 3: instance zero pattern to transformed pattern
  mo := square_set_pattern_pack_diag3(mo);
  op := square_set_pattern_pack_diag3(op);
  
  -- step 4: transformed pattern to index
  mirror_value := regab_transformed_pattern_to_index(mo, op);

  RETURN mirror_value;
END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;
--- Tests.
DO $$
DECLARE
  n INTEGER := 3^3 - 1;
BEGIN
  PERFORM p_assert(regab_mirror_value_diag3_pattern( 0) =  0, 'Expected value is  0.');
  PERFORM p_assert(regab_mirror_value_diag3_pattern(13) = 13, 'Expected value is 13.');
  PERFORM p_assert(regab_mirror_value_diag3_pattern(26) = 26, 'Expected value is 26.');
  PERFORM p_assert(regab_mirror_value_diag3_pattern(19) = 11, 'Expected value is 11.');
  PERFORM p_assert(regab_mirror_value_diag3_pattern(15) =  7, 'Expected value is  7.');
  FOR i IN 0..n LOOP
    --RAISE NOTICE 'i=%', i;
    PERFORM p_assert(regab_mirror_value_diag3_pattern(regab_mirror_value_diag3_pattern(i)) = i, 'Comuputing mirror of mirror of an diag3 pattern index should return itself');
  END LOOP;
END $$;

--
--
--

--
-- Populates the table regab_prng_pattern_ranges with the data appropriate for the function argument.
--
CREATE FUNCTION ragab_populate_pattern_ranges (pattern_name_arg CHAR(6))
RETURNS INTEGER
AS $$
DECLARE
  nrecords INTEGER;
  pattern_rec RECORD;
  index_count INTEGER;
BEGIN
  nrecords := 0;
  SELECT INTO pattern_rec pattern_id, pattern_name, nsquares, ninstances FROM regab_prng_patterns WHERE pattern_name = pattern_name_arg;
  IF pattern_rec.pattern_id IS NULL THEN
    RAISE EXCEPTION 'Pattern record in table regab_prng_patterns has not been found.';
  END IF;
  index_count := 3^pattern_rec.nsquares;

  WITH RECURSIVE index_value_range AS (
    SELECT
      0::INTEGER AS val
    UNION ALL SELECT val + 1::INTEGER AS val
    FROM
      index_value_range
    WHERE
      index_value_range.val < index_count - 1
  ), pattern_range_key AS (
    SELECT val AS index_value FROM index_value_range
  )
  INSERT INTO regab_prng_pattern_ranges (pattern_id, index_value)
    SELECT pattern_rec.pattern_id, index_value FROM pattern_range_key;

  SELECT count(1) INTO nrecords FROM regab_prng_pattern_ranges WHERE pattern_id = pattern_rec.pattern_id;
  
  RETURN nrecords;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Populates the table regab_prng_pattern_ranges with the data appropriate for the function argument.
--
CREATE FUNCTION ragab_populate_pattern_probs (pattern_name_arg CHAR(6))
RETURNS INTEGER
AS $$
DECLARE
  nrecords INTEGER;
  pattern_rec RECORD;
BEGIN
  nrecords := 0;
  SELECT INTO pattern_rec pattern_id, pattern_name, nsquares, ninstances FROM regab_prng_patterns WHERE pattern_name = pattern_name_arg;
  IF pattern_rec.pattern_id IS NULL THEN
    RAISE EXCEPTION 'Pattern record in table regab_prng_patterns has not been found.';
  END IF;

  WITH RECURSIVE empty_count_range AS (
    SELECT
      0::INTEGER AS val
    UNION ALL SELECT val + 1::INTEGER AS val
    FROM
      empty_count_range
    WHERE
      empty_count_range.val < 60
  ), pattern_ranges AS (
    SELECT
      seq AS range_id
    FROM
      regab_prng_pattern_ranges
    WHERE
      pattern_id = pattern_rec.pattern_id
  ), pattern_prob_key AS (
    SELECT
      pr.range_id AS range_id,
      ecr.val AS empty_count
    FROM
      pattern_ranges AS pr
    CROSS JOIN
      empty_count_range AS ecr
  )
  INSERT INTO regab_prng_pattern_probs (range_id, empty_count)
    SELECT range_id, empty_count FROM pattern_prob_key;

  SELECT count(1) INTO nrecords FROM regab_prng_pattern_probs WHERE range_id IN (SELECT seq FROM regab_prng_pattern_ranges WHERE pattern_id = pattern_rec.pattern_id);
  
  RETURN nrecords;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
--
--

--
-- Populates the index_prob_given_ec field in table regab_prng_pattern_probs taking data from
-- the staging table regab_staging_ec_pidx_cnt_tmp (empty_count, index_value, frequency).
--
-- Checks that the pattern_name_arg is an entry in the regab_prng_patterns table.
-- Checks that there are the expected number of records belonging to the given pattern in table regab_prng_pattern_probs.
-- Checks that there are the expected number of records in the staging table.
--
CREATE FUNCTION regab_update_prob_into_pattern_probs_from_staging (pattern_name_arg CHARACTER(6))
RETURNS INTEGER
AS $$
DECLARE
  pid INTEGER;
  ni SMALLINT;
  ns SMALLINT;
  nrec_expected BIGINT;
  nrec_counted_in_table BIGINT;
  nrec_counted_in_staging BIGINT;
BEGIN

  SELECT pattern_id, ninstances, nsquares INTO pid, ni, ns
    FROM regab_prng_patterns WHERE pattern_name = pattern_name_arg;
  IF pid IS NULL THEN
    RAISE EXCEPTION 'Record not found in table regab_prng_patterns matching pattern_name "%".',
      pattern_name_arg;
  END IF;

  SELECT 3^ns*61 INTO nrec_expected;
  SELECT count(1) INTO nrec_counted_in_table FROM regab_prng_pattern_probs
    WHERE range_id IN (SELECT seq FROM regab_prng_pattern_ranges WHERE pattern_id = pid);
  SELECT count(1) INTO nrec_counted_in_staging FROM regab_staging_ec_pidx_cnt_tmp;

  IF nrec_counted_in_table <> nrec_expected THEN
    RAISE EXCEPTION 'The number of record belonging to the "%" pattern must be %, found %.',
      pattern_name_arg, nrec_expected, nrec_counted_in_table;
  END IF;

  IF nrec_counted_in_staging <> nrec_expected THEN
    RAISE EXCEPTION 'The number of record found in the staging table must be %, found %.',
      nrec_expected, nrec_counted_in_staging;
  END IF;
  
  WITH freq_totals_by_ec AS (
    SELECT empty_count, sum(frequency) AS cnt
    FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count
  ), frequencies AS (
    SELECT empty_count, index_value, frequency AS cnt
    FROM regab_staging_ec_pidx_cnt_tmp
  ), probabilities AS (
    SELECT
      f.empty_count AS empty_count,
      f.index_value AS index_value,
      f.cnt / ft.cnt AS probability
    FROM
      freq_totals_by_ec AS ft
    LEFT JOIN
      frequencies AS f ON f.empty_count = ft.empty_count
  ), pattern_ranges AS (
    SELECT
      seq AS prid, index_value FROM regab_prng_pattern_ranges WHERE pattern_id = pid
  ), update_table AS (
    SELECT
      pr.empty_count AS empty_count,
      pr.index_value AS index_value,
      pr.probability AS probability,
      pa.prid AS range_id
    FROM
      probabilities AS pr
    LEFT JOIN
      pattern_ranges AS pa ON pa.index_value = pr.index_value
  ) UPDATE regab_prng_pattern_probs AS ta
  SET
    index_prob_given_ec = probability
  FROM update_table AS tb
  WHERE
    ta.range_id = tb.range_id AND
    ta.empty_count = tb.empty_count;
  
  RETURN nrec_expected;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
--
--

--
-- Given a board configuration returns the values taken by the pattern indexes.
--
CREATE FUNCTION regab_gp_compute_pattern_indexes (mover          square_set,
                                                  opponent       square_set,
                                                  is_principal   BOOL,
                                                  OUT i_edge_0   INTEGER,
                                                  OUT i_edge_1   INTEGER,
                                                  OUT i_edge_2   INTEGER,
                                                  OUT i_edge_3   INTEGER,
                                                  OUT i_corner_0 INTEGER,
                                                  OUT i_corner_1 INTEGER,
                                                  OUT i_corner_2 INTEGER,
                                                  OUT i_corner_3 INTEGER,
                                                  OUT i_xedge_0  INTEGER,
                                                  OUT i_xedge_1  INTEGER,
                                                  OUT i_xedge_2  INTEGER,
                                                  OUT i_xedge_3  INTEGER,
                                                  OUT i_r2_0     INTEGER,
                                                  OUT i_r2_1     INTEGER,
                                                  OUT i_r2_2     INTEGER,
                                                  OUT i_r2_3     INTEGER,
                                                  OUT i_r3_0     INTEGER,
                                                  OUT i_r3_1     INTEGER,
                                                  OUT i_r3_2     INTEGER,
                                                  OUT i_r3_3     INTEGER,
                                                  OUT i_r4_0     INTEGER,
                                                  OUT i_r4_1     INTEGER,
                                                  OUT i_r4_2     INTEGER,
                                                  OUT i_r4_3     INTEGER,
                                                  OUT i_diag4_0  INTEGER,
                                                  OUT i_diag4_1  INTEGER,
                                                  OUT i_diag4_2  INTEGER,
                                                  OUT i_diag4_3  INTEGER,
                                                  OUT i_diag5_0  INTEGER,
                                                  OUT i_diag5_1  INTEGER,
                                                  OUT i_diag5_2  INTEGER,
                                                  OUT i_diag5_3  INTEGER,
                                                  OUT i_diag6_0  INTEGER,
                                                  OUT i_diag6_1  INTEGER,
                                                  OUT i_diag6_2  INTEGER,
                                                  OUT i_diag6_3  INTEGER,
                                                  OUT i_diag7_0  INTEGER,
                                                  OUT i_diag7_1  INTEGER,
                                                  OUT i_diag7_2  INTEGER,
                                                  OUT i_diag7_3  INTEGER,
                                                  OUT i_diag8_0  INTEGER,
                                                  OUT i_diag8_1  INTEGER,
                                                  OUT i_2x5cor_0 INTEGER,
                                                  OUT i_2x5cor_1 INTEGER,
                                                  OUT i_2x5cor_2 INTEGER,
                                                  OUT i_2x5cor_3 INTEGER,
                                                  OUT i_2x5cor_4 INTEGER,
                                                  OUT i_2x5cor_5 INTEGER,
                                                  OUT i_2x5cor_6 INTEGER,
                                                  OUT i_2x5cor_7 INTEGER,
                                                  OUT i_diag3_0  INTEGER,
                                                  OUT i_diag3_1  INTEGER,
                                                  OUT i_diag3_2  INTEGER,
                                                  OUT i_diag3_3  INTEGER)
AS $$
DECLARE
  mo_identity square_set := mover;
  mo_rot_90a  square_set := square_set_rotate_90a(mover);
  mo_rot_180  square_set := square_set_rotate_180(mover);
  mo_rot_90c  square_set := square_set_rotate_90c(mover);
  mo_flip_ve  square_set := square_set_flip_vertical(mover);
  mo_flip_dh  square_set := square_set_flip_diag_h1a8(mover);
  mo_flip_ho  square_set := square_set_flip_horizontal(mover);
  mo_flip_da  square_set := square_set_flip_diag_a1h8(mover);
  ---
  op_identity square_set := opponent;
  op_rot_90a  square_set := square_set_rotate_90a(opponent);
  op_rot_180  square_set := square_set_rotate_180(opponent);
  op_rot_90c  square_set := square_set_rotate_90c(opponent);
  op_flip_ve  square_set := square_set_flip_vertical(opponent);
  op_flip_dh  square_set := square_set_flip_diag_h1a8(opponent);
  op_flip_ho  square_set := square_set_flip_horizontal(opponent);
  op_flip_da  square_set := square_set_flip_diag_a1h8(opponent);
  ---
  pid BIGINT;
BEGIN
  i_edge_0   := regab_transformed_pattern_to_index(square_set_pattern_pack_edge(mo_identity),
                                                   square_set_pattern_pack_edge(op_identity));
  i_edge_1   := regab_transformed_pattern_to_index(square_set_pattern_pack_edge(mo_rot_90a),
                                                   square_set_pattern_pack_edge(op_rot_90a));
  i_edge_2   := regab_transformed_pattern_to_index(square_set_pattern_pack_edge(mo_rot_180),
                                                   square_set_pattern_pack_edge(op_rot_180));
  i_edge_3   := regab_transformed_pattern_to_index(square_set_pattern_pack_edge(mo_rot_90c),
                                                   square_set_pattern_pack_edge(op_rot_90c));
  ---
  i_corner_0 := regab_transformed_pattern_to_index(square_set_pattern_pack_corner(mo_identity),
                                                   square_set_pattern_pack_corner(op_identity));
  i_corner_1 := regab_transformed_pattern_to_index(square_set_pattern_pack_corner(mo_rot_90a),
                                                   square_set_pattern_pack_corner(op_rot_90a));
  i_corner_2 := regab_transformed_pattern_to_index(square_set_pattern_pack_corner(mo_rot_180),
                                                   square_set_pattern_pack_corner(op_rot_180));
  i_corner_3 := regab_transformed_pattern_to_index(square_set_pattern_pack_corner(mo_rot_90c),
                                                   square_set_pattern_pack_corner(op_rot_90c));
  ---
  i_xedge_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_xedge(mo_identity),
                                                   square_set_pattern_pack_xedge(op_identity));
  i_xedge_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_xedge(mo_rot_90a),
                                                   square_set_pattern_pack_xedge(op_rot_90a));
  i_xedge_2  := regab_transformed_pattern_to_index(square_set_pattern_pack_xedge(mo_rot_180),
                                                   square_set_pattern_pack_xedge(op_rot_180));
  i_xedge_3  := regab_transformed_pattern_to_index(square_set_pattern_pack_xedge(mo_rot_90c),
                                                   square_set_pattern_pack_xedge(op_rot_90c));
  ---
  i_r2_0     := regab_transformed_pattern_to_index(square_set_pattern_pack_r2(mo_identity),
                                                   square_set_pattern_pack_r2(op_identity));
  i_r2_1     := regab_transformed_pattern_to_index(square_set_pattern_pack_r2(mo_rot_90a),
                                                   square_set_pattern_pack_r2(op_rot_90a));
  i_r2_2     := regab_transformed_pattern_to_index(square_set_pattern_pack_r2(mo_rot_180),
                                                   square_set_pattern_pack_r2(op_rot_180));
  i_r2_3     := regab_transformed_pattern_to_index(square_set_pattern_pack_r2(mo_rot_90c),
                                                   square_set_pattern_pack_r2(op_rot_90c));
  ---
  i_r3_0     := regab_transformed_pattern_to_index(square_set_pattern_pack_r3(mo_identity),
                                                   square_set_pattern_pack_r3(op_identity));
  i_r3_1     := regab_transformed_pattern_to_index(square_set_pattern_pack_r3(mo_rot_90a),
                                                   square_set_pattern_pack_r3(op_rot_90a));
  i_r3_2     := regab_transformed_pattern_to_index(square_set_pattern_pack_r3(mo_rot_180),
                                                   square_set_pattern_pack_r3(op_rot_180));
  i_r3_3     := regab_transformed_pattern_to_index(square_set_pattern_pack_r3(mo_rot_90c),
                                                   square_set_pattern_pack_r3(op_rot_90c));
  ---
  i_r4_0     := regab_transformed_pattern_to_index(square_set_pattern_pack_r4(mo_identity),
                                                   square_set_pattern_pack_r4(op_identity));
  i_r4_1     := regab_transformed_pattern_to_index(square_set_pattern_pack_r4(mo_rot_90a),
                                                   square_set_pattern_pack_r4(op_rot_90a));
  i_r4_2     := regab_transformed_pattern_to_index(square_set_pattern_pack_r4(mo_rot_180),
                                                   square_set_pattern_pack_r4(op_rot_180));
  i_r4_3     := regab_transformed_pattern_to_index(square_set_pattern_pack_r4(mo_rot_90c),
                                                   square_set_pattern_pack_r4(op_rot_90c));
  ---
  i_diag4_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag4(mo_identity),
                                                   square_set_pattern_pack_diag4(op_identity));
  i_diag4_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag4(mo_rot_90a),
                                                   square_set_pattern_pack_diag4(op_rot_90a));
  i_diag4_2  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag4(mo_rot_180),
                                                   square_set_pattern_pack_diag4(op_rot_180));
  i_diag4_3  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag4(mo_rot_90c),
                                                   square_set_pattern_pack_diag4(op_rot_90c));
  ---
  i_diag5_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag5(mo_identity),
                                                   square_set_pattern_pack_diag5(op_identity));
  i_diag5_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag5(mo_rot_90a),
                                                   square_set_pattern_pack_diag5(op_rot_90a));
  i_diag5_2  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag5(mo_rot_180),
                                                   square_set_pattern_pack_diag5(op_rot_180));
  i_diag5_3  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag5(mo_rot_90c),
                                                   square_set_pattern_pack_diag5(op_rot_90c));
  ---
  i_diag6_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag6(mo_identity),
                                                   square_set_pattern_pack_diag6(op_identity));
  i_diag6_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag6(mo_rot_90a),
                                                   square_set_pattern_pack_diag6(op_rot_90a));
  i_diag6_2  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag6(mo_rot_180),
                                                   square_set_pattern_pack_diag6(op_rot_180));
  i_diag6_3  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag6(mo_rot_90c),
                                                   square_set_pattern_pack_diag6(op_rot_90c));
  ---
  i_diag7_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag7(mo_identity),
                                                   square_set_pattern_pack_diag7(op_identity));
  i_diag7_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag7(mo_rot_90a),
                                                   square_set_pattern_pack_diag7(op_rot_90a));
  i_diag7_2  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag7(mo_rot_180),
                                                   square_set_pattern_pack_diag7(op_rot_180));
  i_diag7_3  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag7(mo_rot_90c),
                                                   square_set_pattern_pack_diag7(op_rot_90c));
  ---
  i_diag8_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag8(mo_identity),
                                                   square_set_pattern_pack_diag8(op_identity));
  i_diag8_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag8(mo_rot_90a),
                                                   square_set_pattern_pack_diag8(op_rot_90a));
  ---
  i_2x5cor_0 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_identity),
                                                   square_set_pattern_pack_2x5cor(op_identity));
  i_2x5cor_1 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_rot_90a),
                                                   square_set_pattern_pack_2x5cor(op_rot_90a));
  i_2x5cor_2 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_rot_180),
                                                   square_set_pattern_pack_2x5cor(op_rot_180));
  i_2x5cor_3 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_rot_90c),
                                                   square_set_pattern_pack_2x5cor(op_rot_90c));
  i_2x5cor_4 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_flip_ve),
                                                   square_set_pattern_pack_2x5cor(op_flip_ve));
  i_2x5cor_5 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_flip_dh),
                                                   square_set_pattern_pack_2x5cor(op_flip_dh));
  i_2x5cor_6 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_flip_ho),
                                                   square_set_pattern_pack_2x5cor(op_flip_ho));
  i_2x5cor_7 := regab_transformed_pattern_to_index(square_set_pattern_pack_2x5cor(mo_flip_da),
                                                   square_set_pattern_pack_2x5cor(op_flip_da));
  ---
  i_diag3_0  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag3(mo_identity),
                                                   square_set_pattern_pack_diag3(op_identity));
  i_diag3_1  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag3(mo_rot_90a),
                                                   square_set_pattern_pack_diag3(op_rot_90a));
  i_diag3_2  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag3(mo_rot_180),
                                                   square_set_pattern_pack_diag3(op_rot_180));
  i_diag3_3  := regab_transformed_pattern_to_index(square_set_pattern_pack_diag3(mo_rot_90c),
                                                   square_set_pattern_pack_diag3(op_rot_90c));

  --- Transform the index value to its principal value (mirror of minimal value).
  IF is_principal THEN
    --- EDGE
    pid := 0;
    SELECT principal_index_value INTO STRICT i_edge_0   FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_edge_0;
    SELECT principal_index_value INTO STRICT i_edge_1   FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_edge_1;
    SELECT principal_index_value INTO STRICT i_edge_2   FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_edge_2;
    SELECT principal_index_value INTO STRICT i_edge_3   FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_edge_3;
    --- CORNER
    pid := 1;
    SELECT principal_index_value INTO STRICT i_corner_0 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_corner_0;
    SELECT principal_index_value INTO STRICT i_corner_1 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_corner_1;
    SELECT principal_index_value INTO STRICT i_corner_2 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_corner_2;
    SELECT principal_index_value INTO STRICT i_corner_3 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_corner_3;
    --- XEDGE
    pid := 2;
    SELECT principal_index_value INTO STRICT i_xedge_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_xedge_0;
    SELECT principal_index_value INTO STRICT i_xedge_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_xedge_1;
    SELECT principal_index_value INTO STRICT i_xedge_2  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_xedge_2;
    SELECT principal_index_value INTO STRICT i_xedge_3  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_xedge_3;
    --- R2
    pid := 3;
    SELECT principal_index_value INTO STRICT i_r2_0     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r2_0;
    SELECT principal_index_value INTO STRICT i_r2_1     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r2_1;
    SELECT principal_index_value INTO STRICT i_r2_2     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r2_2;
    SELECT principal_index_value INTO STRICT i_r2_3     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r2_3;
    --- R3
    pid := 4;
    SELECT principal_index_value INTO STRICT i_r3_0     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r3_0;
    SELECT principal_index_value INTO STRICT i_r3_1     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r3_1;
    SELECT principal_index_value INTO STRICT i_r3_2     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r3_2;
    SELECT principal_index_value INTO STRICT i_r3_3     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r3_3;
    --- R4
    pid := 5;
    SELECT principal_index_value INTO STRICT i_r4_0     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r4_0;
    SELECT principal_index_value INTO STRICT i_r4_1     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r4_1;
    SELECT principal_index_value INTO STRICT i_r4_2     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r4_2;
    SELECT principal_index_value INTO STRICT i_r4_3     FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_r4_3;
    --- DIAG4
    pid := 6;
    SELECT principal_index_value INTO STRICT i_diag4_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag4_0;
    SELECT principal_index_value INTO STRICT i_diag4_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag4_1;
    SELECT principal_index_value INTO STRICT i_diag4_2  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag4_2;
    SELECT principal_index_value INTO STRICT i_diag4_3  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag4_3;
    --- DIAG5
    pid := 7;
    SELECT principal_index_value INTO STRICT i_diag5_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag5_0;
    SELECT principal_index_value INTO STRICT i_diag5_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag5_1;
    SELECT principal_index_value INTO STRICT i_diag5_2  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag5_2;
    SELECT principal_index_value INTO STRICT i_diag5_3  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag5_3;
    --- DIAG6
    pid := 8;
    SELECT principal_index_value INTO STRICT i_diag6_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag6_0;
    SELECT principal_index_value INTO STRICT i_diag6_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag6_1;
    SELECT principal_index_value INTO STRICT i_diag6_2  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag6_2;
    SELECT principal_index_value INTO STRICT i_diag6_3  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag6_3;
    --- DIAG7
    pid := 9;
    SELECT principal_index_value INTO STRICT i_diag7_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag7_0;
    SELECT principal_index_value INTO STRICT i_diag7_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag7_1;
    SELECT principal_index_value INTO STRICT i_diag7_2  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag7_2;
    SELECT principal_index_value INTO STRICT i_diag7_3  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag7_3;
    --- DIAG8
    pid := 10;
    SELECT principal_index_value INTO STRICT i_diag8_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag8_0;
    SELECT principal_index_value INTO STRICT i_diag8_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag8_1;
    --- 2X5COR
    pid := 11;
    SELECT principal_index_value INTO STRICT i_2x5cor_0 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_0;
    SELECT principal_index_value INTO STRICT i_2x5cor_1 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_1;
    SELECT principal_index_value INTO STRICT i_2x5cor_2 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_2;
    SELECT principal_index_value INTO STRICT i_2x5cor_3 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_3;
    SELECT principal_index_value INTO STRICT i_2x5cor_4 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_4;
    SELECT principal_index_value INTO STRICT i_2x5cor_5 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_5;
    SELECT principal_index_value INTO STRICT i_2x5cor_6 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_6;
    SELECT principal_index_value INTO STRICT i_2x5cor_7 FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_2x5cor_7;    
    --- DIAG3
    pid := 12;
    SELECT principal_index_value INTO STRICT i_diag3_0  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag3_0;
    SELECT principal_index_value INTO STRICT i_diag3_1  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag3_1;
    SELECT principal_index_value INTO STRICT i_diag3_2  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag3_2;
    SELECT principal_index_value INTO STRICT i_diag3_3  FROM regab_prng_pattern_ranges WHERE pattern_id = pid AND index_value = i_diag3_3;
  END IF;
  
END;
$$ LANGUAGE plpgsql VOLATILE;
--- Tests.
DO $$
DECLARE
  mo square_set := 2337477075470322191::BIGINT;
  op square_set := 6633411755847992320::BIGINT;
  black player := 0::player;
  pattern_index_values RECORD;
  --
  is_principal BOOL := FALSE; -- could not be true before loading the regab_prng_pattern_probs table.
BEGIN
  SELECT * INTO pattern_index_values FROM regab_gp_compute_pattern_indexes(mo, op, is_principal);
  ---
  PERFORM p_assert(pattern_index_values.i_edge_0 =   40, 'Expected value for i_edge_0 is   40.');
  PERFORM p_assert(pattern_index_values.i_edge_1 =  702, 'Expected value for i_edge_1 is  702.');
  PERFORM p_assert(pattern_index_values.i_edge_2 =  717, 'Expected value for i_edge_2 is  717.');
  PERFORM p_assert(pattern_index_values.i_edge_3 = 2439, 'Expected value for i_edge_3 is 2439.');
  ---
  PERFORM p_assert(pattern_index_values.i_corner_0 = 10057, 'Expected value for i_corner_0 is 10057.');
  PERFORM p_assert(pattern_index_values.i_corner_1 = 13284, 'Expected value for i_corner_1 is 13284.');
  PERFORM p_assert(pattern_index_values.i_corner_2 = 10545, 'Expected value for i_corner_2 is 10545.');
  PERFORM p_assert(pattern_index_values.i_corner_3 = 19368, 'Expected value for i_corner_3 is 19368.');
  ---
  PERFORM p_assert(pattern_index_values.i_xedge_0 = 45967, 'Expected value for i_xedge_0 is 45967.');
  PERFORM p_assert(pattern_index_values.i_xedge_1 = 33507, 'Expected value for i_xedge_1 is 33507.');
  PERFORM p_assert(pattern_index_values.i_xedge_2 = 46644, 'Expected value for i_xedge_2 is 46644.');
  PERFORM p_assert(pattern_index_values.i_xedge_3 = 35244, 'Expected value for i_xedge_3 is 35244.');
  ---
  PERFORM p_assert(pattern_index_values.i_r2_0 = 1506, 'Expected value for i_r2_0 is 1506.');
  PERFORM p_assert(pattern_index_values.i_r2_1 = 5514, 'Expected value for i_r2_1 is 5514.');
  PERFORM p_assert(pattern_index_values.i_r2_2 = 2145, 'Expected value for i_r2_2 is 2145.');
  PERFORM p_assert(pattern_index_values.i_r2_3 = 3255, 'Expected value for i_r2_3 is 3255.');
  --- 
  PERFORM p_assert(pattern_index_values.i_r3_0 =  715, 'Expected value for i_r3_0 is  715.');
  PERFORM p_assert(pattern_index_values.i_r3_1 = 3231, 'Expected value for i_r3_1 is 3231.');
  PERFORM p_assert(pattern_index_values.i_r3_2 = 3632, 'Expected value for i_r3_2 is 3632.');
  PERFORM p_assert(pattern_index_values.i_r3_3 = 4103, 'Expected value for i_r3_3 is 4103.');
  ---
  PERFORM p_assert(pattern_index_values.i_r4_0 = 5097, 'Expected value for i_r4_0 is 5097.');
  PERFORM p_assert(pattern_index_values.i_r4_1 = 5823, 'Expected value for i_r4_1 is 5823.');
  PERFORM p_assert(pattern_index_values.i_r4_2 =  467, 'Expected value for i_r4_2 is  467.');
  PERFORM p_assert(pattern_index_values.i_r4_3 = 3644, 'Expected value for i_r4_3 is 3644.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag4_0 = 48, 'Expected value for i_diag4_0 is 48.');
  PERFORM p_assert(pattern_index_values.i_diag4_1 = 54, 'Expected value for i_diag4_1 is 54.');
  PERFORM p_assert(pattern_index_values.i_diag4_2 = 68, 'Expected value for i_diag4_2 is 68.');
  PERFORM p_assert(pattern_index_values.i_diag4_3 = 17, 'Expected value for i_diag4_3 is 17.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag5_0 =  39, 'Expected value for i_diag5_0 is  39.');
  PERFORM p_assert(pattern_index_values.i_diag5_1 = 181, 'Expected value for i_diag5_1 is 181.');
  PERFORM p_assert(pattern_index_values.i_diag5_2 = 206, 'Expected value for i_diag5_2 is 206.');
  PERFORM p_assert(pattern_index_values.i_diag5_3 =  26, 'Expected value for i_diag5_3 is  26.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag6_0 =  73, 'Expected value for i_diag6_0 is  73.');
  PERFORM p_assert(pattern_index_values.i_diag6_1 = 724, 'Expected value for i_diag6_1 is 724.');
  PERFORM p_assert(pattern_index_values.i_diag6_2 = 702, 'Expected value for i_diag6_2 is 702.');
  PERFORM p_assert(pattern_index_values.i_diag6_3 = 373, 'Expected value for i_diag6_3 is 373.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag7_0 = 228, 'Expected value for i_diag7_0 is 228.');
  PERFORM p_assert(pattern_index_values.i_diag7_1 = 322, 'Expected value for i_diag7_1 is 322.');
  PERFORM p_assert(pattern_index_values.i_diag7_2 = 720, 'Expected value for i_diag7_2 is 720.');
  PERFORM p_assert(pattern_index_values.i_diag7_3 = 482, 'Expected value for i_diag7_3 is 482.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag8_0 = 2184, 'Expected value for i_diag8_0 is 2184.');
  PERFORM p_assert(pattern_index_values.i_diag8_1 = 1201, 'Expected value for i_diag8_1 is 1201.');
  ---
  PERFORM p_assert(pattern_index_values.i_2x5cor_0 = 11704, 'Expected value for i_2x5cor_0 is 11704.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_1 = 41040, 'Expected value for i_2x5cor_1 is 41040.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_2 = 49074, 'Expected value for i_2x5cor_2 is 49074.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_3 = 23337, 'Expected value for i_2x5cor_3 is 23337.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_4 = 21222, 'Expected value for i_2x5cor_4 is 21222.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_5 = 16758, 'Expected value for i_2x5cor_5 is 16758.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_6 = 38871, 'Expected value for i_2x5cor_6 is 38871.');
  PERFORM p_assert(pattern_index_values.i_2x5cor_7 =  9730, 'Expected value for i_2x5cor_7 is  9730.');
  ---
  PERFORM p_assert(pattern_index_values.i_diag3_0 = 13, 'Expected value for i_diag3_0 is 13.');
  PERFORM p_assert(pattern_index_values.i_diag3_1 =  6, 'Expected value for i_diag3_1 is  6.');
  PERFORM p_assert(pattern_index_values.i_diag3_2 = 14, 'Expected value for i_diag3_2 is 14.');
  PERFORM p_assert(pattern_index_values.i_diag3_3 = 17, 'Expected value for i_diag3_3 is 17.');
END $$;

--
-- Populates or updates table regab_prng_gp_pattern_class.
--
-- Example call:
--
-- => SELECT * FROM regab_gp_populate_pattern_class_table(1, 20, 30, '{CMR,CMS}', FALSE, FALSE);
-- 
CREATE FUNCTION regab_gp_populate_pattern_class_table (batch_id_arg         INTEGER,
                                                       empty_count_min_arg  INTEGER,
                                                       empty_count_max_arg  INTEGER,
                                                       status_arg           CHAR(3)[],
                                                       verb                 BOOL,
                                                       simulation           BOOL,
                                                       OUT rec_selected_cnt BIGINT,
                                                       OUT rec_create_cnt   BIGINT,
                                                       OUT rec_update_cnt   BIGINT)
AS $$
DECLARE
  game_position_rec RECORD;
  game_position_cur CURSOR (batch_id_c INTEGER, empty_count_min INTEGER, empty_count_max INTEGER, status_c CHAR(3)[])
    FOR SELECT seq, status, empty_count, mover, opponent
    FROM regab_prng_gp
    WHERE batch_id = batch_id_c AND empty_count >= empty_count_min AND empty_count <= empty_count_max AND status = ANY (status_c);
  gp_pattern_class_o_rec RECORD;
  gp_pattern_class_n_rec RECORD;
  is_principal_index BOOL := TRUE;
BEGIN

  rec_selected_cnt := 0;
  rec_create_cnt := 0;
  rec_update_cnt := 0;

  OPEN game_position_cur(batch_id_arg, empty_count_min_arg, empty_count_max_arg, status_arg);

  LOOP
    FETCH game_position_cur INTO game_position_rec;
    EXIT WHEN NOT FOUND;

    rec_selected_cnt := rec_selected_cnt + 1;

    SELECT INTO gp_pattern_class_o_rec * FROM regab_prng_gp_pattern_class WHERE gp_id = game_position_rec.seq;
    IF gp_pattern_class_o_rec IS NULL THEN
      IF verb THEN
        RAISE NOTICE 'INSERT gp_id = %.', game_position_rec.seq;
      END IF;
      IF NOT simulation THEN
        INSERT INTO regab_prng_gp_pattern_class (gp_id, ins_time, status, cst_time) VALUES (game_position_rec.seq, now(), 'INS', now());
      END IF;
      rec_create_cnt := rec_create_cnt + 1;
    END IF;

    is_principal_index := FALSE;
    SELECT * INTO gp_pattern_class_n_rec FROM regab_gp_compute_pattern_indexes(game_position_rec.mover, game_position_rec.opponent, is_principal_index);

    IF (gp_pattern_class_o_rec IS NULL
        OR gp_pattern_class_o_rec.status <> 'CMP'
        OR gp_pattern_class_o_rec.i_edge_0   <> gp_pattern_class_n_rec.i_edge_0
        OR gp_pattern_class_o_rec.i_edge_1   <> gp_pattern_class_n_rec.i_edge_1
        OR gp_pattern_class_o_rec.i_edge_2   <> gp_pattern_class_n_rec.i_edge_2
        OR gp_pattern_class_o_rec.i_edge_3   <> gp_pattern_class_n_rec.i_edge_3
        OR gp_pattern_class_o_rec.i_corner_0 <> gp_pattern_class_n_rec.i_corner_0
        OR gp_pattern_class_o_rec.i_corner_1 <> gp_pattern_class_n_rec.i_corner_1
        OR gp_pattern_class_o_rec.i_corner_2 <> gp_pattern_class_n_rec.i_corner_2
        OR gp_pattern_class_o_rec.i_corner_3 <> gp_pattern_class_n_rec.i_corner_3
        OR gp_pattern_class_o_rec.i_xedge_0  <> gp_pattern_class_n_rec.i_xedge_0
        OR gp_pattern_class_o_rec.i_xedge_1  <> gp_pattern_class_n_rec.i_xedge_1
        OR gp_pattern_class_o_rec.i_xedge_2  <> gp_pattern_class_n_rec.i_xedge_2
        OR gp_pattern_class_o_rec.i_xedge_3  <> gp_pattern_class_n_rec.i_xedge_3
        OR gp_pattern_class_o_rec.i_r2_0     <> gp_pattern_class_n_rec.i_r2_0
        OR gp_pattern_class_o_rec.i_r2_1     <> gp_pattern_class_n_rec.i_r2_1
        OR gp_pattern_class_o_rec.i_r2_2     <> gp_pattern_class_n_rec.i_r2_2
        OR gp_pattern_class_o_rec.i_r2_3     <> gp_pattern_class_n_rec.i_r2_3
        OR gp_pattern_class_o_rec.i_r3_0     <> gp_pattern_class_n_rec.i_r3_0
        OR gp_pattern_class_o_rec.i_r3_1     <> gp_pattern_class_n_rec.i_r3_1
        OR gp_pattern_class_o_rec.i_r3_2     <> gp_pattern_class_n_rec.i_r3_2
        OR gp_pattern_class_o_rec.i_r3_3     <> gp_pattern_class_n_rec.i_r3_3
        OR gp_pattern_class_o_rec.i_r4_0     <> gp_pattern_class_n_rec.i_r4_0
        OR gp_pattern_class_o_rec.i_r4_1     <> gp_pattern_class_n_rec.i_r4_1
        OR gp_pattern_class_o_rec.i_r4_2     <> gp_pattern_class_n_rec.i_r4_2
        OR gp_pattern_class_o_rec.i_r4_3     <> gp_pattern_class_n_rec.i_r4_3
        OR gp_pattern_class_o_rec.i_diag4_0  <> gp_pattern_class_n_rec.i_diag4_0
        OR gp_pattern_class_o_rec.i_diag4_1  <> gp_pattern_class_n_rec.i_diag4_1
        OR gp_pattern_class_o_rec.i_diag4_2  <> gp_pattern_class_n_rec.i_diag4_2
        OR gp_pattern_class_o_rec.i_diag4_3  <> gp_pattern_class_n_rec.i_diag4_3
        OR gp_pattern_class_o_rec.i_diag5_0  <> gp_pattern_class_n_rec.i_diag5_0
        OR gp_pattern_class_o_rec.i_diag5_1  <> gp_pattern_class_n_rec.i_diag5_1
        OR gp_pattern_class_o_rec.i_diag5_2  <> gp_pattern_class_n_rec.i_diag5_2
        OR gp_pattern_class_o_rec.i_diag5_3  <> gp_pattern_class_n_rec.i_diag5_3
        OR gp_pattern_class_o_rec.i_diag6_0  <> gp_pattern_class_n_rec.i_diag6_0
        OR gp_pattern_class_o_rec.i_diag6_1  <> gp_pattern_class_n_rec.i_diag6_1
        OR gp_pattern_class_o_rec.i_diag6_2  <> gp_pattern_class_n_rec.i_diag6_2
        OR gp_pattern_class_o_rec.i_diag6_3  <> gp_pattern_class_n_rec.i_diag6_3
        OR gp_pattern_class_o_rec.i_diag7_0  <> gp_pattern_class_n_rec.i_diag7_0
        OR gp_pattern_class_o_rec.i_diag7_1  <> gp_pattern_class_n_rec.i_diag7_1
        OR gp_pattern_class_o_rec.i_diag7_2  <> gp_pattern_class_n_rec.i_diag7_2
        OR gp_pattern_class_o_rec.i_diag7_3  <> gp_pattern_class_n_rec.i_diag7_3
        OR gp_pattern_class_o_rec.i_diag8_0  <> gp_pattern_class_n_rec.i_diag8_0
        OR gp_pattern_class_o_rec.i_diag8_1  <> gp_pattern_class_n_rec.i_diag8_1
        OR gp_pattern_class_o_rec.i_2x5cor_0 <> gp_pattern_class_n_rec.i_2x5cor_0
        OR gp_pattern_class_o_rec.i_2x5cor_1 <> gp_pattern_class_n_rec.i_2x5cor_1
        OR gp_pattern_class_o_rec.i_2x5cor_2 <> gp_pattern_class_n_rec.i_2x5cor_2
        OR gp_pattern_class_o_rec.i_2x5cor_3 <> gp_pattern_class_n_rec.i_2x5cor_3
        OR gp_pattern_class_o_rec.i_2x5cor_4 <> gp_pattern_class_n_rec.i_2x5cor_4
        OR gp_pattern_class_o_rec.i_2x5cor_5 <> gp_pattern_class_n_rec.i_2x5cor_5
        OR gp_pattern_class_o_rec.i_2x5cor_6 <> gp_pattern_class_n_rec.i_2x5cor_6
        OR gp_pattern_class_o_rec.i_2x5cor_7 <> gp_pattern_class_n_rec.i_2x5cor_7
        OR gp_pattern_class_o_rec.i_diag3_0  <> gp_pattern_class_n_rec.i_diag3_0
        OR gp_pattern_class_o_rec.i_diag3_1  <> gp_pattern_class_n_rec.i_diag3_1
        OR gp_pattern_class_o_rec.i_diag3_2  <> gp_pattern_class_n_rec.i_diag3_2
        OR gp_pattern_class_o_rec.i_diag3_3  <> gp_pattern_class_n_rec.i_diag3_3
       ) THEN

      IF verb THEN
        RAISE NOTICE 'UPDATE gp_id = %.', game_position_rec.seq;
      END IF;

      IF NOT simulation THEN
        UPDATE regab_prng_gp_pattern_class SET status     = 'CMP',
                                               cst_time   = now(),
                                               i_edge_0   = gp_pattern_class_n_rec.i_edge_0,
                                               i_edge_1   = gp_pattern_class_n_rec.i_edge_1,
                                               i_edge_2   = gp_pattern_class_n_rec.i_edge_2,
                                               i_edge_3   = gp_pattern_class_n_rec.i_edge_3,
                                               i_corner_0 = gp_pattern_class_n_rec.i_corner_0,
                                               i_corner_1 = gp_pattern_class_n_rec.i_corner_1,
                                               i_corner_2 = gp_pattern_class_n_rec.i_corner_2,
                                               i_corner_3 = gp_pattern_class_n_rec.i_corner_3,
                                               i_xedge_0  = gp_pattern_class_n_rec.i_xedge_0,
                                               i_xedge_1  = gp_pattern_class_n_rec.i_xedge_1,
                                               i_xedge_2  = gp_pattern_class_n_rec.i_xedge_2,
                                               i_xedge_3  = gp_pattern_class_n_rec.i_xedge_3,
                                               i_r2_0     = gp_pattern_class_n_rec.i_r2_0,
                                               i_r2_1     = gp_pattern_class_n_rec.i_r2_1,
                                               i_r2_2     = gp_pattern_class_n_rec.i_r2_2,
                                               i_r2_3     = gp_pattern_class_n_rec.i_r2_3,
                                               i_r3_0     = gp_pattern_class_n_rec.i_r3_0,
                                               i_r3_1     = gp_pattern_class_n_rec.i_r3_1,
                                               i_r3_2     = gp_pattern_class_n_rec.i_r3_2,
                                               i_r3_3     = gp_pattern_class_n_rec.i_r3_3,
                                               i_r4_0     = gp_pattern_class_n_rec.i_r4_0,
                                               i_r4_1     = gp_pattern_class_n_rec.i_r4_1,
                                               i_r4_2     = gp_pattern_class_n_rec.i_r4_2,
                                               i_r4_3     = gp_pattern_class_n_rec.i_r4_3,
                                               i_diag4_0  = gp_pattern_class_n_rec.i_diag4_0,
                                               i_diag4_1  = gp_pattern_class_n_rec.i_diag4_1,
                                               i_diag4_2  = gp_pattern_class_n_rec.i_diag4_2,
                                               i_diag4_3  = gp_pattern_class_n_rec.i_diag4_3,
                                               i_diag5_0  = gp_pattern_class_n_rec.i_diag5_0,
                                               i_diag5_1  = gp_pattern_class_n_rec.i_diag5_1,
                                               i_diag5_2  = gp_pattern_class_n_rec.i_diag5_2,
                                               i_diag5_3  = gp_pattern_class_n_rec.i_diag5_3,
                                               i_diag6_0  = gp_pattern_class_n_rec.i_diag6_0,
                                               i_diag6_1  = gp_pattern_class_n_rec.i_diag6_1,
                                               i_diag6_2  = gp_pattern_class_n_rec.i_diag6_2,
                                               i_diag6_3  = gp_pattern_class_n_rec.i_diag6_3,
                                               i_diag7_0  = gp_pattern_class_n_rec.i_diag7_0,
                                               i_diag7_1  = gp_pattern_class_n_rec.i_diag7_1,
                                               i_diag7_2  = gp_pattern_class_n_rec.i_diag7_2,
                                               i_diag7_3  = gp_pattern_class_n_rec.i_diag7_3,
                                               i_diag8_0  = gp_pattern_class_n_rec.i_diag8_0,
                                               i_diag8_1  = gp_pattern_class_n_rec.i_diag8_1,
                                               i_2x5cor_0 = gp_pattern_class_n_rec.i_2x5cor_0,
                                               i_2x5cor_1 = gp_pattern_class_n_rec.i_2x5cor_1,
                                               i_2x5cor_2 = gp_pattern_class_n_rec.i_2x5cor_2,
                                               i_2x5cor_3 = gp_pattern_class_n_rec.i_2x5cor_3,
                                               i_2x5cor_4 = gp_pattern_class_n_rec.i_2x5cor_4,
                                               i_2x5cor_5 = gp_pattern_class_n_rec.i_2x5cor_5,
                                               i_2x5cor_6 = gp_pattern_class_n_rec.i_2x5cor_6,
                                               i_2x5cor_7 = gp_pattern_class_n_rec.i_2x5cor_7,
                                               i_diag3_0  = gp_pattern_class_n_rec.i_diag3_0,
                                               i_diag3_1  = gp_pattern_class_n_rec.i_diag3_1,
                                               i_diag3_2  = gp_pattern_class_n_rec.i_diag3_2,
                                               i_diag3_3  = gp_pattern_class_n_rec.i_diag3_3
          WHERE gp_id = game_position_rec.seq;
      END IF;
        
      rec_update_cnt := rec_update_cnt + 1;
    END IF;

  END LOOP;
  
  CLOSE game_position_cur;

END;
$$ LANGUAGE plpgsql VOLATILE;

COMMIT;
