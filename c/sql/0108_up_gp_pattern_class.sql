--
-- 0108_up_gp_pattern_class.sql
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
-- Adds new table regab_prng_gp_pattern_class.
--

SET search_path TO reversi;

BEGIN;

-- Migration set-up
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0108, now(), 'gp_pattern_class', 'adds the regab_prng_gp_pattern_class table.');

--
-- The table is an extension of regab_prng_gp.
-- It records the index values for all the pattern instances corresponding to the game position being referenced.
--
CREATE TABLE regab_prng_gp_pattern_class (gp_id      BIGINT PRIMARY KEY REFERENCES regab_prng_gp(seq) ON DELETE CASCADE,
                                          ---
                                          ins_time   TIMESTAMP DEFAULT now(),
                                          status     CHAR(3)   DEFAULT 'INS',
                                          cst_time   TIMESTAMP DEFAULT now(),
                                          ---
                                          i_edge_0   INTEGER,
                                          i_edge_1   INTEGER,
                                          i_edge_2   INTEGER,
                                          i_edge_3   INTEGER,
                                          ---
                                          i_corner_0 INTEGER,
                                          i_corner_1 INTEGER,
                                          i_corner_2 INTEGER,
                                          i_corner_3 INTEGER,
                                          ---
                                          i_xedge_0  INTEGER,
                                          i_xedge_1  INTEGER,
                                          i_xedge_2  INTEGER,
                                          i_xedge_3  INTEGER,
                                          ---
                                          i_r2_0     INTEGER,
                                          i_r2_1     INTEGER,
                                          i_r2_2     INTEGER,
                                          i_r2_3     INTEGER,
                                          ---
                                          i_r3_0     INTEGER,
                                          i_r3_1     INTEGER,
                                          i_r3_2     INTEGER,
                                          i_r3_3     INTEGER,
                                          ---
                                          i_r4_0     INTEGER,
                                          i_r4_1     INTEGER,
                                          i_r4_2     INTEGER,
                                          i_r4_3     INTEGER,
                                          ---
                                          i_diag4_0  INTEGER,
                                          i_diag4_1  INTEGER,
                                          i_diag4_2  INTEGER,
                                          i_diag4_3  INTEGER,
                                          ---
                                          i_diag5_0  INTEGER,
                                          i_diag5_1  INTEGER,
                                          i_diag5_2  INTEGER,
                                          i_diag5_3  INTEGER,
                                          ---
                                          i_diag6_0  INTEGER,
                                          i_diag6_1  INTEGER,
                                          i_diag6_2  INTEGER,
                                          i_diag6_3  INTEGER,
                                          ---
                                          i_diag7_0  INTEGER,
                                          i_diag7_1  INTEGER,
                                          i_diag7_2  INTEGER,
                                          i_diag7_3  INTEGER,
                                          ---
                                          i_diag8_0  INTEGER,
                                          i_diag8_1  INTEGER,
                                          ---
                                          i_2x5cor_0 INTEGER,
                                          i_2x5cor_1 INTEGER,
                                          i_2x5cor_2 INTEGER,
                                          i_2x5cor_3 INTEGER,
                                          i_2x5cor_4 INTEGER,
                                          i_2x5cor_5 INTEGER,
                                          i_2x5cor_6 INTEGER,
                                          i_2x5cor_7 INTEGER,
                                          ---
                                          CHECK (status IN ('INS', 'WIP', 'CMP')));

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
-- Flips a square set about the diagonal a1-h8
-- Square h1 is mapped to a8 and vice versa.
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
-- Given a board configuration returns the values taken by the pattern indexes.
--
CREATE FUNCTION regab_gp_compute_pattern_indexes (mover          square_set,
                                                  opponent       square_set,
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
                                                  OUT i_2x5cor_7 INTEGER)
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

END;
$$ LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE;

--- Tests.
DO $$
DECLARE
  mo square_set := 2337477075470322191::BIGINT;
  op square_set := 6633411755847992320::BIGINT;
  black player := 0::player;
  pattern_index_values RECORD;
BEGIN
  SELECT * INTO pattern_index_values FROM regab_gp_compute_pattern_indexes(mo, op);
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
END $$;

--
-- Populates or updates table regab_prng_gp_pattern_class.
--
-- Example call:
--
-- => SELECT * FROM regab_gp_populate_pattern_class_table(1, 20, 30, '{CMR,CMS}', FALSE, FALSE);
-- 
CREATE FUNCTION regab_gp_populate_pattern_class_table (batch_id_in          INTEGER,
                                                       empty_count_min_in   INTEGER,
                                                       empty_count_max_in   INTEGER,
                                                       status_in            CHAR(3)[],
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
    WHERE batch_id = batch_id_c AND empty_count > empty_count_min AND empty_count < empty_count_max AND status = ANY (status_c);
  gp_pattern_class_o_rec RECORD;
  gp_pattern_class_n_rec RECORD;
BEGIN

  rec_selected_cnt := 0;
  rec_create_cnt := 0;
  rec_update_cnt := 0;

  OPEN game_position_cur(batch_id_in, empty_count_min_in, empty_count_max_in, status_in);

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

    SELECT * INTO gp_pattern_class_n_rec FROM regab_gp_compute_pattern_indexes(game_position_rec.mover, game_position_rec.opponent);

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
                                               i_2x5cor_7 = gp_pattern_class_n_rec.i_2x5cor_7
          WHERE gp_id = game_position_rec.seq;
      END IF;
        
      rec_update_cnt := rec_update_cnt + 1;
    END IF;

  END LOOP;
  
  CLOSE game_position_cur;

END;
$$ LANGUAGE plpgsql VOLATILE;



-- End of migration
COMMIT;
