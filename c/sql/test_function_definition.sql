--
-- test_function_definition.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2014 Roberto Corradini. All rights reserved.
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
-- This script has been tested with PostgreSQL.
-- Start psql by running: psql -U reversi -w -d reversi -h localhost
-- Load the file by running the command: \i test_function_definition.sql
--
--
-- This script creates the functions used by the reversi program.
--

SET search_path TO reversi;



--
-- Tests the bit_works_bitscanMS1B_8 function.
--
CREATE OR REPLACE FUNCTION test_bit_works_bitscanMS1B_8() RETURNS VOID AS $$
DECLARE
  computed SMALLINT;
  expected SMALLINT;
BEGIN
  expected := 0;
  computed := bit_works_bitscanMS1B_8(CAST (1 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 0.');

  expected := 1;
  computed := bit_works_bitscanMS1B_8(CAST (2 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 1.');

  expected := 7;
  computed := bit_works_bitscanMS1B_8(CAST (128 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 7.');

  expected := 2;
  computed := bit_works_bitscanMS1B_8(CAST (6 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 2.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the bit_works_bitscanLS1B_64 function.
--
CREATE OR REPLACE FUNCTION test_bit_works_bitscanLS1B_64() RETURNS VOID AS $$
BEGIN
  PERFORM p_assert( 0 = bit_works_bitscanLS1B_64(1::BIGINT),                     'Expected value is  0.');
  PERFORM p_assert( 0 = bit_works_bitscanLS1B_64((-1)::BIGINT),                  'Expected value is  0.');
  PERFORM p_assert( 0 = bit_works_bitscanLS1B_64((x'0000000000000001')::BIGINT), 'Expected value is  0.');
  PERFORM p_assert( 1 = bit_works_bitscanLS1B_64((x'0000000000000002')::BIGINT), 'Expected value is  1.');
  PERFORM p_assert( 0 = bit_works_bitscanLS1B_64((x'0000000000000003')::BIGINT), 'Expected value is  0.');
  PERFORM p_assert( 2 = bit_works_bitscanLS1B_64((x'0000000000000004')::BIGINT), 'Expected value is  2.');
  PERFORM p_assert(63 = bit_works_bitscanLS1B_64((x'8000000000000000')::BIGINT), 'Expected value is 63.');
  PERFORM p_assert(56 = bit_works_bitscanLS1B_64((x'FF00000000000000')::BIGINT), 'Expected value is 56.');
  PERFORM p_assert(48 = bit_works_bitscanLS1B_64((x'00FF000000000000')::BIGINT), 'Expected value is 48.');
  PERFORM p_assert(40 = bit_works_bitscanLS1B_64((x'0000FF0000000000')::BIGINT), 'Expected value is 40.');
  PERFORM p_assert(32 = bit_works_bitscanLS1B_64((x'000000FF00000000')::BIGINT), 'Expected value is 32.');
  PERFORM p_assert(24 = bit_works_bitscanLS1B_64((x'00000000FF000000')::BIGINT), 'Expected value is 24.');
  PERFORM p_assert(16 = bit_works_bitscanLS1B_64((x'0000000000FF0000')::BIGINT), 'Expected value is 16.');
  PERFORM p_assert( 8 = bit_works_bitscanLS1B_64((x'000000000000FF00')::BIGINT), 'Expected value is  8.');
  PERFORM p_assert( 0 = bit_works_bitscanLS1B_64((x'00000000000000FF')::BIGINT), 'Expected value is  0.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the bit_works_fill_in_between_8 function.
--
CREATE OR REPLACE FUNCTION test_bit_works_fill_in_between_8() RETURNS VOID AS $$
DECLARE
  computed SMALLINT;
  expected SMALLINT;
BEGIN
  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (1 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 0. Ref a.');

  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (128 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 0. Ref b.');

  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (8 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 0. Ref c.');

  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (3 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 0. Ref d.');

  expected := 8;
  computed := bit_works_fill_in_between_8(CAST (20 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 8.');

  expected := 126;
  computed := bit_works_fill_in_between_8(CAST (129 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 126.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the bit_works_highest_bit_set_8 function.
--
CREATE OR REPLACE FUNCTION test_bit_works_highest_bit_set_8() RETURNS VOID AS $$
DECLARE
  computed SMALLINT;
  expected SMALLINT;
BEGIN
  expected := 0;
  computed := bit_works_highest_bit_set_8(CAST (0 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 0.');

  expected := 1;
  computed := bit_works_highest_bit_set_8(CAST (1 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 1. Ref a');

  expected := 1;
  computed := bit_works_highest_bit_set_8(CAST (1 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 1. Ref b.');

  expected := 2;
  computed := bit_works_highest_bit_set_8(CAST (2 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 2. Ref a.');

  expected := 2;
  computed := bit_works_highest_bit_set_8(CAST (3 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 2. Ref b.');

  expected := 128;
  computed := bit_works_highest_bit_set_8(CAST (128 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 128. Ref a.');

  expected := 128;
  computed := bit_works_highest_bit_set_8(CAST (255 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected value is 128. Ref b.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the bit_works_lowest_bit_set_8 function.
--
CREATE OR REPLACE FUNCTION test_bit_works_lowest_bit_set_8() RETURNS VOID AS $$
DECLARE
  computed SMALLINT;
  expected SMALLINT;
BEGIN
  expected := 0;
  computed := bit_works_lowest_bit_set_8(0::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected value is 0.');

  expected := 1;
  computed := bit_works_lowest_bit_set_8(1::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected value is 1. Ref a.');

  expected := 2;
  computed := bit_works_lowest_bit_set_8(2::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected value is 2.');

  expected := 1;
  computed := bit_works_lowest_bit_set_8(3::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected value is 1. Ref b.');

  expected := 1;
  computed := bit_works_lowest_bit_set_8(255::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected value is 1. Ref c.');

  expected := 128;
  computed := bit_works_lowest_bit_set_8(128::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected value is 128.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the bit_works_signed_left_shift function.
--
CREATE OR REPLACE FUNCTION test_bit_works_signed_left_shift() RETURNS VOID AS $$
DECLARE
  computed BIGINT;
  expected BIGINT;
BEGIN
  expected := 0;
  computed := bit_works_signed_left_shift(CAST (0 AS BIGINT), 1);
  PERFORM p_assert(expected = computed, 'Expected value is 0. Ref a.');

  expected := 0;
  computed := bit_works_signed_left_shift(CAST (0 AS BIGINT), -1);
  PERFORM p_assert(expected = computed, 'Expected value is 0. Ref b.');

  expected := 2;
  computed := bit_works_signed_left_shift(CAST (1 AS BIGINT), 1);
  PERFORM p_assert(expected = computed, 'Expected value is 2.');

  expected := 6;
  computed := bit_works_signed_left_shift(CAST (24 AS BIGINT), -2);
  PERFORM p_assert(expected = computed, 'Expected value is 6.');

  expected := -9223372036854775808;
  computed := bit_works_signed_left_shift(CAST (1 AS BIGINT), 63);
  PERFORM p_assert(expected = computed, 'Expected value is -9223372036854775808.');

  expected := 1;
  computed := bit_works_signed_left_shift(CAST (-9223372036854775808 AS BIGINT), -63);
  PERFORM p_assert(expected = computed, 'Expected value is 1.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the player_to_char function.
--
CREATE OR REPLACE FUNCTION test_player_to_char() RETURNS VOID AS $$
DECLARE
  computed CHAR(1);
  expected CHAR(1);
  pl       player;
BEGIN
  pl := 0;
  expected := 'b';
  computed := player_to_char(pl);
  PERFORM p_assert(expected = computed, 'Expected value is b.');

  pl := 1;
  expected := 'w';
  computed := player_to_char(pl);
  PERFORM p_assert(expected = computed, 'Expected value is w.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the player_to_string function.
--
CREATE OR REPLACE FUNCTION test_player_to_string() RETURNS VOID AS $$
DECLARE
  computed TEXT;
  expected TEXT;
  pl       player;
BEGIN
  pl := 0;
  expected := 'BLACK';
  computed := player_to_string(pl);
  PERFORM p_assert(expected = computed, 'Expected value is BLACK.');

  pl := 1;
  expected := 'WHITE';
  computed := player_to_string(pl);
  PERFORM p_assert(expected = computed, 'Expected value is WHITE.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the player_opponent function.
--
CREATE OR REPLACE FUNCTION test_player_opponent() RETURNS VOID AS $$
DECLARE
  black player := 0;
  white player := 1;
BEGIN
  PERFORM p_assert(white = player_opponent(black), 'White (1) is the opponent of Black (0).');
  PERFORM p_assert(black = player_opponent(white), 'Black (0) is the opponent of White (1).');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the square_get_column function.
--
CREATE OR REPLACE FUNCTION test_square_populate_addictional_fields() RETURNS VOID AS $$
DECLARE
  entry_count INTEGER;
  rec         RECORD;
BEGIN
  SELECT COUNT(*) INTO STRICT entry_count FROM square_info;
  PERFORM p_assert(64 = entry_count, 'Expected entry_count for square_info is 64.');

  SELECT * INTO STRICT rec FROM square_info WHERE id = 'H4';
  PERFORM p_assert(31 = rec.ordinal, 'Expected ordinal value for H4 is 31.');
  PERFORM p_assert(7 = rec.sq_column, 'Expected sq_column value for H4 is 7.');
  PERFORM p_assert(3 = rec.sq_row, 'Expected sq_row value for H4 is 3.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the square_get_column function.
--
CREATE OR REPLACE FUNCTION test_square_get_column() RETURNS VOID AS $$
BEGIN
  PERFORM p_assert(0 = square_get_column('A1'), 'The column of A1 is 0.');
  PERFORM p_assert(2 = square_get_column('C4'), 'The column of C4 is 2.');
  PERFORM p_assert(7 = square_get_column('H8'), 'The column of H8 is 7.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the square_get_row function.
--
CREATE OR REPLACE FUNCTION test_square_get_row() RETURNS VOID AS $$
BEGIN
  PERFORM p_assert(0 = square_get_row('A1'), 'The row of A1 is 0.');
  PERFORM p_assert(3 = square_get_row('C4'), 'The row of C4 is 3.');
  PERFORM p_assert(7 = square_get_row('H8'), 'The row of H8 is 7.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the square_set_to_string function.
--
CREATE OR REPLACE FUNCTION test_square_set_to_string() RETURNS VOID AS $$
DECLARE
  computed CHAR(64);
  expected CHAR(64);
BEGIN
  expected := '................................................................';
  computed := square_set_to_string(0);
  PERFORM p_assert(expected = computed, 'Expected value is a string of 64 dots.');

  expected := 'x...............................................................';
  computed := square_set_to_string(1);
  PERFORM p_assert(expected = computed, 'Expected value is a string havin one x in the first position and 63 dots.');

  expected := '..xx............................................................';
  computed := square_set_to_string(12);
  PERFORM p_assert(expected = computed, 'Expected value is ..xx......til the end.');

  expected := 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx';
  computed := square_set_to_string(-1);
  PERFORM p_assert(expected = computed, 'Expected value is a string having 64 x chars.');

  expected := 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.';
  computed := square_set_to_string(9223372036854775807);
  PERFORM p_assert(expected = computed, 'Expected value is a string having 63 x chars and a final dot.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the square_set_from_string function.
--
CREATE OR REPLACE FUNCTION test_square_set_from_string() RETURNS VOID AS $$
DECLARE
  computed square_set;
  expected square_set;
BEGIN
  expected := 0;
  computed := square_set_from_string('................................................................');
  PERFORM p_assert(expected = computed, 'Square set must be 0.');

  expected := 1;
  computed := square_set_from_string('x...............................................................');
  PERFORM p_assert(expected = computed, 'Square set must be 1.');

  expected := 12;
  computed := square_set_from_string('..xx............................................................');
  PERFORM p_assert(expected = computed, 'Square set must be 12.');

  expected := -1;
  computed := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  PERFORM p_assert(expected = computed, 'Square set must be -1.');

  expected := 9223372036854775807;
  computed := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.');
  PERFORM p_assert(expected = computed, 'Square set must be 9223372036854775807.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the square_set_to_array function.
--
CREATE OR REPLACE FUNCTION test_square_set_to_array() RETURNS VOID AS $$
BEGIN
  PERFORM p_assert('{"A1"}'       = square_set_to_array(1::square_set), 'Expected result is A1.');
  PERFORM p_assert('{"B1"}'       = square_set_to_array(2::square_set), 'Expected result is B1.');
  PERFORM p_assert('{"A1", "B1"}' = square_set_to_array(3::square_set), 'Expected result is A1, B1.');

  PERFORM p_assert('{}' = square_set_to_array(0::square_set), 'Expected result is an empty array.');

  PERFORM p_assert(64 = array_length(square_set_to_array((-1)::square_set), 1), 'Expected result is 64.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the axis_transform_to_row_one function.
--
CREATE OR REPLACE FUNCTION test_axis_transform_to_row_one() RETURNS VOID AS $$
DECLARE
  computed SMALLINT;
  expected SMALLINT;
  squares  square_set;
BEGIN
  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('HO', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (HO a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('HO', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (HO b)');

  squares := square_set_from_string('xxxxxxxx........................................................');
  expected := 255;
  computed := axis_transform_to_row_one('HO', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (HO c)');

  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (VE a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (VE b)');

  squares := square_set_from_string('x.......x.......x.......x.......x.......x.......x.......x.......');
  expected := 255;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (VE c)');

  squares := square_set_from_string('x.......................x...............x.......x.......x.......');
  expected := 233;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (VE d)');

  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DD a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DD b)');

  squares := square_set_from_string('x........x........x........x........x........x........x........x');
  expected := 255;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DD c)');

  squares := square_set_from_string('x..........................x.................x........x........x');
  expected := 233;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DD d)');

  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DU a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DU b)');

  squares := square_set_from_string('.......x......x......x......x......x......x......x......x.......');
  expected := 255;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DU c)');

  squares := square_set_from_string('.......x....................x.............x......x......x.......');
  expected := 151;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Computed must be different. (DU d)');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the axis_transform_back_from_row_one function.
--
CREATE OR REPLACE FUNCTION test_axis_transform_back_from_row_one() RETURNS VOID AS $$
DECLARE
  bitrow BIGINT;
BEGIN
  bitrow := (x'FF')::BIGINT;
  PERFORM p_assert((x'00000000000000FF')::square_set = axis_transform_back_from_row_one('HO', bitrow), 'Expected value is row_1.');
  PERFORM p_assert((x'0101010101010101')::square_set = axis_transform_back_from_row_one('VE', bitrow), 'Expected value is column_a.');
  PERFORM p_assert((x'8040201008040201')::square_set = axis_transform_back_from_row_one('DD', bitrow), 'Expected value is diagonal_a1_h8.');
  PERFORM p_assert((x'0102040810204080')::square_set = axis_transform_back_from_row_one('DU', bitrow), 'Expected value is diagonal_h1_a8.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the axis_move_ordinal_position_in_bitrow function.
--
CREATE OR REPLACE FUNCTION test_axis_move_ordinal_position_in_bitrow() RETURNS VOID AS $$
DECLARE
  computed    SMALLINT;
  expected    SMALLINT;
  move_column SMALLINT;
  move_row    SMALLINT;
BEGIN
  move_column := 3;
  move_row    := 5;

  expected := 3;
  computed := axis_move_ordinal_position_in_bitrow('HO', move_column, move_row);
  PERFORM p_assert(expected = computed, 'Expected value is 3.');

  expected := 5;
  computed := axis_move_ordinal_position_in_bitrow('VE', move_column, move_row);
  PERFORM p_assert(expected = computed, 'Expected value is 5.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the axis_shift_distance function.
--
CREATE OR REPLACE FUNCTION test_axis_shift_distance() RETURNS VOID AS $$
BEGIN
  PERFORM p_assert(  0 = axis_shift_distance('HO', 0::SMALLINT, 0::SMALLINT), 'Expected value is 0.');
  PERFORM p_assert(  0 = axis_shift_distance('HO', 5::SMALLINT, 0::SMALLINT), 'Expected value is 0.');
  PERFORM p_assert(-16 = axis_shift_distance('HO', 0::SMALLINT, 2::SMALLINT), 'Expected value is -16.');

  PERFORM p_assert(  0 = axis_shift_distance('VE', 0::SMALLINT, 0::SMALLINT), 'Expected value is 0.');
  PERFORM p_assert( -5 = axis_shift_distance('VE', 5::SMALLINT, 0::SMALLINT), 'Expected value is -5.');
  PERFORM p_assert(  0 = axis_shift_distance('VE', 0::SMALLINT, 2::SMALLINT), 'Expected value is 0.');

  PERFORM p_assert(  0 = axis_shift_distance('DD', 0::SMALLINT, 0::SMALLINT), 'Expected value is 0.');
  PERFORM p_assert( 40 = axis_shift_distance('DD', 5::SMALLINT, 0::SMALLINT), 'Expected must be different. (DD b)');
  PERFORM p_assert(-16 = axis_shift_distance('DD', 0::SMALLINT, 2::SMALLINT), 'Expected must be different. (DD c)');

  PERFORM p_assert( 56 = axis_shift_distance('DU', 0::SMALLINT, 0::SMALLINT), 'Expected must be different. (DU a)');
  PERFORM p_assert( 16 = axis_shift_distance('DU', 5::SMALLINT, 0::SMALLINT), 'Expected must be different. (DU b)');
  PERFORM p_assert( 40 = axis_shift_distance('DU', 0::SMALLINT, 2::SMALLINT), 'Expected must be different. (DU c)');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the direction_shift_square_set function.
--
CREATE OR REPLACE FUNCTION test_direction_shift_square_set() RETURNS VOID AS $$
BEGIN
  PERFORM p_assert( 2 = direction_shift_square_set('NE', (x'0000000000000100')::square_set), 'Expected result is 2.');

  PERFORM p_assert((x'00FFFFFFFFFFFFFF')::square_set = direction_shift_square_set('N', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board N.');
  PERFORM p_assert((x'7F7F7F7F7F7F7F7F')::square_set = direction_shift_square_set('W', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board W.');
  PERFORM p_assert((x'FEFEFEFEFEFEFEFE')::square_set = direction_shift_square_set('E', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board E.');
  PERFORM p_assert((x'FFFFFFFFFFFFFF00')::square_set = direction_shift_square_set('S', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board S.');

  PERFORM p_assert((x'007F7F7F7F7F7F7F')::square_set = direction_shift_square_set('NW', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board NW.');
  PERFORM p_assert((x'00FEFEFEFEFEFEFE')::square_set = direction_shift_square_set('NE', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board NE.');
  PERFORM p_assert((x'7F7F7F7F7F7F7F00')::square_set = direction_shift_square_set('SW', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board SW.');
  PERFORM p_assert((x'FEFEFEFEFEFEFE00')::square_set = direction_shift_square_set('SE', (x'FFFFFFFFFFFFFFFF')::square_set), 'Shifting the board SE.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the direction_shift_back_square_set_by_amount function.
--
CREATE OR REPLACE FUNCTION test_direction_shift_back_square_set_by_amount() RETURNS VOID AS $$
DECLARE
  full_set square_set := (x'FFFFFFFFFFFFFFFF')::square_set;
  squares  square_set;
  shift    INTEGER;
BEGIN
  squares := full_set;
  shift   := 7;
  FOR i IN 1..shift LOOP
    squares = direction_shift_square_set('N', squares);
  END LOOP;
  PERFORM p_assert((x'FF00000000000000')::square_set = direction_shift_back_square_set_by_amount('S', squares, shift), 'Expected result is row_8 filled.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the board_populate_bitrow_changes_for_player function.
--
CREATE OR REPLACE FUNCTION test_board_populate_bitrow_changes_for_player() RETURNS VOID AS $$
DECLARE
  entry_count INTEGER;
  computed    SMALLINT;
BEGIN
  SELECT COUNT(*) INTO STRICT entry_count FROM board_bitrow_changes_for_player;
  PERFORM p_assert(524288 = entry_count, 'Expected must be different. (a)');

  SELECT changes INTO STRICT computed FROM board_bitrow_changes_for_player WHERE id = 516;
  PERFORM p_assert(7 = computed, 'Expected value of changes(id=516) is 7.');
 
  SELECT changes INTO STRICT computed FROM board_bitrow_changes_for_player WHERE id = 33388;
  PERFORM p_assert(111 = computed, 'Expected value of changes(id=33388) is 111.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the board_bitrow_changes_for_player function.
--
CREATE OR REPLACE FUNCTION test_board_bitrow_changes_for_player() RETURNS VOID AS $$
DECLARE
  player_row    SMALLINT;
  opponent_row  SMALLINT;
  move_position SMALLINT;
  computed      SMALLINT;
BEGIN
  player_row    := 4;
  opponent_row  := 2;
  move_position := 0;
  computed := board_bitrow_changes_for_player(player_row, opponent_row, move_position);
  PERFORM p_assert(7 = computed, 'Expected value is 7.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_to_string function.
--
CREATE OR REPLACE FUNCTION test_game_position_to_string() RETURNS VOID AS $$
DECLARE
  fixture  RECORD;
  gp       game_position;
  computed CHAR(65);
  expected CHAR(65);
BEGIN
  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'empty';
  gp := fixture.gp;
  computed := game_position_to_string(gp);
--
--            |1       2       3       4       5       6       7       8       .|
--            |ABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHp|
  expected := '................................................................b';
  PERFORM p_assert(expected = computed, 'Computed value must be different. Ref a.');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'initial';
  gp := fixture.gp;
  computed := game_position_to_string(gp);
--
--            |1       2       3       4       5       6       7       8       .|
--            |ABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHp|
  expected := '...........................wb......bw...........................b';
  PERFORM p_assert(expected = computed, 'Computed value must be different. Ref b.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_to_string function.
--
CREATE OR REPLACE FUNCTION test_game_position_from_string() RETURNS VOID AS $$
DECLARE
  gp_string CHAR(65);
  computed  game_position;
  expected  game_position;
BEGIN
  gp_string := '................................................................b';
  computed := game_position_from_string(gp_string);
  expected := (0, 0, 0);
  PERFORM p_assert(expected = computed, 'Computed value must be different. Ref a.');

  gp_string := 'bw..............................................................w';
  computed := game_position_from_string(gp_string);
  expected := (1, 2, 1);
  PERFORM p_assert(expected = computed, 'Computed value must be different. Ref b.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_empties function.
--
CREATE OR REPLACE FUNCTION test_game_position_empties() RETURNS VOID AS $$
DECLARE
  fixture  RECORD;
  gp       game_position;
  computed square_set;
  expected square_set;
BEGIN
  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'empty';
  gp := fixture.gp;
  computed := game_position_empties(gp);
  expected := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  PERFORM p_assert(expected = computed, 'The empty board must have 64 empties.');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'initial';
  gp := fixture.gp;
  computed := game_position_empties(gp);
--
--                                   |1       2       3       4       5       6       7       8       |
--                                   |ABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGH|
  expected := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxx..xxxxxx..xxxxxxxxxxxxxxxxxxxxxxxxxxx');
  PERFORM p_assert(expected = computed, 'The initial board must have 60 empties.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_get_square_set_for_player function.
--
CREATE OR REPLACE FUNCTION test_game_position_get_square_set_for_player_opponent() RETURNS VOID AS $$
DECLARE
  blacks square_set := 1::square_set;
  whites square_set := 2::square_set;
  player player     := 1::player;
  gp     game_position;
BEGIN
  gp := (blacks, whites, player);
  PERFORM p_assert(whites = game_position_get_square_set_for_player(gp),   'Expected result is 2::square_set.');
  PERFORM p_assert(blacks = game_position_get_square_set_for_opponent(gp), 'Expected result is 1::square_set.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_is_move_legal function.
--
CREATE OR REPLACE FUNCTION test_game_position_is_move_legal() RETURNS VOID AS $$
DECLARE
  gp_string CHAR(65);
BEGIN
  PERFORM p_assert(TRUE  = game_position_is_move_legal((1::square_set, 2::square_set, 0::SMALLINT), 'C1'), 'C1 is a valid move.');
  PERFORM p_assert(FALSE = game_position_is_move_legal((1::square_set, 2::square_set, 0::SMALLINT), 'A1'), 'A1 is already occupied.');
  PERFORM p_assert(FALSE = game_position_is_move_legal((1::square_set, 2::square_set, 0::SMALLINT), 'D1'), 'D1 is not a legal move.');

  gp_string := 'w.......b.......b.......b.......b.......b.......b...............w';
  PERFORM p_assert(TRUE  = game_position_is_move_legal(game_position_from_string(gp_string), 'A8'), 'A8 is a valid move.');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_legal_moves function.
--
CREATE OR REPLACE FUNCTION test_game_position_legal_moves() RETURNS VOID AS $$
DECLARE
  fixture  RECORD;
  computed square_set;
  expected square_set;
BEGIN
  PERFORM p_assert(4 = game_position_legal_moves((1, 2, 0)::game_position), 'Expected square set is equal to 4.');
  PERFORM p_assert(8 = game_position_legal_moves((1, 6, 0)::game_position), 'Expected square set is equal to 8.');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'empty';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert(0 = computed, 'Expected square set is equal to 0.');
 
  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'initial';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert(17729692631040 = computed, 'Expected square set is equal to D3(19), C4(26), F5(37), E6(44), or 2^19+2^26+2^27+2^44.');
  PERFORM p_assert('{"D3", "C4", "F5", "E6"}' = square_set_to_array(computed), 'Expected array is equal to {"D3", "C4", "F5", "E6"}');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'first-move-d3';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert('{"C3", "E3", "C5"}' = square_set_to_array(computed), 'Expected array is equal to {"C3", "E3", "C5"}');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'black-has-to-pass';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert('{}' = square_set_to_array(computed), 'Expected array is equal to {}');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'early-game-b-9-moves';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert('{"C3", "C6"}' = square_set_to_array(computed), 'Expected array is equal to {"C3", "C6"}');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'final-b37-w27';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert('{}' = square_set_to_array(computed), 'Expected array is equal to {}');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'make-move-test-case-a-before';
  computed := game_position_legal_moves(fixture.gp);
  PERFORM p_assert('{"D4"}' = square_set_to_array(computed), 'Expected array is equal to {"D4"}');

END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_make_move function.
--
CREATE OR REPLACE FUNCTION test_game_position_make_move() RETURNS VOID AS $$
DECLARE
  fixture_before RECORD;
  fixture_after  RECORD;
  computed game_position;
BEGIN
  SELECT * INTO STRICT fixture_before FROM game_position_test_data WHERE id = 'make-move-test-case-a-before';
  SELECT * INTO STRICT fixture_after  FROM game_position_test_data WHERE id = 'make-move-test-case-a-after';
  computed := game_position_make_move(fixture_before.gp, 'D4');
  PERFORM p_assert(fixture_after.gp = computed, 'Expected game position is equal to make-move-test-case-a-after');

  SELECT * INTO STRICT fixture_before FROM game_position_test_data WHERE id = 'make-move-test-case-b-before';
  SELECT * INTO STRICT fixture_after  FROM game_position_test_data WHERE id = 'make-move-test-case-b-after';
  computed := game_position_make_move(fixture_before.gp, 'D4');
  PERFORM p_assert(fixture_after.gp = computed, 'Expected game position is equal to make-move-test-case-b-after');

  SELECT * INTO STRICT fixture_before FROM game_position_test_data WHERE id = 'make-move-test-case-c-before';
  SELECT * INTO STRICT fixture_after  FROM game_position_test_data WHERE id = 'make-move-test-case-c-after';
  computed := game_position_make_move(fixture_before.gp, 'D4');
  PERFORM p_assert(fixture_after.gp = computed, 'Expected game position is equal to make-move-test-case-c-after');

  SELECT * INTO STRICT fixture_before FROM game_position_test_data WHERE id = 'make-move-test-case-d-before';
  SELECT * INTO STRICT fixture_after  FROM game_position_test_data WHERE id = 'make-move-test-case-d-after';
  computed := game_position_make_move(fixture_before.gp, 'B4');
  PERFORM p_assert(fixture_after.gp = computed, 'Expected game position is equal to make-move-test-case-d-after');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the game_position_pp function.
--
CREATE OR REPLACE FUNCTION test_game_position_pp() RETURNS VOID AS $$
DECLARE
  new_line TEXT := E'\n';

  fixture  RECORD;
  expected TEXT;
BEGIN
  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'black-has-to-pass';
  expected := '';
  expected := expected || '   a b c d e f g h ' || new_line;
  expected := expected || '1  @ O . O . @ . . ' || new_line;
  expected := expected || '2  O O O O O O O @ ' || new_line;
  expected := expected || '3  . O @ @ O O @ @ ' || new_line;
  expected := expected || '4  . O @ O @ @ @ @ ' || new_line;
  expected := expected || '5  . O @ O @ @ @ @ ' || new_line;
  expected := expected || '6  . O @ O O @ O @ ' || new_line;
  expected := expected || '7  . O @ O O O O . ' || new_line;
  expected := expected || '8  @ @ @ @ @ @ O @ ' || new_line;
  expected := expected || 'Player to move: BLACK';
  PERFORM p_assert(expected = game_position_pp(fixture.gp), 'Computed does not match with expected.');
END;
$$ LANGUAGE plpgsql;
