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
-- Tests the axis_shift_distance function.
--
CREATE OR REPLACE FUNCTION test_axis_shift_distance() RETURNS VOID AS $$
DECLARE
BEGIN
  PERFORM p_assert(  0 = axis_shift_distance('HO', 0, 0), 'Expected must be different. (HO a)');
  PERFORM p_assert(  0 = axis_shift_distance('HO', 5, 0), 'Expected must be different. (HO b)');
  PERFORM p_assert(-16 = axis_shift_distance('HO', 0, 2), 'Expected must be different. (HO c)');

  PERFORM p_assert(  0 = axis_shift_distance('VE', 0, 0), 'Expected must be different. (VE a)');
  PERFORM p_assert( -5 = axis_shift_distance('VE', 5, 0), 'Expected must be different. (VE b)');
  PERFORM p_assert(  0 = axis_shift_distance('VE', 0, 2), 'Expected must be different. (VE c)');

  PERFORM p_assert(  0 = axis_shift_distance('DD', 0, 0), 'Expected must be different. (DD a)');
  PERFORM p_assert( 40 = axis_shift_distance('DD', 5, 0), 'Expected must be different. (DD b)');
  PERFORM p_assert(-16 = axis_shift_distance('DD', 0, 2), 'Expected must be different. (DD c)');

  PERFORM p_assert( 56 = axis_shift_distance('DU', 0, 0), 'Expected must be different. (DU a)');
  PERFORM p_assert( 16 = axis_shift_distance('DU', 5, 0), 'Expected must be different. (DU b)');
  PERFORM p_assert( 40 = axis_shift_distance('DU', 0, 2), 'Expected must be different. (DU c)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 5;
  computed := axis_move_ordinal_position_in_bitrow('VE', move_column, move_row);
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (HO a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('HO', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (HO b)');

  squares := square_set_from_string('xxxxxxxx........................................................');
  expected := 255;
  computed := axis_transform_to_row_one('HO', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (HO c)');

  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (VE a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (VE b)');

  squares := square_set_from_string('x.......x.......x.......x.......x.......x.......x.......x.......');
  expected := 255;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (VE c)');

  squares := square_set_from_string('x.......................x...............x.......x.......x.......');
  expected := 233;
  computed := axis_transform_to_row_one('VE', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (VE d)');

  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DD a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DD b)');

  squares := square_set_from_string('x........x........x........x........x........x........x........x');
  expected := 255;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DD c)');

  squares := square_set_from_string('x..........................x.................x........x........x');
  expected := 233;
  computed := axis_transform_to_row_one('DD', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DD d)');

  squares := square_set_from_string('................................................................');
  expected := 0;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DU a)');

  squares := square_set_from_string('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx');
  expected := 255;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DU b)');

  squares := square_set_from_string('.......x......x......x......x......x......x......x......x.......');
  expected := 255;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DU c)');

  squares := square_set_from_string('.......x....................x.............x......x......x.......');
  expected := 151;
  computed := axis_transform_to_row_one('DU', squares);
  PERFORM p_assert(expected = computed, 'Expected must be different. (DU d)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 0;
  computed := bit_works_signed_left_shift(CAST (0 AS BIGINT), -1);
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');

  expected := 2;
  computed := bit_works_signed_left_shift(CAST (1 AS BIGINT), 1);
  PERFORM p_assert(expected = computed, 'Expected must be different. (c)');

  expected := 6;
  computed := bit_works_signed_left_shift(CAST (24 AS BIGINT), -2);
  PERFORM p_assert(expected = computed, 'Expected must be different. (d)');

  expected := -9223372036854775808;
  computed := bit_works_signed_left_shift(CAST (1 AS BIGINT), 63);
  PERFORM p_assert(expected = computed, 'Expected must be different. (e)');

  expected := 1;
  computed := bit_works_signed_left_shift(CAST (-9223372036854775808 AS BIGINT), -63);
  PERFORM p_assert(expected = computed, 'Expected must be different. (f)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 1;
  computed := bit_works_lowest_bit_set_8(1::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');

  expected := 2;
  computed := bit_works_lowest_bit_set_8(2::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected must be different. (c)');

  expected := 1;
  computed := bit_works_lowest_bit_set_8(3::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected must be different. (d)');

  expected := 1;
  computed := bit_works_lowest_bit_set_8(255::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected must be different. (e)');

  expected := 128;
  computed := bit_works_lowest_bit_set_8(128::SMALLINT);
  PERFORM p_assert(expected = computed, 'Expected must be different. (f)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 1;
  computed := bit_works_highest_bit_set_8(CAST (1 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');

  expected := 1;
  computed := bit_works_highest_bit_set_8(CAST (1 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (c)');

  expected := 2;
  computed := bit_works_highest_bit_set_8(CAST (2 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (d)');

  expected := 2;
  computed := bit_works_highest_bit_set_8(CAST (3 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (e)');

  expected := 128;
  computed := bit_works_highest_bit_set_8(CAST (128 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (f)');

  expected := 128;
  computed := bit_works_highest_bit_set_8(CAST (255 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (g)');
END;
$$ LANGUAGE plpgsql;



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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 1;
  computed := bit_works_bitscanMS1B_8(CAST (2 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');

  expected := 7;
  computed := bit_works_bitscanMS1B_8(CAST (128 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (c)');

  expected := 2;
  computed := bit_works_bitscanMS1B_8(CAST (6 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (d)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (128 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');

  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (8 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (c)');

  expected := 0;
  computed := bit_works_fill_in_between_8(CAST (3 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (d)');

  expected := 8;
  computed := bit_works_fill_in_between_8(CAST (20 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (e)');

  expected := 126;
  computed := bit_works_fill_in_between_8(CAST (129 AS SMALLINT));
  PERFORM p_assert(expected = computed, 'Expected must be different. (f)');
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
-- Tests the square_set_to_string function.
--
CREATE OR REPLACE FUNCTION test_square_set_to_string() RETURNS VOID AS $$
DECLARE
  computed CHAR(64);
  expected CHAR(64);
BEGIN
  expected := '................................................................';
  computed := square_set_to_string(0);
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  expected := 'x...............................................................';
  computed := square_set_to_string(1);
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');

  expected := '..xx............................................................';
  computed := square_set_to_string(12);
  PERFORM p_assert(expected = computed, 'Expected must be different. (c)');

  expected := 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx';
  computed := square_set_to_string(-1);
  PERFORM p_assert(expected = computed, 'Expected must be different. (d)');

  expected := 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.';
  computed := square_set_to_string(9223372036854775807);
  PERFORM p_assert(expected = computed, 'Expected must be different. (e)');
END;
$$ LANGUAGE plpgsql;



--
-- Tests the player_to_string function.
--
CREATE OR REPLACE FUNCTION test_player_to_string() RETURNS VOID AS $$
DECLARE
  computed CHAR(1);
  expected CHAR(1);
  pl       player;
BEGIN
  pl := 0;
  expected := 'b';
  computed := player_to_string(pl);
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  pl := 1;
  expected := 'w';
  computed := player_to_string(pl);
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');

  SELECT * INTO STRICT fixture FROM game_position_test_data WHERE id = 'initial';
  gp := fixture.gp;
  computed := game_position_to_string(gp);
--
--            |1       2       3       4       5       6       7       8       .|
--            |ABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHp|
  expected := '...........................wb......bw...........................b';
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');
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
  PERFORM p_assert(expected = computed, 'Expected must be different. (a)');
  
  gp_string := 'bw..............................................................w';
  computed := game_position_from_string(gp_string);
  expected := (1, 2, 1);
  PERFORM p_assert(expected = computed, 'Expected must be different. (b)');
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
