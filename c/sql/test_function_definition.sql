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

