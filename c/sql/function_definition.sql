--
-- function_definition.sql
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
-- Load the file by running the command: \i function_definition.sql
--
--
-- This script creates the functions used by the reversi program.
--

SET search_path TO reversi;


--
-- Realizes the assert statement.
--
CREATE OR REPLACE FUNCTION p_assert(p_assertion BOOLEAN, p_message_on_error VARCHAR(255)) RETURNS VOID AS $$
BEGIN
  IF p_assertion IS NULL THEN
    RAISE EXCEPTION 'Assertion is null, that is not supported.';
  END IF;
  IF NOT p_assertion THEN
    RAISE EXCEPTION '%', p_message_on_error;
  END IF;
END;
$$ LANGUAGE plpgsql;


--
-- Returns true if the move is legal.
--
CREATE OR REPLACE FUNCTION game_position_is_move_legal(blacks BIGINT, whites BIGINT, player SMALLINT, move Square) RETURNS BOOLEAN AS $$
DECLARE
  bit_move              BIGINT;
  move_ordinal          INT;
  empties               BIGINT;
  empty_square_set      BIGINT;
  p_square_set          BIGINT;
  o_square_set          BIGINT;
  move_column           INT;
  move_row              INT;
  axis                  RECORD;
  move_ordinal_position INT;
  shift_distance        INT;
  p_bitrow              SMALLINT;
  o_bitrow              SMALLINT;
BEGIN
  PERFORM p_assert(player = 0 OR player = 1, 'Parameter player must be in the range 0..1.');
  SELECT ordinal INTO STRICT move_ordinal FROM square_info WHERE id = move;
  empty_square_set := CAST(0 AS BIGINT);
  bit_move := CAST(1 AS BIGINT) << move_ordinal;
  empties := empties(blacks, whites);
  IF (empties & bit_move) = empty_square_set THEN RETURN FALSE; END IF;
  IF player = 0 THEN
    p_square_set := blacks;
    o_square_set := whites;
  ELSE
    p_square_set := whites;
    o_square_set := blacks;
  END IF;
  move_column := move_ordinal % 8;
  move_row    := move_ordinal / 8;
  RAISE NOTICE 'move_ordinal=%, move_column=%, move_row=%',move_ordinal, move_column, move_row ;
  FOR axis IN SELECT id, ordinal FROM axis_info ORDER BY ordinal LOOP
    move_ordinal_position := axis_move_ordinal_position_in_bitrow(axis.id, move_column, move_row);
    shift_distance := axis_shift_distance(axis.id, move_column, move_row);
    RAISE NOTICE 'axis.id=%, move_ordinal_position=%, shift_distance=%', axis.id, move_ordinal_position, shift_distance;
    p_bitrow := axis_transform_to_row_one(axis.id, bit_works_signed_left_shift(p_square_set, shift_distance));
    o_bitrow := axis_transform_to_row_one(axis.id, bit_works_signed_left_shift(o_square_set, shift_distance));
    RAISE NOTICE 'p_bitrow=%, o_bitrow=%', p_bitrow, o_bitrow;
  END LOOP;
  RETURN TRUE;
END
$$ LANGUAGE plpgsql;


--
-- Returns an 8-bit row representation of the player pieces after applying the move.
--
CREATE OR REPLACE FUNCTION board_bitrow_changes_for_player(player_row SMALLINT, opponent_row SMALLINT, move_position SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  bitrow_changes_for_player_index INT;
  result SMALLINT;
BEGIN
  bitrow_changes_for_player_index := player_row | (opponent_row << 8) | (CAST (move_position AS INT) << 16);
  SELECT changes INTO STRICT result FROM bitrow_changes_for_player WHERE id = bitrow_changes_for_player_index;
  RETURN result;
END
$$ LANGUAGE plpgsql;

--
-- Returns a value computed shifting the `bit_sequence` parameter
-- to left by a signed amount given by the `shift` parameter.
--
CREATE OR REPLACE FUNCTION bit_works_signed_left_shift(bit_sequence BIGINT, shift INT) RETURNS BIGINT AS $$
DECLARE
BEGIN
  IF shift >= 0 THEN
    RETURN bit_sequence << shift;
  ELSE
    RETURN bit_sequence >> -shift;
  END IF;
END
$$ LANGUAGE plpgsql;


--
-- Maps the principal line of each axis into row one.
--
CREATE OR REPLACE FUNCTION axis_transform_to_row_one(axis Axis, square_set BIGINT) RETURNS SMALLINT AS $$
DECLARE
  result         BIGINT;
  column_a       BIGINT;
  diagonal_a1_h8 BIGINT;
  diagonal_h1_a8 BIGINT;
BEGIN
  column_a       := CAST (x'0101010101010101' AS BIGINT);
  diagonal_a1_h8 := CAST (x'8040201008040201' AS BIGINT);
  diagonal_h1_a8 := CAST (x'0102040810204080' AS BIGINT);
  result := square_set;
  IF axis = 'HO' THEN
    result := result;
  ELSEIF axis = 'VE' THEN
    result := result & column_a;
    result := result | (result >> 28);
    result := result | (result >> 14);
    result := result | (result >>  7);
  ELSEIF axis = 'DD' THEN
    result := result & diagonal_a1_h8;
    result := result | (result >> 32);
    result := result | (result >> 16);
    result := result | (result >>  8);
  ELSEIF axis = 'DU' THEN
    result := result & diagonal_h1_a8;
    result := result | (result >> 32);
    result := result | (result >> 16);
    result := result | (result >>  8);
  ELSE
    RAISE EXCEPTION 'Parameter axis out of range.';
  END IF;
  RETURN CAST ((result & 255) AS SMALLINT);
END
$$ LANGUAGE plpgsql;


--
-- Returns the ordinal position of the move.
--
CREATE OR REPLACE FUNCTION axis_move_ordinal_position_in_bitrow(axis Axis, move_column INT, move_row INT) RETURNS INT AS $$
BEGIN
  IF  axis = 'VE' THEN
    RETURN move_row;
  END IF;
  RETURN move_column;
END
$$ LANGUAGE plpgsql;


--
-- Computes the shift quantity.
--
CREATE OR REPLACE FUNCTION axis_shift_distance(axis Axis, move_column INT, move_row INT) RETURNS INT AS $$
BEGIN
  IF axis = 'HO' THEN
    RETURN -move_row << 3;
  ELSEIF axis = 'VE' THEN
    RETURN -move_column;
  ELSEIF axis = 'DD' THEN
    RETURN (move_column - move_row) << 3;
  ELSEIF axis = 'DU' THEN
    RETURN (7 - move_column - move_row) << 3;
  ELSE
    RAISE EXCEPTION 'Parameter axis out of range.';
  END IF;
END
$$ LANGUAGE plpgsql;


--
-- Returns a string describing the game position state.
--
CREATE OR REPLACE FUNCTION game_position_to_string(blacks BIGINT, whites BIGINT, player SMALLINT) RETURNS CHAR(66) AS $$
DECLARE
  ret  CHAR(66);
BEGIN
  ret := board_to_string(blacks, whites) || ';' || player_to_string(player);
  RETURN ret;
END;
$$ LANGUAGE plpgsql;


--
-- Returns a string describing the player.
--
CREATE OR REPLACE FUNCTION player_to_string(player SMALLINT) RETURNS CHAR(1) AS $$
DECLARE
  ret  CHAR(1);
BEGIN
  IF player = CAST(0 AS SMALLINT) THEN
      ret := 'b';
  ELSE
      ret := 'w';
  END IF;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a string describing the board state.
--
CREATE OR REPLACE FUNCTION board_to_string(blacks BIGINT, whites BIGINT) RETURNS CHAR(64) AS $$
DECLARE
  ret  CHAR(64);
  mask BIGINT;
BEGIN
  ret := '';
  FOR i IN 0..63 LOOP
    mask := CAST(1 AS BIGINT) << i;
    IF mask & blacks <> 0 THEN
      ret := ret || 'b';
    ELSIF mask & whites <> 0 THEN
      ret := ret || 'w';
    ELSE
      ret := ret || '.';
    END IF;
  END LOOP;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a string describing the square set.
--
CREATE OR REPLACE FUNCTION square_set_to_string(square_set BIGINT) RETURNS CHAR(64) AS $$
DECLARE
  ret CHAR(64);
BEGIN
  ret := '';
  FOR i IN 0..63 LOOP
    IF (CAST(1 AS BIGINT) << i) & square_set <> 0 THEN
      ret := ret || 'x';
    ELSE
      ret := ret || '_';
    END IF;
  END LOOP;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the set of empty squares.
--
CREATE OR REPLACE FUNCTION empties(blacks BIGINT, whites BIGINT) RETURNS BIGINT AS $$
DECLARE
  empties BIGINT;
BEGIN
  empties := ~(blacks | whites);
  RETURN empties;
END;
$$ LANGUAGE plpgsql;
