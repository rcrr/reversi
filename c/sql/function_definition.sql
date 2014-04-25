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
-- Returns the index (0..7) of the most significant bit set in the bit_sequence parameter.
--
CREATE OR REPLACE FUNCTION bit_works_bitscanMS1B_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  log2_array SMALLINT[] := '{ 0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                              5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
                            }';
  result  SMALLINT;
  masked  SMALLINT;
BEGIN
  masked := bit_sequence & CAST (255 AS SMALLINT);
  result := log2_array[masked + 1];
  RETURN result;
END
$$ LANGUAGE plpgsql;



--
-- Returns a bit sequence having set the bits between the two, or zero
-- when only one bit is set.
--
-- Bits higher than 8 (the second byte of the smallint) are masked to 0.
--
-- The bitsequence parameter must have one or two bits set.
--
-- For example: 00100010 returns 00011100.
--
-- When the input data doesn't meet the requirements the result is unpredictable.
--
CREATE OR REPLACE FUNCTION bit_works_fill_in_between_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  res     SMALLINT;
  tmp     SMALLINT;
  mask    SMALLINT;
  masked  SMALLINT;
BEGIN
  mask := CAST (255 AS SMALLINT);
  masked := bit_sequence & mask;
  res := ~masked & mask;
  tmp := masked - 1;
  res := ((res # tmp) & mask);
  res := ((1 << bit_works_bitscanMS1B_8(masked)) - 1) & res;
  RETURN res;
END
$$ LANGUAGE plpgsql;



--
-- Returns an int value having all the bit set in bit_sequence turned to 0
-- except the most significant one.
--
-- When parameter bit_sequence is equal to 0 it returns 0.
--
-- Bits higher than 8 (the second byte of the smallint) are masked to 0.
--
CREATE OR REPLACE FUNCTION bit_works_highest_bit_set_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  log2_array SMALLINT[] := '{ 0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
                              5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                              6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
                              7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
                            }';
  result  SMALLINT;
  masked  SMALLINT;
BEGIN
  masked := bit_sequence & CAST (255 AS SMALLINT);
  IF masked = 0 THEN
    result := 0;
  ELSE
    result := 1 << log2_array[masked + 1];
  END IF;
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
-- Returns a string describing the player.
-- Tests written.
--
CREATE OR REPLACE FUNCTION player_to_string(pl player) RETURNS CHAR(1) AS $$
DECLARE
  ret  CHAR(1);
BEGIN
  IF pl = 0 THEN
      ret := 'b';
  ELSE
      ret := 'w';
  END IF;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a string describing the square set.
-- Tests written.
--
CREATE OR REPLACE FUNCTION square_set_to_string(squares square_set) RETURNS CHAR(64) AS $$
DECLARE
  ret CHAR(64);
BEGIN
  ret := '';
  FOR i IN 0..63 LOOP
    IF (CAST(1 AS square_set) << i) & squares <> 0 THEN
      ret := ret || 'x';
    ELSE
      ret := ret || '.';
    END IF;
  END LOOP;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a square_set from the given string.
-- Tests written.
--
CREATE OR REPLACE FUNCTION square_set_from_string(ss_string CHAR(64)) RETURNS square_set AS $$
DECLARE
  squares square_set;
  c      char;
  i      int;
  square square_set;
BEGIN
  IF length(ss_string) <> 64 THEN
    RAISE EXCEPTION 'Value of length(ss_string) is %, it must be 64!', length(ss_string);
  END IF;
  squares := 0;
  i := 0;
  FOREACH c IN ARRAY string_to_array(ss_string, NULL)
  LOOP
    square = CAST (1 AS square_set) << i;
    IF (c = 'x') THEN
      squares := squares | square;
    ELSEIF (c = '.') THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'Square must be either x or ., it is equal to "%".', c;
    END IF;
    i := i + 1;
  END LOOP;
  RETURN squares;
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
-- The board_bitrow_changes_for_player collects the precomputed effects of moving
-- a piece in any of the eigth squares in a row.
-- The size is so computed:
--  - there are 256 arrangments of player discs,
--  - and 256 arrangements of opponent pieces,
--  - the potential moves are 8.
-- So the number of entries is 256 * 256 * 8 = 524,288 records = 512k records.
-- Not all the entries are legal! The first set of eigth bits and the second one (opponent row)
-- must not set the same position.
--
-- The index of the array is computed by this formula:
-- index = playerRow | (opponentRow << 8) | (movePosition << 16);
--
-- After initialization the table is never changed.
--
-- DROP TABLE IF EXISTS board_bitrow_changes_for_player;
--
CREATE TABLE board_bitrow_changes_for_player(id      INTEGER,
                                             changes SMALLINT,
                                             PRIMARY KEY(id));

--
-- Populates the table board_bitrow_changes_for_player.
--
CREATE OR REPLACE FUNCTION board_populate_bitrow_changes_for_player() RETURNS VOID AS $$
DECLARE
  player_row_count      INTEGER;
  opponent_row_count    INTEGER;
  move_position         INTEGER;
  player_row            SMALLINT;
  opponent_row          SMALLINT;
  filled_in_row         SMALLINT;
  empties_in_row        SMALLINT;
  game_move             SMALLINT;
  table_id              INTEGER;
  player_row_after_move SMALLINT;
  potential_bracketing_disc_on_the_left SMALLINT;
  left_rank             SMALLINT;
BEGIN
  FOR player_row_count IN 0..255 LOOP
    player_row := CAST (player_row_count AS SMALLINT);
    FOR opponent_row_count IN 0..255 LOOP
      opponent_row := CAST (opponent_row_count AS SMALLINT);
      filled_in_row := player_row | opponent_row;
      empties_in_row := (~filled_in_row) & CAST (255 AS SMALLINT);
      FOR move_position IN 0..7 LOOP
        game_move := 1 << move_position;
        table_id := player_row_count | (opponent_row_count << 8) | (move_position << 16);
        IF (player_row & opponent_row <> 0) OR (game_move & filled_in_row <> 0) THEN
          player_row_after_move := player_row;
        ELSE
        
          -- The square of the move is added to the player configuration of the row after the move.
          player_row_after_move := player_row | game_move;

           -- The potential bracketing disc on the right is the first player disc found moving
           -- on the left starting from the square of the move.
          potential_bracketing_disc_on_the_left := bit_works_highest_bit_set_8(CAST (player_row & (game_move - 1) AS SMALLINT));

           -- The left rank is the sequence of adiacent discs that start from the bracketing disc and end
           -- with the move disc.
          left_rank := bit_works_fill_in_between_8(potential_bracketing_disc_on_the_left | game_move);


          -- MUST BE CONPLETED WITH THE FLIPPING LOGIC!!!! --
          player_row_after_move := 0;
        END IF;
        INSERT INTO board_bitrow_changes_for_player (id, changes) VALUES (table_id, player_row_after_move);
      END LOOP;
    END LOOP;
  END LOOP;
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
-- Returns a string describing the board state.
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_to_string(gp game_position) RETURNS CHAR(65) AS $$
DECLARE
  ret    CHAR(65);
  mask   BIGINT;
  blacks square_set;
  whites square_set;
  pl     player;
BEGIN
  blacks := gp.blacks;
  whites := gp.whites;
  pl     := gp.player;
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
  ret := ret || player_to_string(pl);
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a game position from the given string.
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_from_string(gp_string CHAR(65)) RETURNS game_position AS $$
DECLARE
  blacks square_set;
  whites square_set;
  p      player;
  c      char;
  i      int;
  square square_set;
BEGIN
  IF length(gp_string) <> 65 THEN
    RAISE EXCEPTION 'Value of length(gp_string) is %, it must be 65!', length(gp_string);
  END IF;
  blacks := 0;
  whites := 0;
  p := 0;
  i := 0;
  FOREACH c IN ARRAY string_to_array(gp_string, NULL)
  LOOP
    IF (i = 64) THEN
      IF (c = 'b') THEN
        p := 0;
      ELSEIF (c = 'w') THEN
        p := 1;
      ELSE
        RAISE EXCEPTION 'Player must be either b or w, it is equal to "%".', c;
      END IF;
    ELSE
      square = CAST (1 AS square_set) << i;
      IF (c = 'b') THEN
        blacks := blacks | square;
      ELSEIF (c = 'w') THEN
        whites := whites | square;
      ELSEIF (c = '.') THEN
        NULL;
      ELSE
        RAISE EXCEPTION 'Square color must be either b or w or ., it is equal to "%".', c;
      END IF;
    END IF;
    i := i + 1;
  END LOOP;
  RETURN (blacks, whites, p);
END
$$ LANGUAGE plpgsql;



--
-- Returns the set of empty squares.
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_empties(gp game_position) RETURNS square_set AS $$
DECLARE
  empties square_set;
BEGIN
  empties := ~(gp.blacks | gp.whites);
  RETURN empties;
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
  empties := game_position_empties(blacks, whites);
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
  RAISE NOTICE 'move_ordinal=%, move_column=%, move_row=%',move_ordinal, move_column, move_row;
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
