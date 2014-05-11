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
-- Tests written.
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
  res    SMALLINT;
  masked SMALLINT;
BEGIN
  masked := bit_sequence & CAST (255 AS SMALLINT);
  res := log2_array[masked + 1];
  RETURN res;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the index of the least significant bit set in the bit_sequence parameter.
--
-- Parameter bit_sequence must be different from 0.
-- If no bit set is found, meaning that bit_sequence is equal to 0, 64 is
-- returned, that is clearly a wrong value.
--
CREATE OR REPLACE FUNCTION bit_works_bitscanLS1B_64(bit_sequence BIGINT) RETURNS SMALLINT AS $$
DECLARE
  tmp    BIGINT   := bit_sequence;
  ret    SMALLINT := 0;
  mask_1 BIGINT   := (x'00000000FFFFFFFF')::BIGINT;
  mask_2 BIGINT   := (x'000000000000FFFF')::BIGINT;
  mask_3 BIGINT   := (x'00000000000000FF')::BIGINT;
  mask_4 BIGINT   := (x'000000000000000F')::BIGINT;
  mask_5 BIGINT   := (x'0000000000000007')::BIGINT;
  mask_6 BIGINT   := (x'0000000000000003')::BIGINT;
  mask_7 BIGINT   := (x'0000000000000001')::BIGINT;
BEGIN
  IF (tmp & mask_1) = 0 THEN
   ret := ret + 32;
   tmp := tmp >> 32;
  END IF;
  IF (tmp & mask_2) = 0 THEN
   ret := ret + 16;
   tmp := tmp >> 16;
  END IF;
  IF (tmp & mask_3) = 0 THEN
   ret := ret + 8;
   tmp := tmp >> 8;
  END IF;
  IF (tmp & mask_4) = 0 THEN
   ret := ret + 4;
   tmp := tmp >> 4;
  END IF;
  IF (tmp & mask_5) = 0 THEN
   ret := ret + 2;
   tmp := tmp >> 2;
  END IF;
  IF (tmp & mask_6) = 0 THEN
   ret := ret + 1;
   tmp := tmp >> 1;
  END IF;
  IF (tmp & mask_7) = 0 THEN
   ret := ret + 1;
   tmp := tmp >> 1;
  END IF;
  RETURN ret;
END;
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
-- Tests written.
--
CREATE OR REPLACE FUNCTION bit_works_fill_in_between_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  mask    SMALLINT;
  masked  SMALLINT;
BEGIN
  mask := CAST (255 AS SMALLINT);
  masked := bit_sequence & mask;
  RETURN ((1 << bit_works_bitscanMS1B_8(masked)) - 1) & ((~masked # (masked - 1)) & mask);
END;
$$ LANGUAGE plpgsql;



--
-- Returns an int value having all the bit set in bit_sequence turned to 0
-- except the most significant one.
--
-- When parameter bit_sequence is equal to 0 it returns 0.
--
-- Bits higher than 8 (the second byte of the smallint) are masked to 0.
--
-- Tests written.
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
  res    SMALLINT;
  masked SMALLINT;
BEGIN
  masked := bit_sequence & CAST (255 AS SMALLINT);
  IF masked = 0 THEN
    res := 0;
  ELSE
    res := 1 << log2_array[masked + 1];
  END IF;
  RETURN res;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a bit sequence having one bit set, the lowest found
-- in the bit_sequence parameter.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION bit_works_lowest_bit_set_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  mask   SMALLINT;
  masked SMALLINT;
BEGIN
  mask := 255::SMALLINT;
  masked := bit_sequence & mask;
  RETURN ((masked & (masked - 1)) # masked) & mask;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a value computed shifting the bit_sequence parameter
-- to left by a signed amount given by the shift parameter.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION bit_works_signed_left_shift(bit_sequence BIGINT, shift INTEGER) RETURNS BIGINT AS $$
BEGIN
  IF shift >= 0 THEN
    RETURN (bit_sequence::BIT(64) << +shift)::BIGINT;
  ELSE
    RETURN (bit_sequence::BIT(64) >> -shift)::BIGINT;
  END IF;
END;
$$ LANGUAGE plpgsql;


--
-- Returns a string describing the palyer.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION player_to_string(player player) RETURNS CHAR(1) AS $$
DECLARE
  ret CHAR(1);
BEGIN
  IF player = 0 THEN
    ret := 'b';
  ELSE
    ret := 'w';
  END IF;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the player's opponent.
--
CREATE OR REPLACE FUNCTION player_opponent(player player) RETURNS player AS $$
BEGIN
  RETURN 1 - player;
END;
$$ LANGUAGE plpgsql;



--
-- Populates the table square_info with sq_column and sq_row fields.
--
CREATE OR REPLACE FUNCTION square_populate_addictional_fields() RETURNS VOID AS $$
DECLARE
  r square_info;
BEGIN
  FOR r IN SELECT * FROM square_info LOOP
    UPDATE square_info SET sq_column = r.ordinal % 8, sq_row = r.ordinal / 8
    WHERE id = r.id;
  END LOOP;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the column ordinal of the square.
--
CREATE OR REPLACE FUNCTION square_get_column(sq square) RETURNS SMALLINT AS $$
DECLARE
  ret SMALLINT;
BEGIN
  SELECT sq_column INTO STRICT ret FROM square_info WHERE sq = id;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the row ordinal of the square.
--
CREATE OR REPLACE FUNCTION square_get_row(sq square) RETURNS SMALLINT AS $$
DECLARE
  ret SMALLINT;
BEGIN
  SELECT sq_row INTO STRICT ret FROM square_info WHERE sq = id;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a string describing the square set.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION square_set_to_string(squares square_set) RETURNS CHAR(64) AS $$
DECLARE
  ret CHAR(64);
BEGIN
  ret := '';
  FOR i IN 0..63 LOOP
    IF (1::square_set << i) & squares <> 0 THEN
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
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION square_set_from_string(ss_string CHAR(64)) RETURNS square_set AS $$
DECLARE
  square  square_set := 0;
  i       INTEGER    := 0;
  squares square_set := 0;
  c       CHAR;
BEGIN
  IF length(ss_string) <> 64 THEN
    RAISE EXCEPTION 'Value of length(ss_string) is %, it must be 64!', length(ss_string);
  END IF;
  FOREACH c IN ARRAY string_to_array(ss_string, NULL)
  LOOP
    square = 1::square_set << i;
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
END;
$$ LANGUAGE plpgsql;



--
-- Returns an array of squares from the given square_set.
--
CREATE OR REPLACE FUNCTION square_set_to_array(squares square_set) RETURNS square[] AS $$
DECLARE
  ret               square[]   := '{}';
  remaining_squares square_set := squares;
  empty_square_set  square_set := 0;
  game_move         square;
  game_move_index   INTEGER;
BEGIN
  WHILE remaining_squares != empty_square_set LOOP
    game_move_index := bit_works_bitscanLS1B_64(remaining_squares);
    SELECT id INTO STRICT game_move FROM square_info WHERE ordinal = game_move_index;
    ret := ret || game_move;
    remaining_squares := remaining_squares # (1::square_set << game_move_index);
  END LOOP;
  return ret;
END;
$$ LANGUAGE plpgsql;



--
-- Maps the principal line of each axis into row one.
--
-- Shifting right should be done casting to BIT(64), but becouse we are protected by the bitwise and we can
-- just skip it!
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION axis_transform_to_row_one(axis axis, squares square_set) RETURNS SMALLINT AS $$
DECLARE
  row_one        square_set := (x'00000000000000FF')::square_set;
  column_a       square_set := (x'0101010101010101')::square_set;
  diagonal_a1_h8 square_set := (x'8040201008040201')::square_set;
  diagonal_h1_a8 square_set := (x'0102040810204080')::square_set;
  ret            square_set;
BEGIN
  ret := squares;
  IF axis = 'HO' THEN
    NULL;
  ELSEIF axis = 'VE' THEN
    ret := ret & column_a;
    ret := ret | (ret >> 28);
    ret := ret | (ret >> 14);
    ret := ret | (ret >>  7);
  ELSEIF axis = 'DD' THEN
    ret := ret & diagonal_a1_h8;
    ret := ret | (ret >> 32);
    ret := ret | (ret >> 16);
    ret := ret | (ret >>  8);
  ELSEIF axis = 'DU' THEN
    ret := ret & diagonal_h1_a8;
    ret := ret | (ret >> 32);
    ret := ret | (ret >> 16);
    ret := ret | (ret >>  8);
  ELSE
    RAISE EXCEPTION 'Parameter axis out of range.';
  END IF;
  RETURN (ret & row_one)::SMALLINT;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the ordinal position of the move.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION axis_move_ordinal_position_in_bitrow(axis axis, move_column SMALLINT, move_row SMALLINT) RETURNS SMALLINT AS $$
BEGIN
  IF  axis = 'VE' THEN
    RETURN move_row;
  END IF;
  RETURN move_column;
END;
$$ LANGUAGE plpgsql;



--
-- Computes the shift quantity.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION axis_shift_distance(axis axis, move_column INT, move_row INT) RETURNS INT AS $$
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
END;
$$ LANGUAGE plpgsql;



--
-- Returns a square_set value by shifting the
-- squares parameter by one position on the board.
--
-- Parameter dir must belong to the direction enum.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION direction_shift_square_set(dir direction, squares square_set) RETURNS square_set AS $$
DECLARE
  all_squares_except_column_a square_set := (x'FEFEFEFEFEFEFEFE')::square_set;
  all_squares_except_column_h square_set := (x'7F7F7F7F7F7F7F7F')::square_set;
BEGIN
  CASE dir
    WHEN 'NW' THEN RETURN (squares::BIT(64) >> 9)::square_set & all_squares_except_column_h;
    WHEN 'N'  THEN RETURN (squares::BIT(64) >> 8)::square_set;
    WHEN 'NE' THEN RETURN (squares::BIT(64) >> 7)::square_set & all_squares_except_column_a;
    WHEN 'W'  THEN RETURN (squares::BIT(64) >> 1)::square_set & all_squares_except_column_h;
    WHEN 'E'  THEN RETURN (squares::BIT(64) << 1)::square_set & all_squares_except_column_a;
    WHEN 'SW' THEN RETURN (squares::BIT(64) << 7)::square_set & all_squares_except_column_h;
    WHEN 'S'  THEN RETURN (squares::BIT(64) << 8)::square_set;
    WHEN 'SE' THEN RETURN (squares::BIT(64) << 9)::square_set & all_squares_except_column_a;
    ELSE
      RAISE EXCEPTION 'Parameter dir out of range.';
  END CASE;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a new square_set value by shifting back the squares parameter
-- by a number of positions as given by the amount parameter.
--
-- Amount must be in the 0..7 range, meaning that 0 is equal to no shift, 1 is
-- on position, and so on.
--
-- It is safe to call this function only after a number of shift call equal to the amount value.
-- This is becouse the function doesn't mask after shifting.
--
CREATE OR REPLACE FUNCTION direction_shift_back_square_set_by_amount(dir direction, squares square_set, amount INTEGER) RETURNS square_set AS $$
DECLARE
BEGIN
  CASE dir
    WHEN 'NW' THEN RETURN (squares::BIT(64) >> (9 * amount))::square_set;
    WHEN 'N'  THEN RETURN (squares::BIT(64) >> (8 * amount))::square_set;
    WHEN 'NE' THEN RETURN (squares::BIT(64) >> (7 * amount))::square_set;
    WHEN 'W'  THEN RETURN (squares::BIT(64) >> (1 * amount))::square_set;
    WHEN 'E'  THEN RETURN (squares::BIT(64) << (1 * amount))::square_set;
    WHEN 'SW' THEN RETURN (squares::BIT(64) << (7 * amount))::square_set;
    WHEN 'S'  THEN RETURN (squares::BIT(64) << (8 * amount))::square_set;
    WHEN 'SE' THEN RETURN (squares::BIT(64) << (9 * amount))::square_set;
    ELSE
      RAISE EXCEPTION 'Parameter dir out of range.';
  END CASE;
END;
$$ LANGUAGE plpgsql;



--
-- Populates the table board_bitrow_changes_for_player.
--
-- The table must be empty before running the function.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION board_populate_bitrow_changes_for_player() RETURNS VOID AS $$
DECLARE
  player_row_count                       INTEGER;
  opponent_row_count                     INTEGER;
  move_position                          INTEGER;
  player_row                             SMALLINT;
  opponent_row                           SMALLINT;
  filled_in_row                          SMALLINT;
  empties_in_row                         SMALLINT;
  game_move                              SMALLINT;
  table_id                               INTEGER;
  player_row_after_move                  SMALLINT;
  potential_bracketing_disc_on_the_left  SMALLINT;
  potential_bracketing_disc_on_the_right SMALLINT;
  left_rank                              SMALLINT;
  right_rank                             SMALLINT;
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

          -- If the rank contains empy squares, this is a fake flip, and it doesn't do anything.
          -- If the rank is full, it cannot be full of anything different than opponent discs, so
          -- it adds the discs to the after move player configuration.
          IF ((left_rank & empties_in_row) = 0) THEN
            player_row_after_move := player_row_after_move | left_rank;
          END IF;

          --Here it does the same procedure computed on the left also on the right.
          potential_bracketing_disc_on_the_right := bit_works_lowest_bit_set_8(CAST (player_row & ~(game_move - 1) AS SMALLINT));
          right_rank := bit_works_fill_in_between_8(potential_bracketing_disc_on_the_right | game_move);
          IF ((right_rank & empties_in_row) = 0) THEN
            player_row_after_move := player_row_after_move | right_rank;
          END IF;

          -- It checks that the after move configuration is different from
          -- the starting one for the player.
          -- This case can happen because it never checked that
          -- the bracketing piece was not adjacent to the move disc,
          -- on such a case, on both side, the move is illegal, and it is recorded setting
          -- the result configuation appropriately.
          IF (player_row_after_move = (player_row | game_move)) THEN
            player_row_after_move := player_row;
          END IF;

        END IF;
        INSERT INTO board_bitrow_changes_for_player (id, changes) VALUES (table_id, player_row_after_move);
      END LOOP;
    END LOOP;
  END LOOP;
END;
$$ LANGUAGE plpgsql;



--
-- Returns an 8-bit row representation of the player pieces after applying the move.
--
CREATE OR REPLACE FUNCTION board_bitrow_changes_for_player(player_row SMALLINT, opponent_row SMALLINT, move_position SMALLINT) RETURNS SMALLINT AS $$
DECLARE
  bitrow_changes_for_player_index INTEGER;
  ret                             SMALLINT;
BEGIN
  bitrow_changes_for_player_index := player_row | (opponent_row << 8) | (move_position::INTEGER << 16);
  SELECT changes INTO STRICT ret FROM board_bitrow_changes_for_player WHERE id = bitrow_changes_for_player_index;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a string describing the board state.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_to_string(gp game_position) RETURNS CHAR(65) AS $$
DECLARE
  ret  CHAR(65) := '';
  mask square_set;
BEGIN
  FOR i IN 0..63 LOOP
    mask := 1::square_set << i;
    IF mask & gp.blacks <> 0 THEN
      ret := ret || 'b';
    ELSIF mask & gp.whites <> 0 THEN
      ret := ret || 'w';
    ELSE
      ret := ret || '.';
    END IF;
  END LOOP;
  ret := ret || player_to_string(gp.player);
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a game position from the given string.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_from_string(gp_string CHAR(65)) RETURNS game_position AS $$
DECLARE
  blacks square_set := 0;
  whites square_set := 0;
  player player     := 0;
  i      INTEGER    := 0;
  c      CHAR;
  square square_set;
BEGIN
  IF length(gp_string) <> 65 THEN
    RAISE EXCEPTION 'Value of length(gp_string) is %, it must be 65!', length(gp_string);
  END IF;
  FOREACH c IN ARRAY string_to_array(gp_string, NULL)
  LOOP
    IF (i = 64) THEN
      IF (c = 'b') THEN
        player := 0;
      ELSEIF (c = 'w') THEN
        player := 1;
      ELSE
        RAISE EXCEPTION 'Player must be either b or w, it is equal to "%".', c;
      END IF;
    ELSE
      square = 1::square_set << i;
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
  RETURN (blacks, whites, player);
END;
$$ LANGUAGE plpgsql;



--
-- Returns the set of empty squares.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_empties(gp game_position) RETURNS square_set AS $$
BEGIN
  RETURN ~(gp.blacks | gp.whites);
END;
$$ LANGUAGE plpgsql;



--
-- Returns the set of empty squares.
--
CREATE OR REPLACE FUNCTION game_position_get_square_set_for_player(gp game_position) RETURNS square_set AS $$
BEGIN
  IF gp.player = 0 THEN
    RETURN gp.blacks;
  ELSE
    RETURN gp.whites;
  END IF;
END;
$$ LANGUAGE plpgsql;



--
-- Returns the set of empty squares.
--
CREATE OR REPLACE FUNCTION game_position_get_square_set_for_opponent(gp game_position) RETURNS square_set AS $$
BEGIN
  IF gp.player = 0 THEN
    RETURN gp.whites;
  ELSE
    RETURN gp.blacks;
  END IF;
END;
$$ LANGUAGE plpgsql;



--
-- Returns true if the move is legal.
--
-- Tests written.
--
CREATE OR REPLACE FUNCTION game_position_is_move_legal(gp game_position, move square) RETURNS BOOLEAN AS $$
DECLARE
  empties               square_set := game_position_empties(gp);
  empty_square_set      square_set := 0;
  bit_move              square_set;
  move_ordinal          INTEGER;
  p_square_set          square_set;
  o_square_set          square_set;
  move_column           SMALLINT;
  move_row              SMALLINT;
  axis                  RECORD;
  move_ordinal_position SMALLINT;
  shift_distance        INTEGER;
  p_bitrow              SMALLINT;
  o_bitrow              SMALLINT;
BEGIN
  IF (empties & bit_move) = empty_square_set THEN RETURN FALSE; END IF;
  SELECT ordinal INTO STRICT move_ordinal FROM square_info WHERE id = move;
  bit_move := 1::square_set << move_ordinal;
  IF gp.player = 0 THEN
    p_square_set := gp.blacks;
    o_square_set := gp.whites;
  ELSE
    p_square_set := gp.whites;
    o_square_set := gp.blacks;
  END IF;
  move_column := move_ordinal % 8;
  move_row    := move_ordinal / 8;
  FOR axis IN SELECT id, ordinal FROM axis_info ORDER BY ordinal LOOP
    move_ordinal_position := axis_move_ordinal_position_in_bitrow(axis.id, move_column, move_row);
    shift_distance := axis_shift_distance(axis.id, move_column, move_row);
    p_bitrow := axis_transform_to_row_one(axis.id, bit_works_signed_left_shift(p_square_set, shift_distance));
    o_bitrow := axis_transform_to_row_one(axis.id, bit_works_signed_left_shift(o_square_set, shift_distance));
    IF (board_bitrow_changes_for_player(p_bitrow, o_bitrow, move_ordinal_position) != p_bitrow) THEN
      RETURN TRUE;
    END IF;
  END LOOP;
  -- If no capture on the four directions happens, return false.
  RETURN FALSE;
END;
$$ LANGUAGE plpgsql;



--
-- Returns a set of squares that represents the legal moves for the game position.
--
CREATE OR REPLACE FUNCTION game_position_legal_moves(gp game_position) RETURNS square_set AS $$
DECLARE
  ret              square_set := 0;
  empty_square_set square_set := 0;
  empties          square_set := game_position_empties(gp);
  p_square_set     square_set;
  o_square_set     square_set;
  dir              RECORD;
  wave             square_set;
  shift            INTEGER;
BEGIN
  IF gp.player = 0 THEN
    p_square_set := gp.blacks;
    o_square_set := gp.whites;
  ELSE
    p_square_set := gp.whites;
    o_square_set := gp.blacks;
  END IF;
  
  FOR dir IN SELECT id, ordinal, opposite FROM direction_info ORDER BY ordinal LOOP
    wave := direction_shift_square_set(dir.id, empties) & o_square_set;
    shift := 1;
    WHILE wave != empty_square_set LOOP
      wave = direction_shift_square_set(dir.id, wave);
      shift := shift + 1;
      ret := ret | direction_shift_back_square_set_by_amount(dir.opposite, wave & p_square_set, shift);
      wave := wave & o_square_set;
    END LOOP;
  END LOOP;
  RETURN ret;
END;
$$ LANGUAGE plpgsql;



--
-- Executes a game move on the given position.
--
CREATE OR REPLACE FUNCTION game_position_make_move(gp game_position, game_move square) RETURNS game_position AS $$
DECLARE
  player       CONSTANT player     := gp.player;
  opponent     CONSTANT player     := player_opponent(player);
  p_square_set CONSTANT square_set := game_position_get_square_set_for_player(gp);
  o_square_set CONSTANT square_set := game_position_get_square_set_for_opponent(gp);
  move_column  CONSTANT SMALLINT   := square_get_column(game_move);
  move_row     CONSTANT SMALLINT   := square_get_row(game_move);
  /*
  const Player p = gp->player;
  const Player o = player_opponent(p);
  const Board *b = gp->board;
  const SquareSet p_bit_board = board_get_player(b, p);
  const SquareSet o_bit_board = board_get_player(b, o);
  const int column = move % 8;
  const int row = move / 8;
  */
  axis RECORD;
BEGIN

  RAISE NOTICE 'move_column=%, move_row=%', move_column, move_row;

  /*
  SquareSet new_bit_board[2];
  const SquareSet unmodified_mask = ~bitboard_mask_for_all_directions[move];
  new_bit_board[p] = p_bit_board & unmodified_mask;
  new_bit_board[o] = o_bit_board & unmodified_mask;
  */
  
  FOR axis IN SELECT id, ordinal FROM axis_info ORDER BY ordinal LOOP
  END LOOP;
  /*
  for (Axis axis = HO; axis <= DU; axis++) {
    const int move_ordinal_position = axis_move_ordinal_position_in_bitrow(axis, column, row);
    const int shift_distance = axis_shift_distance(axis, column, row);
    uint8 p_bitrow = axis_transform_to_row_one(axis, bit_works_signed_left_shift(p_bit_board, shift_distance));
    uint8 o_bitrow = axis_transform_to_row_one(axis, bit_works_signed_left_shift(o_bit_board, shift_distance));
    p_bitrow = board_bitrow_changes_for_player(p_bitrow, o_bitrow, move_ordinal_position);
    o_bitrow &= ~p_bitrow;
    new_bit_board[p] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(axis, p_bitrow), -shift_distance);
    new_bit_board[o] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(axis, o_bitrow), -shift_distance);
  }
  */
  RETURN (0::square_set, 0::square_set, 0::player);

END;
$$ LANGUAGE plpgsql;
