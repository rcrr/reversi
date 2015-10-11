--
-- function_definition.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2014, 2015 Roberto Corradini. All rights reserved.
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
-- Start psql by running: psql -U es -w -d es -h localhost
-- Load the file by running the command: \i function_definition.sql
--
--
-- This script creates the functions used by the reversi program.
--

SET search_path TO reversi;



CREATE OR REPLACE FUNCTION p_assert(p_assertion BOOLEAN, p_message_on_error VARCHAR(255)) RETURNS VOID AS $$
--
-- Realizes the assert statement.
--
BEGIN
  IF p_assertion IS NULL THEN
    RAISE EXCEPTION 'Assertion is null, that is not supported.';
  END IF;
  IF NOT p_assertion THEN
    RAISE EXCEPTION '%', p_message_on_error;
  END IF;
END;
$$ LANGUAGE plpgsql;
COMMENT ON FUNCTION p_assert (BOOLEAN, VARCHAR(255)) IS 'Realizes the assert statement.';



CREATE OR REPLACE FUNCTION bit_works_popcnt(bit_sequence BIGINT) RETURNS SMALLINT AS $$
--
-- Returns the count of the bit set to 1 in the bit_sequence argument.
--
DECLARE
  sign_mask CONSTANT square_set := (x'7FFFFFFFFFFFFFFF')::square_set;

  tmp BIGINT   := bit_sequence;
  cnt SMALLINT := 0;
BEGIN
  IF tmp < 0 THEN
    cnt := cnt + 1;
    tmp := tmp & sign_mask;
  END IF;
  <<the_loop>>
  LOOP
    IF tmp = 0 THEN
      EXIT the_loop;
    END IF;
    cnt := cnt + 1;
    tmp := tmp & (tmp - 1);
  END LOOP;
  RETURN cnt;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION bit_works_bitscanMS1B_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
--
-- Returns the index (0..7) of the most significant bit set in the bit_sequence parameter.
-- Only bits from 0 to 7 in parameter bit_sequence are considered, 8..15 are irrelevant. 
--
DECLARE
  log2_array CONSTANT SMALLINT[] :=
    '{ 0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
       5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
       6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
       7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
     }';
BEGIN
  RETURN log2_array[(bit_sequence::INTEGER & 255) + 1];
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION bit_works_bitscanLS1B_64(bit_sequence BIGINT) RETURNS SMALLINT AS $$
--
-- Returns the index of the least significant bit set in the bit_sequence parameter.
--
-- Parameter bit_sequence must be different from 0.
-- If no bit set is found, meaning that bit_sequence is equal to 0, 64 is
-- returned, that is clearly a wrong value.
--
DECLARE
  mask_1 CONSTANT BIGINT := (x'00000000FFFFFFFF')::BIGINT;
  mask_2 CONSTANT BIGINT := (x'000000000000FFFF')::BIGINT;
  mask_3 CONSTANT BIGINT := (x'00000000000000FF')::BIGINT;
  mask_4 CONSTANT BIGINT := (x'000000000000000F')::BIGINT;
  mask_5 CONSTANT BIGINT := (x'0000000000000007')::BIGINT;
  mask_6 CONSTANT BIGINT := (x'0000000000000003')::BIGINT;
  mask_7 CONSTANT BIGINT := (x'0000000000000001')::BIGINT;

  tmp BIGINT   := bit_sequence;
  ret SMALLINT := 0;
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
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION bit_works_fill_in_between_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
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
DECLARE
  mask   CONSTANT SMALLINT := 255::SMALLINT;
  masked CONSTANT SMALLINT := bit_sequence & mask;
BEGIN
  RETURN ((1 << bit_works_bitscanMS1B_8(masked)) - 1) & ((~masked # (masked - 1)) & mask);
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION bit_works_highest_bit_set_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
--
-- Returns an int value having all the bit set in bit_sequence turned to 0
-- except the most significant one.
--
-- When parameter bit_sequence is equal to 0 it returns 0.
--
-- Bits higher than 7 (the second byte of the smallint) are masked to 0.
--
DECLARE
  log2_array CONSTANT SMALLINT[] :=
    '{   0,   1,   2,   2,   4,   4,   4,   4,   8,   8,   8,   8,   8,   8,   8,   8,
        16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,  16,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
        64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
        64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
        64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,  64,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
       128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128
     }';
BEGIN
  RETURN log2_array[(bit_sequence::INTEGER & 255) + 1];
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION bit_works_lowest_bit_set_8(bit_sequence SMALLINT) RETURNS SMALLINT AS $$
--
-- Returns a bit sequence having one bit set, the lowest found
-- in the bit_sequence parameter.
--
-- Bits higher than 7 (the second byte of the smallint) are masked to 0.
--
DECLARE
  mask   CONSTANT SMALLINT := 255::SMALLINT;
  masked CONSTANT SMALLINT := bit_sequence & mask;
BEGIN
  RETURN ((masked & (masked - 1)) # masked) & mask;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION bit_works_signed_left_shift(bit_sequence BIGINT, shift INTEGER) RETURNS BIGINT AS $$
--
-- Returns a value computed shifting the bit_sequence parameter
-- to left by a signed amount given by the shift parameter.
--
BEGIN
  IF shift >= 0 THEN
    RETURN (bit_sequence::BIT(64) << +shift)::BIGINT;
  ELSE
    RETURN (bit_sequence::BIT(64) >> -shift)::BIGINT;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION player_to_char(player player) RETURNS CHAR(1) AS $$
--
-- Returns a char describing the palyer.
--
BEGIN
  IF player = 0 THEN
    RETURN 'b';
  ELSE
    RETURN 'w';
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION player_to_string(player player) RETURNS TEXT AS $$
--
-- Returns a string describing the palyer.
--
BEGIN
  IF player = 0 THEN
    RETURN 'BLACK';
  ELSE
    RETURN 'WHITE';
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION player_opponent(player player) RETURNS player AS $$
--
-- Returns the player's opponent.
--
BEGIN
  RETURN 1 - player;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION square_populate_addictional_fields() RETURNS VOID AS $$
--
-- Populates the table square_info with sq_column and sq_row fields.
--
DECLARE
  -- This array has sixtyfour entries. The index, having range 0-63, represent one of the squares
  -- of the table. Each entry is a bitboard mask having set all the squares that are
  -- reachable moving along the eigth directions, when starting from the square identified by
  -- the index itself.
  --
  -- WARNING: the definition should be square_set[] but as stated by the official documentation: "Arrays of domains are not yet supported.".
  --
  move_mask_for_all_directions_init_data CONSTANT BIGINT[] := array[
    (x'81412111090503FE')::BIGINT, (x'02824222120A07FD')::BIGINT, (x'0404844424150EFB')::BIGINT, (x'08080888492A1CF7')::BIGINT,
    (x'10101011925438EF')::BIGINT, (x'2020212224A870DF')::BIGINT, (x'404142444850E0BF')::BIGINT, (x'8182848890A0C07F')::BIGINT,
    (x'412111090503FE03')::BIGINT, (x'824222120A07FD07')::BIGINT, (x'04844424150EFB0E')::BIGINT, (x'080888492A1CF71C')::BIGINT,
    (x'101011925438EF38')::BIGINT, (x'20212224A870DF70')::BIGINT, (x'4142444850E0BFE0')::BIGINT, (x'82848890A0C07FC0')::BIGINT,
    (x'2111090503FE0305')::BIGINT, (x'4222120A07FD070A')::BIGINT, (x'844424150EFB0E15')::BIGINT, (x'0888492A1CF71C2A')::BIGINT,
    (x'1011925438EF3854')::BIGINT, (x'212224A870DF70A8')::BIGINT, (x'42444850E0BFE050')::BIGINT, (x'848890A0C07FC0A0')::BIGINT,
    (x'11090503FE030509')::BIGINT, (x'22120A07FD070A12')::BIGINT, (x'4424150EFB0E1524')::BIGINT, (x'88492A1CF71C2A49')::BIGINT,
    (x'11925438EF385492')::BIGINT, (x'2224A870DF70A824')::BIGINT, (x'444850E0BFE05048')::BIGINT, (x'8890A0C07FC0A090')::BIGINT,
    (x'090503FE03050911')::BIGINT, (x'120A07FD070A1222')::BIGINT, (x'24150EFB0E152444')::BIGINT, (x'492A1CF71C2A4988')::BIGINT,
    (x'925438EF38549211')::BIGINT, (x'24A870DF70A82422')::BIGINT, (x'4850E0BFE0504844')::BIGINT, (x'90A0C07FC0A09088')::BIGINT,
    (x'0503FE0305091121')::BIGINT, (x'0A07FD070A122242')::BIGINT, (x'150EFB0E15244484')::BIGINT, (x'2A1CF71C2A498808')::BIGINT,
    (x'5438EF3854921110')::BIGINT, (x'A870DF70A8242221')::BIGINT, (x'50E0BFE050484442')::BIGINT, (x'A0C07FC0A0908884')::BIGINT,
    (x'03FE030509112141')::BIGINT, (x'07FD070A12224282')::BIGINT, (x'0EFB0E1524448404')::BIGINT, (x'1CF71C2A49880808')::BIGINT,
    (x'38EF385492111010')::BIGINT, (x'70DF70A824222120')::BIGINT, (x'E0BFE05048444241')::BIGINT, (x'C07FC0A090888482')::BIGINT,
    (x'FE03050911214181')::BIGINT, (x'FD070A1222428202')::BIGINT, (x'FB0E152444840404')::BIGINT, (x'F71C2A4988080808')::BIGINT,
    (x'EF38549211101010')::BIGINT, (x'DF70A82422212020')::BIGINT, (x'BFE0504844424140')::BIGINT, (x'7FC0A09088848281')::BIGINT
  ];
  r square_info;
BEGIN
  FOR r IN SELECT * FROM square_info LOOP
    UPDATE square_info
    SET
      sq_column = r.ordinal % 8,
      sq_row = r.ordinal / 8,
      move_mask_for_all_directions = move_mask_for_all_directions_init_data[r.ordinal + 1]
    WHERE id = r.id;
  END LOOP;
END;
$$ LANGUAGE plpgsql VOLATILE;



CREATE OR REPLACE FUNCTION square_get_column(sq square) RETURNS SMALLINT AS $$
--
-- Returns the column ordinal of the square.
--
DECLARE
  ret SMALLINT;
BEGIN
  SELECT sq_column INTO STRICT ret FROM square_info WHERE sq = id;
  RETURN ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION square_get_row(sq square) RETURNS SMALLINT AS $$
--
-- Returns the row ordinal of the square.
--
DECLARE
  ret SMALLINT;
BEGIN
  SELECT sq_row INTO STRICT ret FROM square_info WHERE sq = id;
  RETURN ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION square_set_to_string(squares square_set) RETURNS CHAR(64) AS $$
--
-- Returns a string describing the square set.
--
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
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION square_set_from_string(ss_string CHAR(64)) RETURNS square_set AS $$
--
-- Returns a square_set from the given string.
--
DECLARE
  square  square_set := 0;
  i       INTEGER    := 0;
  squares square_set := 0;
  ch      CHAR;
BEGIN
  IF length(ss_string) <> 64 THEN
    RAISE EXCEPTION 'Value of length(ss_string) is %, it must be 64!', length(ss_string);
  END IF;
  FOREACH ch IN ARRAY string_to_array(ss_string, NULL)
  LOOP
    square = 1::square_set << i;
    IF (ch = 'x') THEN
      squares := squares | square;
    ELSEIF (ch = '.') THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'Square must be either x or ., it is equal to "%".', ch;
    END IF;
    i := i + 1;
  END LOOP;
  RETURN squares;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION square_set_to_array(squares square_set) RETURNS square[] AS $$
--
-- Returns an array of squares from the given square_set.
--
DECLARE
  empty_square_set CONSTANT square_set := 0;
  square_array     CONSTANT square[]   := array(SELECT id FROM square_info ORDER BY ordinal);

  ret               square[]   := '{}';
  remaining_squares square_set := squares;
  game_move_index   INTEGER;
BEGIN
  WHILE remaining_squares != empty_square_set LOOP
    game_move_index := bit_works_bitscanLS1B_64(remaining_squares);
    ret := ret || square_array[game_move_index + 1];
    remaining_squares := remaining_squares # (1::square_set << game_move_index);
  END LOOP;
  return ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION axis_transform_to_row_one(axis axis, squares square_set) RETURNS SMALLINT AS $$
--
-- Maps the principal line of each axis into row one.
--
-- Shifting right should be done casting to BIT(64), but becouse we are protected by the bitwise and we can
-- just skip it!
--
DECLARE
  row_one        CONSTANT square_set := (x'00000000000000FF')::square_set;
  column_a       CONSTANT square_set := (x'0101010101010101')::square_set;
  diagonal_a1_h8 CONSTANT square_set := (x'8040201008040201')::square_set;
  diagonal_h1_a8 CONSTANT square_set := (x'0102040810204080')::square_set;

  ret square_set := squares;
BEGIN
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
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION axis_transform_back_from_row_one(axis axis, bitrow BIGINT) RETURNS square_set AS $$
--
-- Maps back the principal line of each axis from row one.
--
-- Returns a square set having the bits along the axis reference file set to
-- the corresponding ones on the bitrow parameter 0..7, all other position are set to zero.
--
DECLARE
  column_a            CONSTANT square_set := (x'0101010101010101')::square_set;
  diagonal_a1_h8      CONSTANT square_set := (x'8040201008040201')::square_set;
  diagonal_h1_a8      CONSTANT square_set := (x'0102040810204080')::square_set;
  squares_b1_f1_a2_e2 CONSTANT square_set := (x'0000000000001122')::square_set;

  tmp square_set;
BEGIN
  CASE axis
    WHEN 'HO' THEN RETURN bitrow;
    WHEN 'VE' THEN
      tmp := bitrow;
      tmp := tmp | (tmp <<  7);
      tmp := tmp | (tmp << 14);
      tmp := tmp | (tmp << 28);
      RETURN tmp & column_a;
    WHEN 'DD' THEN
      tmp := bitrow;
      tmp := tmp | (tmp <<  8);
      tmp := tmp | (tmp << 16);
      tmp := tmp | (tmp << 32);
      RETURN tmp & diagonal_a1_h8;
    WHEN 'DU' THEN
      tmp := bitrow;
      tmp := tmp | (tmp << 8);
      tmp := tmp | ((tmp & squares_b1_f1_a2_e2) << 16);
      tmp := tmp | (tmp << 32);
      RETURN tmp & diagonal_h1_a8;
    ELSE
      RAISE EXCEPTION 'Parameter axis out of range.';
  END CASE;

END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION axis_move_ordinal_position_in_bitrow(axis axis, move_column SMALLINT, move_row SMALLINT) RETURNS SMALLINT AS $$
--
-- Returns the ordinal position of the move.
--
BEGIN
  IF  axis = 'VE' THEN
    RETURN move_row;
  END IF;
  RETURN move_column;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION axis_shift_distance(axis axis, move_column SMALLINT, move_row SMALLINT) RETURNS SMALLINT AS $$
--
-- Computes the shift quantity.
--
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
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION direction_shift_square_set(dir direction, squares square_set) RETURNS square_set AS $$
--
-- Returns a square_set value by shifting the
-- squares parameter by one position on the board.
--
-- Parameter dir must belong to the direction enum.
--
DECLARE
  all_squares_except_column_a CONSTANT square_set := (x'FEFEFEFEFEFEFEFE')::square_set;
  all_squares_except_column_h CONSTANT square_set := (x'7F7F7F7F7F7F7F7F')::square_set;
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
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION direction_shift_back_square_set_by_amount(dir direction, squares square_set, amount INTEGER) RETURNS square_set AS $$
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
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION board_populate_bitrow_changes_for_player() RETURNS VOID AS $$
--
-- Populates the table board_bitrow_changes_for_player.
--
-- The table must be empty before running the function.
--
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
$$ LANGUAGE plpgsql VOLATILE;



CREATE OR REPLACE FUNCTION board_bitrow_changes_for_player(player_row SMALLINT, opponent_row SMALLINT, move_position SMALLINT) RETURNS SMALLINT AS $$
--
-- Returns an 8-bit row representation of the player pieces after applying the move.
--
DECLARE
  bitrow_changes_for_player_index CONSTANT INTEGER := player_row::INTEGER | (opponent_row::INTEGER << 8) | (move_position::INTEGER << 16);

  ret SMALLINT;
BEGIN
  SELECT changes INTO STRICT ret FROM board_bitrow_changes_for_player WHERE id = bitrow_changes_for_player_index;
  RETURN ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_to_string(gp game_position) RETURNS CHAR(65) AS $$
--
-- Returns a string describing the board state.
--
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
  ret := ret || player_to_char(gp.player);
  RETURN ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_from_string(gp_string CHAR(65)) RETURNS game_position AS $$
--
-- Returns a game position from the given string.
--
DECLARE
  blacks square_set := 0;
  whites square_set := 0;
  player player     := 0;
  i      INTEGER    := 0;
  ch     CHAR;
  square square_set;
BEGIN
  IF length(gp_string) <> 65 THEN
    RAISE EXCEPTION 'Value of length(gp_string) is %, it must be 65!', length(gp_string);
  END IF;
  FOREACH ch IN ARRAY string_to_array(gp_string, NULL)
  LOOP
    IF (i = 64) THEN
      IF (ch = 'b') THEN
        player := 0;
      ELSEIF (ch = 'w') THEN
        player := 1;
      ELSE
        RAISE EXCEPTION 'Player must be either b or w, it is equal to "%".', c;
      END IF;
    ELSE
      square = 1::square_set << i;
      IF (ch = 'b') THEN
        blacks := blacks | square;
      ELSEIF (ch = 'w') THEN
        whites := whites | square;
      ELSEIF (ch = '.') THEN
        NULL;
      ELSE
        RAISE EXCEPTION 'Square color must be either b or w or ., it is equal to "%".', c;
      END IF;
    END IF;
    i := i + 1;
  END LOOP;
  RETURN (blacks, whites, player);
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_empties(gp game_position) RETURNS square_set AS $$
--
-- Returns the set of empty squares.
--
BEGIN
  RETURN ~(gp.blacks | gp.whites);
END;
$$ LANGUAGE plpgsql IMMUTABLE;
COMMENT ON FUNCTION game_position_empties (game_position) IS 'Returns the set of empty squares.';



CREATE OR REPLACE FUNCTION game_position_get_square_set_for_player(gp game_position) RETURNS square_set AS $$
--
-- Returns the square set for the player.
--
BEGIN
  IF gp.player = 0 THEN
    RETURN gp.blacks;
  ELSE
    RETURN gp.whites;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_get_square_set_for_opponent(gp game_position) RETURNS square_set AS $$
--
-- Returns the square set for the opponent.
--
BEGIN
  IF gp.player = 0 THEN
    RETURN gp.whites;
  ELSE
    RETURN gp.blacks;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_is_move_legal(gp game_position, move square) RETURNS BOOLEAN AS $$
--
-- Returns true if the move is legal.
--
DECLARE
  empties               CONSTANT square_set := game_position_empties(gp);
  empty_square_set      CONSTANT square_set := 0;
  move_ordinal          INTEGER;
  bit_move              square_set;
  p_square_set          square_set;
  o_square_set          square_set;
  move_column           SMALLINT;
  move_row              SMALLINT;
  axis_record           RECORD;
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
  FOR axis_record IN SELECT id, ordinal FROM axis_info ORDER BY ordinal LOOP
    move_ordinal_position := axis_move_ordinal_position_in_bitrow(axis_record.id, move_column, move_row);
    shift_distance := axis_shift_distance(axis_record.id, move_column, move_row);
    p_bitrow := axis_transform_to_row_one(axis_record.id, bit_works_signed_left_shift(p_square_set, shift_distance));
    o_bitrow := axis_transform_to_row_one(axis_record.id, bit_works_signed_left_shift(o_square_set, shift_distance));
    IF (board_bitrow_changes_for_player(p_bitrow, o_bitrow, move_ordinal_position) != p_bitrow) THEN
      RETURN TRUE;
    END IF;
  END LOOP;
  -- If no capture on the four directions happens, return false.
  RETURN FALSE;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_legal_moves(gp game_position) RETURNS square_set AS $$
--
-- Returns a set of squares that represents the legal moves for the game position.
--
DECLARE
  empty_square_set CONSTANT square_set := 0;
  empties          CONSTANT square_set := game_position_empties(gp);

  p_square_set     square_set;
  o_square_set     square_set;
  dir              RECORD;
  wave             square_set;
  shift            INTEGER;
  ret              square_set := 0;
BEGIN
  IF gp.player = 0 THEN
    p_square_set := gp.blacks;
    o_square_set := gp.whites;
  ELSE
    p_square_set := gp.whites;
    o_square_set := gp.blacks;
  END IF;
  FOR dir IN SELECT id, ordinal, opposite FROM direction_info ORDER BY ordinal LOOP
    wave  := direction_shift_square_set(dir.id, empties) & o_square_set;
    shift := 1;
    WHILE wave != empty_square_set LOOP
      wave  := direction_shift_square_set(dir.id, wave);
      shift := shift + 1;
      ret   := ret | direction_shift_back_square_set_by_amount(dir.opposite, wave & p_square_set, shift);
      wave  := wave & o_square_set;
    END LOOP;
  END LOOP;
  RETURN ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_make_move(gp game_position, game_move square) RETURNS game_position AS $$
--
-- Executes a game move on the given position.
--
DECLARE
  player         CONSTANT player     := gp.player;
  opponent       CONSTANT player     := player_opponent(player);
  p_square_set   CONSTANT square_set := game_position_get_square_set_for_player(gp);
  o_square_set   CONSTANT square_set := game_position_get_square_set_for_opponent(gp);
  move_column    CONSTANT SMALLINT   := square_get_column(game_move);
  move_row       CONSTANT SMALLINT   := square_get_row(game_move);
  unmodified_set CONSTANT square_set := ~(SELECT move_mask_for_all_directions FROM square_info WHERE id = game_move);

  new_board             transient_board;
  axis                  axis;
  move_ordinal_position SMALLINT;
  shift_distance        SMALLINT;
  p_bitrow              SMALLINT;
  o_bitrow              SMALLINT;
BEGIN
  new_board := (p_square_set & unmodified_set, o_square_set & unmodified_set);
  FOREACH axis IN ARRAY (SELECT enum_range(NULL::axis)) LOOP
    move_ordinal_position := axis_move_ordinal_position_in_bitrow(axis, move_column, move_row);
    shift_distance := axis_shift_distance(axis, move_column, move_row);
    p_bitrow := axis_transform_to_row_one(axis, bit_works_signed_left_shift(p_square_set, shift_distance));
    o_bitrow := axis_transform_to_row_one(axis, bit_works_signed_left_shift(o_square_set, shift_distance));
    p_bitrow := board_bitrow_changes_for_player(p_bitrow, o_bitrow, move_ordinal_position);
    o_bitrow := o_bitrow & ~p_bitrow;
    new_board.p_square_set := new_board.p_square_set | bit_works_signed_left_shift(axis_transform_back_from_row_one(axis, p_bitrow), -shift_distance); 
    new_board.o_square_set := new_board.o_square_set | bit_works_signed_left_shift(axis_transform_back_from_row_one(axis, o_bitrow), -shift_distance); 
  END LOOP;
  IF opponent = 0 THEN
    RETURN (new_board.o_square_set, new_board.p_square_set, opponent);
  ELSE
    RETURN (new_board.p_square_set, new_board.o_square_set, opponent);
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_pass(gp game_position) RETURNS game_position AS $$
--
-- Executes a game pass move on the given position.
--
BEGIN
  RETURN (gp.blacks, gp.whites, player_opponent(gp.player));
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_pp(gp game_position) RETURNS TEXT AS $$
--
-- Executes a game move on the given position.
--
DECLARE
  new_line CONSTANT TEXT := E'\n';

  i_sq INTEGER;
  sq   square_set;
  ret  TEXT;
BEGIN
  ret := '';
  i_sq := 0;
  ret := ret || '   a b c d e f g h ' || new_line;
  FOR i_row IN 1..8 LOOP
    ret := ret || i_row::TEXT || '  ';
    FOR i_column IN 1..8 LOOP
      sq := 1::square_set << i_sq;
      IF sq & gp.blacks <> 0 THEN
        ret := ret || '@';
      ELSEIF sq & gp.whites <> 0 THEN
        ret := ret || 'O';
      ELSE
        ret := ret || '.';
      END IF;
      ret := ret || ' ';
      i_sq := i_sq + 1;
    END LOOP;
    ret := ret || new_line;
  END LOOP;
  ret := ret || 'Player to move: ' || player_to_string(gp.player);
  RETURN ret;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_final_value(gp game_position) RETURNS SMALLINT AS $$
--
-- Computes the value of a final position.
--
DECLARE
  b_count    CONSTANT SMALLINT := bit_works_popcnt(gp.blacks);
  w_count    CONSTANT SMALLINT := bit_works_popcnt(gp.whites);
  difference CONSTANT SMALLINT := b_count - w_count;

  empties SMALLINT;
  delta   SMALLINT;
BEGIN
  IF difference = 0 THEN
    RETURN 0;
  ELSE
    empties := 64 - (b_count + w_count);
    IF difference > 0 THEN
      delta := difference + empties;
    ELSE
      delta := difference - empties;
    END IF;
    IF gp.player = 0 THEN
      RETURN +delta;
    ELSE
      RETURN -delta;
    END IF;
  END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;



CREATE OR REPLACE FUNCTION game_position_solve(    gp              game_position,
                                                   solver          CHAR(20),
                                                   log             BOOLEAN,
                                                   run_label       CHAR(4),
                                                   log_description TEXT,
                                               OUT sn              search_node)
AS $$
--
-- Solves a game position.
--
DECLARE
  best_score        CONSTANT SMALLINT := +64;
  worst_score       CONSTANT SMALLINT := -64;
  dummy_parent_hash CONSTANT BIGINT   := game_position_hash(game_position_pass(gp));

  square_count INTEGER := 0;
BEGIN
  IF log THEN
    TRUNCATE game_tree_log_staging;
    CREATE TEMPORARY SEQUENCE call_id_seq START 1;
  END IF;
  CASE solver
    WHEN 'SQL_MINIMAX_SOLVER' THEN
      sn := game_tree_minimax_solver_impl(gp, log, dummy_parent_hash);
    WHEN 'SQL_ALPHABETA_SOLVER' THEN
      sn := game_tree_alphabeta_solver_impl(gp, log, dummy_parent_hash, worst_score, best_score, TRUE);
    ELSE
      sn := (NULL, NULL);
  END CASE;
  IF log THEN
    DROP SEQUENCE call_id_seq;
    PERFORM gt_load_from_staging(run_label, solver, log_description);
    TRUNCATE game_tree_log_staging;
  END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;



CREATE OR REPLACE FUNCTION game_tree_minimax_solver_impl(    gp          game_position,
                                                             log         BOOLEAN,
                                                             parent_hash BIGINT,
                                                         OUT node        search_node)
AS $$
--
-- Utility function used by game_position_solve, it solves the game by applying a plain minimax algorithm.
--
DECLARE
  moves                     CONSTANT square_set := game_position_legal_moves(gp);
  empty_square_set          CONSTANT square_set := 0;
  out_of_range_defeat_score CONSTANT SMALLINT   := -65;

  remaining_move_array  square[];
  game_move             square;
  flipped_players       game_position;
  flipped_players_moves square_set;
  node_tmp              search_node;
  node_child            search_node;
  gp_child              game_position;
  hash                  BIGINT;
BEGIN
  IF log THEN
    hash := game_position_hash(gp);
    INSERT INTO game_tree_log_staging (sub_run_id, call_id, hash, parent_hash, blacks, whites, player)
      VALUES (0, nextval('call_id_seq'), hash, parent_hash, gp.blacks, gp.whites, gp.player);
  END IF;
  IF moves = empty_square_set THEN
    flipped_players := game_position_pass(gp);
    flipped_players_moves := game_position_legal_moves(flipped_players);
    IF flipped_players_moves <> 0 THEN
      node_tmp := game_tree_minimax_solver_impl(flipped_players, log, hash);
      node := (node_tmp.game_move, -node_tmp.game_value);
    ELSE
      node := (NULL, game_position_final_value(gp));
    END IF;
  ELSE
    node := (NULL, out_of_range_defeat_score);
    remaining_move_array := square_set_to_array(moves);
    FOREACH game_move IN ARRAY remaining_move_array LOOP
      gp_child := game_position_make_move(gp, game_move);
      node_tmp := game_tree_minimax_solver_impl(gp_child, log, hash);
      node_child := (node_tmp.game_move, -node_tmp.game_value);
      IF node_child.game_value > node.game_value THEN
        node := (game_move, node_child.game_value);
      END IF;
    END LOOP;
  END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;



CREATE OR REPLACE FUNCTION game_tree_alphabeta_solver_impl(    gp                          game_position,
                                                               log                         BOOLEAN,
                                                               parent_hash                 BIGINT,
                                                               alpha                       SMALLINT,
                                                               beta                        SMALLINT,
                                                               previuos_position_has_moves BOOLEAN,
                                                           OUT node                        search_node)
AS $$
--
-- Utility function used by game_position_solve, it solves the game using a base alpha-beta algorithm.
-- Legal moves are sorted by their natural order, from A1 to H8.
--
DECLARE
  moves                     CONSTANT square_set := game_position_legal_moves(gp);
  empty_square_set          CONSTANT square_set := 0;
  out_of_range_defeat_score CONSTANT SMALLINT   := -65;

  remaining_move_array  square[];
  game_move             square;
  flipped_players       game_position;
  flipped_players_moves square_set;
  node_tmp              search_node;
  node_child            search_node;
  gp_child              game_position;
  hash                  BIGINT;
BEGIN
  IF log THEN
    hash := game_position_hash(gp);
    INSERT INTO game_tree_log_staging (sub_run_id, call_id, hash, parent_hash, blacks, whites, player)
      VALUES (0, nextval('call_id_seq'), hash, parent_hash, gp.blacks, gp.whites, gp.player);
  END IF;
  IF moves = empty_square_set THEN
    IF (game_position_empties(gp) <> empty_square_set AND previuos_position_has_moves) THEN
      flipped_players := game_position_pass(gp);
      node_tmp := game_tree_alphabeta_solver_impl(flipped_players, log, hash, -beta, -alpha, FALSE);
      node := (node_tmp.game_move, -node_tmp.game_value);
    ELSE
      node := (NULL, game_position_final_value(gp));
    END IF;
  ELSE
    node := (NULL, out_of_range_defeat_score);
    remaining_move_array := square_set_to_array(moves);
    <<moves_loop>>
    FOREACH game_move IN ARRAY remaining_move_array LOOP
      gp_child := game_position_make_move(gp, game_move);
      node_tmp := game_tree_alphabeta_solver_impl(gp_child, log, hash, -beta, -node.game_value, TRUE);
      node_child := (node_tmp.game_move, -node_tmp.game_value);
      IF node_child.game_value > node.game_value THEN
        node := (game_move, node_child.game_value);
        EXIT moves_loop WHEN node.game_value >= beta; 
      END IF;
    END LOOP moves_loop;
  END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;



CREATE OR REPLACE FUNCTION game_position_hash(gp game_position) RETURNS BIGINT AS $$
--
-- Returns the hash value for the game position.
-- The hash is computed by mean of a zobrist technique.
--
DECLARE
  --
  -- Zobrist bitstrings generated by the URANDOM Linux generator.
  -- The array has 129 entries, 64 for black squares, 64 for white ones, and one for player.
  --
  zobrist_bitstrings CONSTANT BIGINT[] := array[
    (x'4EC764D9D2FC995C')::BIGINT, (x'4689879C5E2B6C8D')::BIGINT, (x'7B8509672627BDE6')::BIGINT, (x'5EC9EED7E83A1070')::BIGINT,
    (x'8C61E367C148A670')::BIGINT, (x'12BF65D47CB500F3')::BIGINT, (x'2D128CD312314B15')::BIGINT, (x'EC8FFA579C3FF006')::BIGINT,
    (x'1B772E0C17A8637D')::BIGINT, (x'FEA5D5093AE4F127')::BIGINT, (x'3F6223B7D74731E8')::BIGINT, (x'9BF6F10BF080F403')::BIGINT,
    (x'CE03618789AACB66')::BIGINT, (x'29264F46A5C844E3')::BIGINT, (x'D4BD485D4B88FC6E')::BIGINT, (x'AAC3C81799DAE8F5')::BIGINT,
    (x'46B256D2079DA270')::BIGINT, (x'5765CB86D7BE25B4')::BIGINT, (x'7754057541AAAF9E')::BIGINT, (x'B9E9E765C5214D6D')::BIGINT,
    (x'957833AE886E5003')::BIGINT, (x'59BEDBE41933BEB6')::BIGINT, (x'D5D328EC1AE90B22')::BIGINT, (x'26FA194CD5A4CC9A')::BIGINT,
    (x'34790BFC50B7571E')::BIGINT, (x'E91BC0D61C9FA3C5')::BIGINT, (x'8FB89136550EFB06')::BIGINT, (x'36701B34D43E30CD')::BIGINT,
    (x'AAFD6ACF34BB27C1')::BIGINT, (x'401DD1B2B7A72AB4')::BIGINT, (x'6F5903B64BBD50D7')::BIGINT, (x'9067F2A3CB7EBB34')::BIGINT,
    (x'2C7807F6E28CD962')::BIGINT, (x'42B2DB1AED17AFD5')::BIGINT, (x'F42857186A09F927')::BIGINT, (x'258AB4C5E9986E0C')::BIGINT,
    (x'8F8BA0BC0571B0F6')::BIGINT, (x'CB7AC29208905214')::BIGINT, (x'89914660AA38896A')::BIGINT, (x'380CAC8536886A07')::BIGINT,
    (x'7B7F399C6EC9563D')::BIGINT, (x'B9CBFBEEE95F05B1')::BIGINT, (x'46AAAFC2159C8F19')::BIGINT, (x'DCE680A0882BCCEF')::BIGINT,
    (x'347DDA36E42E2D99')::BIGINT, (x'48EAF8CB224BCBB6')::BIGINT, (x'1EACE389084B2674')::BIGINT, (x'261BC842C735974D')::BIGINT,
    (x'F334875975FD986B')::BIGINT, (x'F6B436CC3615018C')::BIGINT, (x'6658B92BCD930893')::BIGINT, (x'4B6BF5B88078852E')::BIGINT,
    (x'30461B28416207E1')::BIGINT, (x'A2EE71D448670786')::BIGINT, (x'0C8CD31138E8A683')::BIGINT, (x'5FF41A78E0014DD0')::BIGINT,
    (x'A57DE5B0A848A8E0')::BIGINT, (x'71D68A3A9600F975')::BIGINT, (x'EBB35D1E75B69FFB')::BIGINT, (x'FCC1D327A771464B')::BIGINT,
    (x'B6C1F45DB043CF56')::BIGINT, (x'92EF77063313F3CE')::BIGINT, (x'5F48E97C72B167E2')::BIGINT, (x'5806342E75C7BB6B')::BIGINT,
    (x'056C0070CF70F8DD')::BIGINT, (x'81AD4662EE05E75A')::BIGINT, (x'1C10E0B05C7B3C49')::BIGINT, (x'C684C383E396972A')::BIGINT,
    (x'35747D859FC1A7E1')::BIGINT, (x'FDDCF0169427CEE5')::BIGINT, (x'3952CBE313B377E2')::BIGINT, (x'DA1B5C1E5BCCB131')::BIGINT,
    (x'10B747061AF1559C')::BIGINT, (x'8388EDE0F8EDCBFC')::BIGINT, (x'4873A84B8ABF0FA8')::BIGINT, (x'B30E990FE793A807')::BIGINT,
    (x'6B8C5C09C78D0A9F')::BIGINT, (x'7A488BC144D344AA')::BIGINT, (x'C04C26EC10A73AE9')::BIGINT, (x'9278CDFF5EBA539E')::BIGINT,
    (x'FF3A1C4370FB8703')::BIGINT, (x'70D16AE16E34B5B5')::BIGINT, (x'2732FCB913E2E5C3')::BIGINT, (x'C59EE008F48EB1FA')::BIGINT,
    (x'38C7DB9E584C3680')::BIGINT, (x'16F9194953701E88')::BIGINT, (x'2E4C5FAC8051C8FD')::BIGINT, (x'27DD41B7B5D864FA')::BIGINT,
    (x'ECA6A65750A55AEA')::BIGINT, (x'5EBECF093FF380E7')::BIGINT, (x'AA4B94B2F0585B82')::BIGINT, (x'C88047DA71E83C4D')::BIGINT,
    (x'B55C0550FE29B255')::BIGINT, (x'2A1F8CB7ECEB6200')::BIGINT, (x'7B841E8CB0945F2D')::BIGINT, (x'6E5D5D9ED39B4961')::BIGINT,
    (x'67222624CF379C81')::BIGINT, (x'0CC24C31A8BA7F90')::BIGINT, (x'4B54A89AF4A4DF4C')::BIGINT, (x'91A6E548CAC3E417')::BIGINT,
    (x'83DEB6121EA84F96')::BIGINT, (x'9F951DD98DBB9DF5')::BIGINT, (x'25584291FF15CB27')::BIGINT, (x'3371E38504F0CB1F')::BIGINT,
    (x'0EB9B65B5DE9637D')::BIGINT, (x'2D292615860C0AE9')::BIGINT, (x'8DAFEA0E2F5DD418')::BIGINT, (x'5E30A0DFD3A6BA54')::BIGINT,
    (x'5C442D0B47BF7364')::BIGINT, (x'7C2EEBB8C8734B16')::BIGINT, (x'440AF7B361B686A6')::BIGINT, (x'BC6C2CB56CE41EDD')::BIGINT,
    (x'2DF7B73E01F1DB6C')::BIGINT, (x'D76592C6235F3FFE')::BIGINT, (x'2139DC845CE8D2B0')::BIGINT, (x'42168DFAFC8BF871')::BIGINT,
    (x'B0E6598E3C707763')::BIGINT, (x'D7C55A777BF74808')::BIGINT, (x'04C0175493D0C6EC')::BIGINT, (x'78B2B49BF53DD1A5')::BIGINT,
    (x'C6020A48A9F0B2CD')::BIGINT, (x'08CA89D2EA10FEB4')::BIGINT, (x'07DE9D1304C3B2BD')::BIGINT, (x'769EF9C4E120BC5F')::BIGINT,
    (x'EEBD5772D344846F')::BIGINT, (x'1E972C3F168BBF7F')::BIGINT, (x'0C72560FAD1EC422')::BIGINT, (x'D85B15F5DBCD19B9')::BIGINT
  ];

  hash BIGINT := 0;
  mask square_set;
BEGIN
  FOR i IN 0..63 LOOP
    mask := 1::square_set << i;
    IF (gp.blacks & mask) <> 0 THEN
      hash := hash # zobrist_bitstrings[i + 1]; 
    END IF;
    IF (gp.whites & mask) <> 0 THEN
      hash := hash # zobrist_bitstrings[i + 65]; 
    END IF;
  END LOOP;
  IF gp.player = 1 THEN
    hash := ~hash;
  END IF;
  RETURN hash;
END;
$$ LANGUAGE plpgsql IMMUTABLE;
