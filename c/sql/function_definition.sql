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
  ret BOOLEAN;
BEGIN
  PERFORM p_assert(player = 0 OR player = 1, 'Parameter player must be in the range 0..1.');
  ret := FALSE;
  return ret;
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