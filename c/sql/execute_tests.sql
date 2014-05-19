--
-- execute_tests.sql
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
-- Load the file by running the command: \i execute_tests.sql
--
--
-- This script creates the functions used by the reversi program.
--

SET search_path TO reversi;

SELECT test_bit_works_bitscanMS1B_8();
SELECT test_bit_works_bitscanLS1B_64();
SELECT test_bit_works_fill_in_between_8();
SELECT test_bit_works_highest_bit_set_8();
SELECT test_bit_works_lowest_bit_set_8();
SELECT test_bit_works_signed_left_shift();

SELECT test_player_to_string();
SELECT test_player_opponent();

SELECT test_square_populate_addictional_fields();
SELECT test_square_get_column();
SELECT test_square_get_row();

SELECT test_square_set_to_string();
SELECT test_square_set_from_string();
SELECT test_square_set_to_array();

SELECT test_axis_transform_to_row_one();
SELECT test_axis_transform_back_from_row_one();
SELECT test_axis_move_ordinal_position_in_bitrow();
SELECT test_axis_shift_distance();

SELECT test_direction_shift_square_set();
SELECT test_direction_shift_back_square_set_by_amount();

SELECT test_board_populate_bitrow_changes_for_player();
SELECT test_board_bitrow_changes_for_player();

SELECT test_game_position_to_string();
SELECT test_game_position_from_string();
SELECT test_game_position_empties();
SELECT test_game_position_get_square_set_for_player_opponent();
SELECT test_game_position_is_move_legal();
SELECT test_game_position_legal_moves();
SELECT test_game_position_make_move();
