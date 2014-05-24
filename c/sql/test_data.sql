--
-- test_data.sql
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
-- Load the file by running the command: \i test_data.sql
--
--
-- This script creates the schema and populate data for testing the reversi program.
--

SET search_path TO reversi;


--
-- The game_position_test_data table holds sample game positions.
--
-- DROP TABLE IF EXISTS game_position_test_data;
--
CREATE TABLE game_position_test_data (id          VARCHAR,
                                      gp          game_position,
                                      description VARCHAR,
                                      PRIMARY KEY(id));

-- Populates the game_position_test_data table.
INSERT INTO game_position_test_data (id, gp, description) VALUES
  ('empty',                   game_position_from_string('................................................................b'), 'The empty position, black to move.'),
  ('initial',                 game_position_from_string('...........................wb......bw...........................b'), 'The initial position.'),
  ('first-move-d3',           game_position_from_string('...................b.......bb......bw...........................w'), 'First move D3.'),
  ('black-has-to-pass',       game_position_from_string('wb.b.w..bbbbbbbw.bwwbbww.bwbwwww.bwbwwww.bwbbwbw.bwbbbb.wwwwwwbwb'), 'The black player has to pass.'),
  ('early-game-b-9-moves',    game_position_from_string('...bbb......b......bbww....bb......bb.......b...................w'), 'The board after nine moves of a generic game.'),
  ('early-game-bc3-10-moves', game_position_from_string('...bbb......b.....wwwww....bb......bb.......b...................b'), 'The board after ten moves of a generic game.'),
  ('early-game-bc6-10-moves', game_position_from_string('...bbb......b......bbww....bw......wb.....w.b...................b'), 'The board after ten moves of a generic game.'),
  ('early-game-c-12-moves',   game_position_from_string('................wbbb..w..w.wbw...wwbw.....bb.w..................b'), 'The board after twelve moves of a generic game.'),
  ('final-b37-w27',           game_position_from_string('wwwwwbbbwwwbbbbbwwwbbbwbwwbwbbwbbbwbwbwbbwbwbwbbbbbbbbwbbbbbwwwwb'), 'A final position, all square filled.'),
  --
  --
  --
  -- Entries for testing game_position_make_move function.
  -- Game positions come in pairs:
  -- - make-move-test-case-x-before is a game position configuration
  -- - make-move-test-case-x-after is the expected configuration after a defined move
  --  
  --                                                         |1       2       3       4       5       6       7       8       .|
  --                                                         |ABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHP|
  ('make-move-test-case-a-before', game_position_from_string('.........wwwww...wbbbw...wb.bw...wbbbw...wwwww..................w'), 'Test case MAKE MOVE A: before the move.'),
  ('make-move-test-case-a-after',  game_position_from_string('.........wwwww...wwwww...wwwww...wwwww...wwwww..................b'), 'Test case MAKE MOVE A: after the d4 move.'),
  ('make-move-test-case-b-before', game_position_from_string('wwwwwww.wbbbbbw.wbbbbbw.wbb.bbw.wbbbbbw.wbbbbbw.wwwwwww.........w'), 'Test case MAKE MOVE B: before the move.'),
  ('make-move-test-case-b-after',  game_position_from_string('wwwwwww.wwbwbww.wbwwwbw.wwwwwww.wbwwwbw.wwbwbww.wwwwwww.........b'), 'Test case MAKE MOVE B: after the d4 move.'),
  ('make-move-test-case-c-before', game_position_from_string('bbbbbbbwbbbbbbbwbbbbbbbwbbb.bbbwbbbbbbbwbbbbbbbwbbbbbbbwwwwwwwwww'), 'Test case MAKE MOVE C: before the move.'),
  ('make-move-test-case-c-after',  game_position_from_string('bbbbbbbwbbbbbbbwbbbbbbbwbbbwwwwwbbbwwbbwbbbwbwbwbbbwbbwwwwwwwwwwb'), 'Test case MAKE MOVE C: after the d4 move.'),
  ('make-move-test-case-d-before', game_position_from_string('.b..w....w.w....bbb.....b.b.w...bbb......w.b.....b..w....w......w'), 'Test case MAKE MOVE D: before the move.'),
  ('make-move-test-case-d-after',  game_position_from_string('.b..w....w.w....bww.....bwb.w...bww......w.w.....b..w....w......b'), 'Test case MAKE MOVE D: after the b4 move.'),
  --
  --
  --
  --                                              |1       2       3       4       5       6       7       8       .|
  --                                              |ABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHABCDEFGHP|
  ('ffo-01-simplified', game_position_from_string('..bbbbb..wwwbb.w.wwwbbww.wbwwwbwwbbbwwbw..bwbwww.bbbwwww.wwwwwb.b'), 'From position ffo-01 executing two moves, A8 wins +18'),
  --
  --
  --
  ('all-blacks',              game_position_from_string('bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbw'), 'All blacks, white to move.');
  -- Template string used to insert a new entry: ('', game_position_from_string(''), ''),




