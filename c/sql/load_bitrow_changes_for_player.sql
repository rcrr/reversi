--
-- load_bitrow_changes_for_player.sql
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
-- Load the file by running the command: \i load_bitrow_changes_for_player.sql
--
--
-- This script creates a table named board_bitrow_changes_for_player_from_csv, and
-- populates it according to the file "../out/bitrow_changes_for_player.csv".
-- If the table already exists it is deleted and recreated.
--

SET search_path TO reversi;

DROP TABLE IF EXISTS board_bitrow_changes_for_player_from_csv;

CREATE TABLE IF NOT EXISTS board_bitrow_changes_for_player_from_csv(array_index    INT,
                                                                    player_row     SMALLINT,
                                                                    opponent_row   SMALLINT,
                                                                    move_position  SMALLINT,
                                                                    player_changes SMALLINT,
                                                                    PRIMARY KEY(array_index));


TRUNCATE TABLE board_bitrow_changes_for_player_from_csv;

\COPY board_bitrow_changes_for_player_from_csv FROM '../out/bitrow_changes_for_player.csv' WITH (FORMAT CSV, DELIMITER ';', HEADER true);

VACUUM (FULL, ANALYZE) board_bitrow_changes_for_player_from_csv;
