--
-- create_schema.sql
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
-- Load the file by running the command: \i create_schema.sql
--
--
-- This script creates the schema used by the reversi program.
--

CREATE SCHEMA reversi;

SET search_path TO reversi;



--
-- DOMAIN square_set
--
CREATE DOMAIN square_set AS BIGINT;



--
-- ENUM axis
-- The axes are the lines that pass throw a square, a general square has four axes.
--
-- HO: Horizontal axis (W-E).
-- VE: Vertical axis (N-S).
-- DD: Diagonal Down axis (NW-SE), A1-H8.
-- DU: Diagonal Up axis (NE-SW), A8-H1.
--
-- DROP TYPE IF EXISTS axis;
--
CREATE TYPE axis AS ENUM ('HO', 'VE', 'DD', 'DU');

--
-- The axis_info table holds all info related to Axes.
--
-- DROP TABLE IF EXISTS axis_info;
--
CREATE TABLE axis_info (id      axis,
                        ordinal SMALLINT,
                        PRIMARY KEY(id));

-- Populates the axis_info table.
INSERT INTO axis_info (id, ordinal) VALUES
  ('HO', 0),
  ('VE', 1),
  ('DD', 2),
  ('DU', 3);



--
-- ENUM square
-- Square is an enum that realize the base unit of the game board.
--
-- Squares are represented by two characters, a letter and a numeric digit.
--
-- For instance, let's take a square: D4.
-- This symbol identifies the square at the cross of column d and row 4.
--
-- Here is represented the collection of the 64 square as them are
-- organized in the game board:
-- 
--      a    b    c    d    e    f    g    h
--   =========================================
-- 1 = A1 = B1 = C1 = D1 = E1 = F1 = G1 = H1 =
--   =========================================
-- 2 = A2 = B2 = C2 = D2 = E2 = F2 = G2 = H2 =
--   =========================================
-- 3 = A3 = B3 = C3 = D3 = E3 = F3 = G3 = H3 =
--   =========================================
-- 4 = A4 = B4 = C4 = D4 = E4 = F4 = G4 = H4 =
--   =========================================
-- 5 = A5 = B5 = C5 = D5 = E5 = F5 = G5 = H5 =
--   =========================================
-- 6 = A6 = B6 = C6 = D6 = E6 = F6 = G6 = H6 =
--   =========================================
-- 7 = A7 = B7 = C7 = D7 = E7 = F7 = G7 = H7 =
--   =========================================
-- 8 = A8 = B8 = C8 = D8 = E8 = F8 = G8 = H8 =
--   =========================================
-- 
-- Has to be noticed that the sequence of the squares is organized by rows. It means that
-- the square ordered list is as follow:
--
-- (A1, B1, C1, D1, E1, F1, G1, H1, A2, ... H8).
--
CREATE TYPE square AS ENUM (
  'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
  'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
  'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
  'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
  'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5',
  'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6',
  'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7',
  'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'H8');

--
-- The square_info table holds all info related to squares.
--
-- DROP TABLE IF EXISTS square_info;
--
CREATE TABLE square_info (id                           square,
                          ordinal                      SMALLINT,
                          sq_column                    SMALLINT,
                          sq_row                       SMALLINT,
                          move_mask_for_all_directions square_set,
                          PRIMARY KEY(id));

-- Populates the square_info table.
INSERT INTO square_info (id, ordinal) VALUES
  ('A1',  0), ('B1',  1), ('C1',  2), ('D1',  3), ('E1',  4), ('F1',  5), ('G1',  6), ('H1',  7),
  ('A2',  8), ('B2',  9), ('C2', 10), ('D2', 11), ('E2', 12), ('F2', 13), ('G2', 14), ('H2', 15),
  ('A3', 16), ('B3', 17), ('C3', 18), ('D3', 19), ('E3', 20), ('F3', 21), ('G3', 22), ('H3', 23),
  ('A4', 24), ('B4', 25), ('C4', 26), ('D4', 27), ('E4', 28), ('F4', 29), ('G4', 30), ('H4', 31),
  ('A5', 32), ('B5', 33), ('C5', 34), ('D5', 35), ('E5', 36), ('F5', 37), ('G5', 38), ('H5', 39),
  ('A6', 40), ('B6', 41), ('C6', 42), ('D6', 43), ('E6', 44), ('F6', 45), ('G6', 46), ('H6', 47),
  ('A7', 48), ('B7', 49), ('C7', 50), ('D7', 51), ('E7', 52), ('F7', 53), ('G7', 54), ('H7', 55),
  ('A8', 56), ('B8', 57), ('C8', 58), ('D8', 59), ('E8', 60), ('F8', 61), ('G8', 62), ('H8', 63);



--
-- ENUM direction
-- The directions that are available in a regular board's square are
-- eight, Up, Down, Left, Right, and the four diagonal between them.
--
-- Each regular square has eight neighbor ones,
-- each identified by the proper direction. Boundary squares have fewer neighbors.
-- 
-- The direction enum is represented by the respective cardinal point literal.
--
--
--  NW: North-West direction.
--  N:  North direction.
--  NE: North-East direction.
--  W:  West direction.
--  E:  East direction.
--  SW: South-West direction.
--  S:  South direction.
--  SE: South-Est direction.
--
-- DROP TYPE IF EXISTS direction;
--
CREATE TYPE direction AS ENUM ('NW', 'N', 'NE', 'W', 'E', 'SW', 'S', 'SE');

--
-- The direction_info table holds all info related to directions.
--
-- DROP TABLE IF EXISTS direction_info;
--
CREATE TABLE direction_info (id       direction,
                             ordinal  SMALLINT,
                             opposite direction,
                             PRIMARY KEY(id));

-- Populates the direction_info table.
INSERT INTO direction_info (id, ordinal, opposite) VALUES
  ('NW', 0, 'SE'),
  ('N',  1, 'S' ),
  ('NE', 2, 'SW'),
  ('W',  3, 'E' ),
  ('E',  4, 'W' ),
  ('SW', 5, 'NE'),
  ('S',  6, 'N' ),
  ('SE', 7, 'NW');



--
-- ENUM color
--
CREATE TYPE color AS ENUM ('BLACK', 'WHITE', 'EMPTY');

--
-- The color_info table holds all info related to colors.
--
-- DROP TABLE IF EXISTS color_info;
--
CREATE TABLE color_info (id      color,
                         ordinal SMALLINT,
                         PRIMARY KEY(id));

-- Populates the color_info table.
INSERT INTO color_info (id, ordinal) VALUES
  ('BLACK', 0),
  ('WHITE', 1),
  ('EMPTY', 2);



--
-- DOMAIN player
--
CREATE DOMAIN player AS SMALLINT CHECK (VALUE = 0 OR VALUE = 1);

--
-- The player_info table holds all info related to players.
--
-- DROP TABLE IF EXISTS player_info;
--
CREATE TABLE player_info (id    player,
                          color color,
                          PRIMARY KEY(id));

-- Populates the player_info table.
INSERT INTO player_info (id, color) VALUES
  (0, 'BLACK'),
  (1, 'WHITE');



--
-- TYPE game_position
--
CREATE TYPE game_position AS (
  blacks square_set,
  whites square_set,
  player player
);



--
-- TYPE transient_board
--
CREATE TYPE transient_board AS (
  p_square_set square_set,
  o_square_set square_set
);



--
-- TYPE search_node
--
CREATE TYPE search_node AS (
  game_move  square,
  game_value SMALLINT
);



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
-- DROP TABLE IF EXISTS game_tree_log_header;
--
CREATE TABLE game_tree_log_header (run_id       SERIAL     PRIMARY KEY,
                                   run_label    CHAR(4)    NOT NULL UNIQUE,
                                   engine_id    CHAR(20)   NOT NULL,
                                   run_date     TIMESTAMP  NOT NULL,
                                   description  TEXT);



--
-- DROP TABLE IF EXISTS game_tree_log;
--
CREATE TABLE game_tree_log (run_id       INTEGER   REFERENCES game_tree_log_header (run_id) ON DELETE CASCADE,
                            sub_run_id   SMALLINT  NOT NULL,
                            call_id      INTEGER   NOT NULL,
                            hash         BIGINT,
                            parent_hash  BIGINT,
                            blacks       square_set,
                            whites       square_set,
                            player       player,
                            PRIMARY KEY(run_id, sub_run_id, call_id));



--
-- DROP TABLE IF EXISTS game_tree_log_staging;
--
CREATE TABLE game_tree_log_staging (sub_run_id   SMALLINT  NOT NULL,
                                    call_id      INTEGER   NOT NULL,
                                    hash         BIGINT,
                                    parent_hash  BIGINT,
                                    blacks       square_set,
                                    whites       square_set,
                                    player       player,
                                    PRIMARY KEY(sub_run_id, call_id));
