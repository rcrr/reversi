--
-- 0000_up_regab_create_schema.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2017, 2018 Roberto Corradini. All rights reserved.
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
-- Load the file by running the command: \i regab_create_schema.sql
--
--
-- This script extends the schema used by the reversi program by adding the regab batch,
-- and positions tables.
--

SET search_path TO reversi;

--
-- Table regab_connection_log logs into the database all the connection done by the regab C program.
--
CREATE TABLE regab_connection_log (seq       SERIAL     PRIMARY KEY,
                                   con_time  TIMESTAMP);

--
-- Table regab_prng_gp_h contains the batches of random games created by running the regab C program
-- with the -a generate flag.
--
-- seq:                       Unique auto increment record id.
-- ins_time:                  Insertion timespamp.
-- status:                    Status field (INS for "inserted", CMP for "completed").
-- prng_seed:                 Integer used to initialize the PRNG sequence.
-- ngames:                    Number of games that are part of the batch.
-- npositions:                Number of game positions collected into the batch.
--
CREATE TABLE regab_prng_gp_h (seq            SERIAL     PRIMARY KEY,
                              ins_time       TIMESTAMP,
                              status         CHAR(3),
                              prng_seed      BIGINT     UNIQUE,
                              ngames         INTEGER,
                              npositions     INTEGER);

--
-- Table regab_prng_gp contains the game positions belonging to the random games that are part of the baches
-- described by regab_prng_gp_h table.
--
-- seq:                       Unique auto increment record id.
--
-- batch_id:                  Foreign key on the header table. Fields batch_id, game_id, pos_id together are the key of the relation.
-- game_id:                   Game id for a random game.
-- pos_id:                    Position id for a game position in a random game.
--
-- ins_time:                  Insertion timespamp.
-- status:                    Status field (INS for "inserted", WIP for "work in progres", CMP for "computed").
-- cst_time:                  Timestamp for the last change on the status field (CST: Change STatus).
--
-- mover:                     The set of disks owned by the player
-- opponent:                  The set of disks owned by the opponent
-- player:                    Player having to move.
--
-- empty_count:               Count of empty squares.
-- legal_move_set:            Set of legal moves for the position.
-- legal_move_count:          Count of legal moves for the position.
-- legal_move_count_adjusted: Equal to legal_move_count with the exception of a PASS position, when the value is one instead of zero.
-- parent_move:               Move just played to arrive at the given game position. 
--
-- game_value:                Exact value of the game.
-- best_move:                 The best move returned by the a-b search, could be that other moves have the same value.
--
CREATE TABLE regab_prng_gp (seq                       SERIAL     PRIMARY KEY,
                            --
                            batch_id                  INTEGER    REFERENCES regab_prng_gp_h (seq) ON DELETE CASCADE,
                            game_id                   INTEGER    NOT NULL,
                            pos_id                    INTEGER    NOT NULL,
                            --
                            ins_time                  TIMESTAMP,
                            status                    CHAR(3),
                            cst_time                  TIMESTAMP,
                            --
                            mover                     SQUARE_SET,
                            opponent                  SQUARE_SET,
                            player                    PLAYER,
                            --
                            empty_count               SMALLINT,
                            legal_move_set            SQUARE_SET,
                            legal_move_count          SMALLINT,
                            legal_move_count_adjusted SMALLINT,
                            parent_move               GAME_MOVE,
                            --
                            game_value                SMALLINT   DEFAULT 0,
                            best_move                 GAME_MOVE  DEFAULT 'UN',
                            leaf_count                BIGINT     DEFAULT 0,
                            node_count                BIGINT     DEFAULT 0,
                            --
                            UNIQUE (batch_id, game_id, pos_id));
