--
-- 0103_up_patterns.sql
--
-- This file is part of the reversi program
-- http://github.com/rcrr/reversi
--
-- Author: Roberto Corradini mailto:rob_corradini@yahoo.it
-- Copyright 2018 Roberto Corradini. All rights reserved.
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
--
-- Creates schema for game position patterns.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0103, now(), 'patterns', 'creates tables for patterns and game position classification');

---
--- Table regab_prng_patterns
--- Patterns are enumarated exactly as the board_pattern_id_t enum as defined in board_pattern.h
---
CREATE TABLE regab_prng_patterns (pattern_id      SMALLINT    PRIMARY KEY,
                                  pattern_name    CHAR(6)     NOT NULL UNIQUE,
                                  ins_time        TIMESTAMP,
                                  ninstances      SMALLINT    NOT NULL,
                                  nsquares        SMALLINT    NOT NULL,
                                  description     TEXT,
                                  CONSTRAINT ninstances_is_positive CHECK(ninstances > 0),
                                  CONSTRAINT nsquares_is_positive CHECK(nsquares > 0));

--
-- Table regab_prng_pattern_ranges
--
CREATE TABLE regab_prng_pattern_ranges (seq                   BIGSERIAL        PRIMARY KEY,
                                        ---
                                        pattern_id            SMALLINT         REFERENCES regab_prng_patterns(pattern_id) ON DELETE CASCADE,
                                        index_value           INTEGER,
                                        ---
                                        mirror_value          INTEGER          DEFAULT NULL,
                                        principal_index_value INTEGER          DEFAULT NULL,
                                        ---
                                        UNIQUE (pattern_id, index_value)
                                        );

--
-- Table regab_prng_pattern_probs
--
-- Example query:
-- SELECT empty_count AS ec, index_prob_given_ec AS prob FROM regab_prng_pattern_probs AS ps JOIN regab_prng_pattern_ranges AS rs ON ps.range_id = rs.seq WHERE rs. pattern_id = 0 AND index_value = 360 ORDER BY empty_count;
--
CREATE TABLE regab_prng_pattern_probs (range_id           BIGINT            REFERENCES regab_prng_pattern_ranges(seq) ON DELETE CASCADE,
                                       empty_count        SMALLINT,
                                       ---
                                       index_prob_given_ec DOUBLE PRECISION DEFAULT NULL,
                                       ---
                                       UNIQUE (range_id, empty_count)
                                       );

--
-- Table used to load csv file having format:
--   EMPTY_COUNT;PATTERN_INDEX;COUNT
--
-- The CSV file has been produced by a command like this:
-- rcrr@hypnotic:~/base/prj/reversi/c$ ./build/bin/endgame_solver -f db/gpdb-sample-games.txt -q initial -s rand -n 1000000000 -P EDGE -r 628
--
CREATE TABLE regab_staging_ec_pidx_cnt_tmp (empty_count   SMALLINT,
                                            index_value   INTEGER,
                                            frequency     BIGINT);

--
-- The table is an extension of regab_prng_gp.
-- It records the index values for all the pattern instances corresponding to the game position being referenced.
--
CREATE TABLE regab_prng_gp_pattern_class (gp_id      BIGINT PRIMARY KEY REFERENCES regab_prng_gp(seq) ON DELETE CASCADE,
                                          ---
                                          ins_time   TIMESTAMP DEFAULT now(),
                                          status     CHAR(3)   DEFAULT 'INS',
                                          cst_time   TIMESTAMP DEFAULT now(),
                                          ---
                                          i_edge_0   INTEGER,
                                          i_edge_1   INTEGER,
                                          i_edge_2   INTEGER,
                                          i_edge_3   INTEGER,
                                          ---
                                          i_corner_0 INTEGER,
                                          i_corner_1 INTEGER,
                                          i_corner_2 INTEGER,
                                          i_corner_3 INTEGER,
                                          ---
                                          i_xedge_0  INTEGER,
                                          i_xedge_1  INTEGER,
                                          i_xedge_2  INTEGER,
                                          i_xedge_3  INTEGER,
                                          ---
                                          i_r2_0     INTEGER,
                                          i_r2_1     INTEGER,
                                          i_r2_2     INTEGER,
                                          i_r2_3     INTEGER,
                                          ---
                                          i_r3_0     INTEGER,
                                          i_r3_1     INTEGER,
                                          i_r3_2     INTEGER,
                                          i_r3_3     INTEGER,
                                          ---
                                          i_r4_0     INTEGER,
                                          i_r4_1     INTEGER,
                                          i_r4_2     INTEGER,
                                          i_r4_3     INTEGER,
                                          ---
                                          i_diag4_0  INTEGER,
                                          i_diag4_1  INTEGER,
                                          i_diag4_2  INTEGER,
                                          i_diag4_3  INTEGER,
                                          ---
                                          i_diag5_0  INTEGER,
                                          i_diag5_1  INTEGER,
                                          i_diag5_2  INTEGER,
                                          i_diag5_3  INTEGER,
                                          ---
                                          i_diag6_0  INTEGER,
                                          i_diag6_1  INTEGER,
                                          i_diag6_2  INTEGER,
                                          i_diag6_3  INTEGER,
                                          ---
                                          i_diag7_0  INTEGER,
                                          i_diag7_1  INTEGER,
                                          i_diag7_2  INTEGER,
                                          i_diag7_3  INTEGER,
                                          ---
                                          i_diag8_0  INTEGER,
                                          i_diag8_1  INTEGER,
                                          ---
                                          i_2x5cor_0 INTEGER,
                                          i_2x5cor_1 INTEGER,
                                          i_2x5cor_2 INTEGER,
                                          i_2x5cor_3 INTEGER,
                                          i_2x5cor_4 INTEGER,
                                          i_2x5cor_5 INTEGER,
                                          i_2x5cor_6 INTEGER,
                                          i_2x5cor_7 INTEGER,
                                          ---
                                          i_diag3_0  INTEGER,
                                          i_diag3_1  INTEGER,
                                          i_diag3_2  INTEGER,
                                          i_diag3_3  INTEGER,
                                          ---
                                          CHECK (status IN ('INS', 'WIP', 'CMP')));

COMMIT;
