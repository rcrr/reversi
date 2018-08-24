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
VALUES (0103, now(), 'patterns', 'creates basic tables for patterns and game position classification');

---
--- Table regab_prng_patterns
--- Patterns are enumarated exactly as the board_pattern_id_t enum as defined in board_pattern.h
---
CREATE TABLE regab_prng_patterns (seq             SERIAL      PRIMARY KEY,
                                  pattern_name_id INTEGER     NOT NULL UNIQUE,
                                  pattern_name    CHAR(6)     NOT NULL UNIQUE,
                                  ins_time        TIMESTAMP,
                                  ninstances      SMALLINT    NOT NULL,
                                  nsquares        SMALLINT    NOT NULL,
                                  description     TEXT,
                                  CONSTRAINT ninstances_is_positive CHECK(ninstances > 0),
                                  CONSTRAINT nsquares_is_positive CHECK(nsquares > 0));
---
--- Populates the patter table with EDGE, CORNER, XEDGE, R2, R3, R4, DIAG4, DIAG5, DIAG6, DIAG7, DIAG8, and 2X5COR patterns.
---
INSERT INTO regab_prng_patterns (ins_time, pattern_name_id, pattern_name, ninstances, nsquares, description)
  SELECT * FROM (VALUES
    (now(),  0, 'EDGE',    4,  8, 'The edge of the board'),
    (now(),  1, 'CORNER',  4,  9, 'The 3x3 corner'),
    (now(),  2, 'XEDGE',   4, 10, 'The edge of the board plus X squares'),
    (now(),  3, 'R2',      4,  8, 'Second row, A2-B2-C2-D2-E2-F2-G2-H2'),
    (now(),  4, 'R3',      4,  8, 'Third row, A3-B3-C3-D3-E3-F3-G3-H3'),
    (now(),  5, 'R4',      4,  8, 'Fourth row, A4-B4-C4-D4-E4-F4-G4-H4'),
    (now(),  6, 'DIAG4',   4,  4, 'Four square diagonal, D1-C2-B3-A4'),
    (now(),  7, 'DIAG5',   4,  5, 'Five square diagonal, E1-D2-C3-B4-A5'),
    (now(),  8, 'DIAG6',   4,  6, 'Six square diagonal, F1-E2-D3-C4-B5-A6'),
    (now(),  9, 'DIAG7',   4,  7, 'Seven square diagonal, G1-F2-E3-D4-C5-B6-A7'),
    (now(), 10, 'DIAG8',   2,  8, 'Eight square diagonal, H1-G2-F3-E4-D5-C6-B7-A8'),
    (now(), 11, '2X5COR',  8, 10, 'Ten square, asymmetric corner')
  ) AS tmp_table(ins_time, pattern_name_id, pattern_name, ninstances, nsquares, description);

COMMIT;
