--
-- 0107_up_more_patterns.sql
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
-- Adds more patterns to the regab schema.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0107, now(), 'more_patterns', 'adds all patterns needed by the GLEM implementation as described by M. Buro in his papers');

---
--- Creates the DIAG4, DIAG5, DIAG6, DIAG7, DIAG8, and 2X5COR patterns.
---
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'R2',      4,  8, 'Second row, A2-B2-C2-D2-E2-F2-G2-H2');
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'R3',      4,  8, 'Third row, A3-B3-C3-D3-E3-F3-G3-H3');
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'R4',      4,  8, 'Fourth row, A4-B4-C4-D4-E4-F4-G4-H4');

INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'DIAG4',   4,  4, 'Four square diagonal, D1-C2-B3-A4');
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'DIAG5',   4,  5, 'Five square diagonal, E1-D2-C3-B4-A5');
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'DIAG6',   4,  6, 'Six square diagonal, F1-E2-D3-C4-B5-A6');
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'DIAG7',   4,  7, 'Seven square diagonal, G1-F2-E3-D4-C5-B6-A7');
INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), 'DIAG8',   2,  8, 'Eight square diagonal, H1-G2-F3-E4-D5-C6-B7-A8');

INSERT INTO regab_prng_patterns (ins_time, pattern_name, ninstances, nsquares, description) VALUES (now(), '2X5COR',  8, 10, 'Ten square, asymmetric corner');



---
--- Add column pattern_id, populate it, and add a UNIQUE and NOT NULL constraint.
--- Patterns are enumarated exactly as the board_pattern_id_t enum as defined in board_pattern.h
---
ALTER TABLE regab_prng_patterns ADD COLUMN pattern_id SMALLINT;

UPDATE regab_prng_patterns SET pattern_id =  0 WHERE pattern_name = 'EDGE';
UPDATE regab_prng_patterns SET pattern_id =  1 WHERE pattern_name = 'CORNER';
UPDATE regab_prng_patterns SET pattern_id =  2 WHERE pattern_name = 'XEDGE';
UPDATE regab_prng_patterns SET pattern_id =  3 WHERE pattern_name = 'R2';
UPDATE regab_prng_patterns SET pattern_id =  4 WHERE pattern_name = 'R3';
UPDATE regab_prng_patterns SET pattern_id =  5 WHERE pattern_name = 'R4';
UPDATE regab_prng_patterns SET pattern_id =  6 WHERE pattern_name = 'DIAG4';
UPDATE regab_prng_patterns SET pattern_id =  7 WHERE pattern_name = 'DIAG5';
UPDATE regab_prng_patterns SET pattern_id =  8 WHERE pattern_name = 'DIAG6';
UPDATE regab_prng_patterns SET pattern_id =  9 WHERE pattern_name = 'DIAG7';
UPDATE regab_prng_patterns SET pattern_id = 10 WHERE pattern_name = 'DIAG8';
UPDATE regab_prng_patterns SET pattern_id = 11 WHERE pattern_name = '2X5COR';

ALTER TABLE regab_prng_patterns ALTER COLUMN pattern_id SET NOT NULL;

ALTER TABLE regab_prng_patterns ADD CONSTRAINT regab_prng_patterns_app_key UNIQUE (pattern_id);

COMMIT;
