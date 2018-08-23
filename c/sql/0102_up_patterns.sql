--
-- 0102_up_patterns.sql
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
VALUES (0102, now(), 'patterns', 'creates basic tables for patterns and game position classification');

CREATE INDEX regab_prng_gp_bes_idx ON regab_prng_gp (batch_id, empty_count, status);

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
--- Creates the EDGE, CORNER, and XEDGE patterns.
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


---
--- Table regab_prng_gp_classification
---
CREATE TABLE regab_prng_gp_classification_h (seq              SERIAL      PRIMARY KEY,
                                             ---
                                             ins_time         TIMESTAMP,
                                             status           CHAR(3),
                                             ---
                                             pattern_id       INTEGER     REFERENCES regab_prng_patterns (seq) ON DELETE CASCADE,
                                             batch_id         INTEGER     REFERENCES regab_prng_gp_h (seq) ON DELETE CASCADE,
                                             empty_count      SMALLINT,
                                             ---
                                             UNIQUE (pattern_id, batch_id, empty_count),
                                             CHECK (empty_count >= 0 AND empty_count <= 60),
                                             CHECK (status IN ('INS', 'CMP')));


---
--- Table regab_prng_gp_classification
---
CREATE TABLE regab_prng_gp_classification (seq                SERIAL      PRIMARY KEY,
                                           ---
                                           ins_time           TIMESTAMP   DEFAULT now(),
                                           status             CHAR(3)     DEFAULT 'INS',
                                           cst_time           TIMESTAMP   DEFAULT now(),
                                           ---
                                           class_id           INTEGER     REFERENCES regab_prng_gp_classification_h (seq) ON DELETE CASCADE,
                                           gp_id              BIGINT      REFERENCES regab_prng_gp (seq) ON DELETE CASCADE,
                                           pattern_instance   SMALLINT,
                                           ---
                                           index_value        INTEGER     DEFAULT NULL,
                                           ---
                                           UNIQUE (class_id, gp_id, pattern_instance),
                                           CHECK (pattern_instance >= 0),
                                           CHECK (status IN ('INS', 'WIP', 'CMP')));

--
-- Documentation ...
--
CREATE OR REPLACE FUNCTION insert_regab_prng_gp_classification_h (pattern_name_input CHARACTER,
                                                                  batch_id_input     INTEGER,
                                                                  empty_count_input  INTEGER)
RETURNS INTEGER
AS $$
DECLARE
  pid       INTEGER;
  ni        SMALLINT;
  cgid      INTEGER;
  gp_count  BIGINT;
  gpc_count BIGINT;
BEGIN
  SELECT seq INTO STRICT pid FROM regab_prng_patterns WHERE pattern_name = pattern_name_input;
  
  INSERT INTO regab_prng_gp_classification_h (pattern_id, batch_id, empty_count, ins_time, status)
    VALUES (pid, batch_id_input, empty_count_input, now(), 'CMP') RETURNING seq INTO cgid;

  SELECT ninstances INTO STRICT ni FROM regab_prng_patterns WHERE seq = pid;
  SELECT count(1) INTO STRICT gp_count FROM regab_prng_gp WHERE batch_id = batch_id_input AND empty_count = empty_count_input;

  WITH RECURSIVE pattern_instance_range AS (
    SELECT
      0::SMALLINT AS val
    UNION ALL SELECT val + 1::SMALLINT AS val
    FROM
      pattern_instance_range
    WHERE
      pattern_instance_range.val < ni - 1
  ), game_positions AS (
    SELECT seq FROM regab_prng_gp
    WHERE batch_id = batch_id_input AND empty_count = empty_count_input
  ), classification_key AS (
    SELECT
      gp.seq AS gp_id,
      pir.val AS pattern_instance
    FROM
      pattern_instance_range AS pir
    CROSS JOIN
      game_positions AS gp
  )
  INSERT INTO regab_prng_gp_classification (class_id, gp_id, pattern_instance)
  SELECT cgid, gp_id, pattern_instance FROM classification_key;
  
  SELECT count(1) INTO STRICT gpc_count FROM regab_prng_gp_classification
  WHERE class_id = cgid;

  IF gpc_count <> (gp_count * ni) THEN
    RAISE EXCEPTION 'Inserted row count is not consistent.';
  END IF;
   
  RETURN cgid;
END;
$$ LANGUAGE plpgsql;


COMMIT;
