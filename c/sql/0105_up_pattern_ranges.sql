--
-- 0105_up_pattern_ranges.sql
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
-- Creates a new table regab_prng_pattern_ranges
--

SET search_path TO reversi;

BEGIN;


-- Migration set-up
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0105, now(), 'pattern_ranges', 'adds a new table regab_prng_pattern_ranges that contains pattern indexes and their properties.');


--
-- Table regab_prng_pattern_ranges
--
CREATE TABLE regab_prng_pattern_ranges (seq                   SERIAL    PRIMARY KEY,
                                        ---
                                        ins_time              TIMESTAMP DEFAULT now(),
                                        status                CHAR(3)   DEFAULT 'INS',
                                        cst_time              TIMESTAMP DEFAULT now(),
                                        ---
                                        pattern_id            INTEGER   REFERENCES regab_prng_patterns(seq) ON DELETE CASCADE,
                                        index_value           INTEGER,
                                        empty_count           SMALLINT,
                                        mirror_value          INTEGER   DEFAULT NULL,
                                        principal_index_value INTEGER   DEFAULT NULL,
                                        index_prob_given_ec   DOUBLE PRECISION DEFAULT NULL,
                                        ---
                                        UNIQUE (pattern_id, index_value, empty_count),
                                        CHECK (status IN ('INS', 'WIP', 'CMP')));

                             
--
-- Populates the table regab_prng_pattern_ranges with the data appropriate for the function argument.
--
CREATE FUNCTION ragab_populate_pattern_ranges (pattern_name_arg CHAR(6))
RETURNS INTEGER
AS $$
DECLARE
  nrecords INTEGER;
  pattern_rec RECORD;
  index_count INTEGER;
BEGIN
  nrecords := 0;
  SELECT INTO pattern_rec seq, pattern_name, nsquares, ninstances FROM regab_prng_patterns WHERE pattern_name = pattern_name_arg;
  IF pattern_rec.seq IS NULL THEN
    RAISE EXCEPTION 'Pattern record in table regab_prng_patterns has not been found.';
  END IF;
  index_count := 3^pattern_rec.nsquares;

  WITH RECURSIVE index_value_range AS (
    SELECT
      0::INTEGER AS val
    UNION ALL SELECT val + 1::INTEGER AS val
    FROM
      index_value_range
    WHERE
      index_value_range.val < index_count - 1
  ), empty_count_range AS (
    SELECT
      0::INTEGER AS val
    UNION ALL SELECT val + 1::INTEGER AS val
    FROM
      empty_count_range
    WHERE
      empty_count_range.val < 60   
  ), pattern_range_key AS (
    SELECT
      ivr.val AS index_value,
      ecr.val AS empty_count
    FROM
      index_value_range AS ivr
    CROSS JOIN
      empty_count_range AS ecr
  )
  INSERT INTO regab_prng_pattern_ranges (pattern_id, index_value, empty_count)
  SELECT pattern_rec.seq, index_value, empty_count FROM pattern_range_key;

  SELECT count(1) INTO nrecords FROM regab_prng_pattern_ranges WHERE pattern_id = pattern_rec.seq;
  
  RETURN nrecords;
END;
$$ LANGUAGE plpgsql VOLATILE;

---
--- Populates the mirror_value and principal_index_value field in table regab_prng_pattern_ranges.
---
--DO $$
--DECLARE
--  pn CHAR(6) := 'EDGE';
--  pid INTEGER;
--BEGIN
--  SELECT seq INTO pid FROM regab_prng_patterns WHERE pattern_name = pn;
--  PERFORM ragab_populate_pattern_ranges(pn);
--  UPDATE regab_prng_pattern_ranges SET mirror_value = regab_mirror_value_edge_pattern(index_value) WHERE pattern_id = pid;
--  UPDATE regab_prng_pattern_ranges SET principal_index_value = least(index_value, mirror_value) WHERE pattern_id = pid;
--END $$;

--- Tests.
--DO $$
--DECLARE
--  computed INTEGER;
--BEGIN
--  SELECT count(distinct(index_value)) INTO computed FROM regab_prng_pattern_ranges;
--  PERFORM p_assert(computed = 6561, 'Expected value is 6561.');
--  --
--  SELECT count(distinct(principal_index_value)) INTO computed FROM regab_prng_pattern_ranges;
--  PERFORM p_assert(computed = 3321, 'Expected value is 3321, ((6561 - 3^4) / 2) + 3^4.');
--END $$;


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

---
--- Loads the CSV file into the tmp staging table.
---
--\COPY regab_staging_ec_pidx_cnt_tmp  FROM '0105_data_pattern_index_frequencies_EDGE_826_1billion.sql' WITH (FORMAT CSV, DELIMITER ';', HEADER true);

---
--- Populates the probability field in table regab_prng_patterns
---
--WITH freq_totals_by_ec AS (
--  SELECT empty_count, sum(frequency) AS cnt
--  FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count
--), frequencies AS (
--  SELECT empty_count, index_value, sum(frequency) AS cnt
--  FROM regab_staging_ec_pidx_cnt_tmp GROUP BY empty_count, index_value ORDER BY empty_count
--), probabilities AS (
--  SELECT
--    f.empty_count AS empty_count,
--    f.index_value AS index_value,
--    f.cnt / ft.cnt AS probability
--  FROM
--    freq_totals_by_ec AS ft
--  LEFT JOIN
--    frequencies AS f ON f.empty_count = ft.empty_count
--  ORDER BY
--    empty_count, index_value
--) UPDATE regab_prng_pattern_ranges AS ta
--SET
--  index_prob_given_ec = probability,
--  cst_time = now(),
--  status = 'CMP'
--FROM probabilities AS tb
--WHERE
--  ta.pattern_id = (SELECT seq FROM regab_prng_patterns WHERE pattern_name = 'EDGE') AND
--  ta.index_value = tb.index_value AND
--  ta.empty_count = tb.empty_count;
--
--TRUNCATE regab_staging_ec_pidx_cnt_tmp;

-- End of migration
COMMIT;

VACUUM ANALYZE regab_prng_pattern_ranges;
