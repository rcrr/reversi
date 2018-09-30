--
-- 0109_up_action_extract.sql
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
-- Functions for the REGAB extract action.
--

SET search_path TO reversi;

BEGIN;
 
INSERT INTO migrations (migration_id, ins_time, label, description)
VALUES (0109, now(), 'action_extract', 'creates functions used by the REGAB extract action');

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_check_batches('{1, 3}');
--
CREATE FUNCTION regab_action_extract_check_batches (IN  batch_id_array_arg INTEGER[],
                                                    ---
                                                    OUT batch_id INTEGER,
                                                    OUT status   CHARACTER(3),
                                                    OUT ngames   INTEGER)
RETURNS SETOF record
AS $$
BEGIN
  RETURN QUERY SELECT ta.seq, ta.status, ta.ngames FROM regab_prng_gp_h AS ta WHERE ta.seq = ANY(batch_id_array_arg) ORDER BY ta.seq ASC;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_check_patterns('{0, 3, 4, 5}');
--
CREATE FUNCTION regab_action_extract_check_patterns (IN  pattern_id_array_arg INTEGER[],
                                                     ---
                                                     OUT pattern_id   SMALLINT,
                                                     OUT pattern_name CHARACTER(6),
                                                     OUT ninstances   SMALLINT,
                                                     OUT nsquares     SMALLINT)
RETURNS SETOF record
AS $$
BEGIN
  RETURN QUERY SELECT ta.pattern_id, ta.pattern_name, ta.ninstances, ta.nsquares FROM regab_prng_patterns AS ta WHERE ta.pattern_id = ANY(pattern_id_array_arg) ORDER BY ta.pattern_id ASC;
END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_count_positions(22, '{1, 7, 8}', '{"CMR", "CMS"}');
--
CREATE FUNCTION regab_action_extract_count_positions (IN empty_count_arg    INTEGER,
                                                      IN batch_id_array_arg INTEGER[],
                                                      IN status_array_arg   CHARACTER(3)[],
                                                      ---
                                                      OUT batch_id          INTEGER,
                                                      OUT status            CHARACTER(3),
                                                      OUT game_position_cnt BIGINT,
                                                      OUT classified_cnt    BIGINT)
RETURNS SETOF record
AS $$
BEGIN

  RETURN QUERY
  SELECT
    gp.batch_id AS batch_id,
    gp.status AS status,
    count(gp.seq) AS game_position_cnt,
    count(pc.gp_id) AS classified_cnt
  FROM
    regab_prng_gp AS gp
  LEFT JOIN
    regab_prng_gp_pattern_class AS pc
  ON
    gp.seq = pc.gp_id
  WHERE
    gp.empty_count = empty_count_arg AND
    gp.status = ANY (status_array_arg) AND
    gp.batch_id = ANY (batch_id_array_arg)
  GROUP BY
    gp.batch_id, gp.status
  ORDER BY
    gp.batch_id, gp.status ASC;

END;
$$ LANGUAGE plpgsql VOLATILE;

--
-- Usage example:
-- tst_regab=> SELECT * FROM regab_action_extract_count_pattern_freqs(20, '{3}', '{"CMR", "CMS"}', 12);
--
CREATE FUNCTION regab_action_extract_count_pattern_freqs (IN empty_count_arg    INTEGER,
                                                          IN batch_id_array_arg INTEGER[],
                                                          IN status_array_arg   CHARACTER(3)[],
                                                          IN pattern_id_arg     INTEGER,
                                                          ---
                                                          OUT pattern_id              INTEGER,
                                                          OUT index_value             INTEGER,
                                                          OUT mirror_value            INTEGER,
                                                          OUT principal_index_value   INTEGER,
                                                          OUT cnt_0                   BIGINT,
                                                          OUT cnt_1                   BIGINT,
                                                          OUT cnt_2                   BIGINT,
                                                          OUT cnt_3                   BIGINT,
                                                          OUT cnt_4                   BIGINT,
                                                          OUT cnt_5                   BIGINT,
                                                          OUT cnt_6                   BIGINT,
                                                          OUT cnt_7                   BIGINT,
                                                          OUT total_cnt               BIGINT,
                                                          OUT relative_frequency      DOUBLE PRECISION,
                                                          OUT theoretical_probability DOUBLE PRECISION)
RETURNS SETOF record
AS $$
DECLARE
  ninstances      SMALLINT;
  nsquares        SMALLINT;
  index_value_cnt INTEGER;
BEGIN

  SELECT ta.ninstances, ta.nsquares INTO ninstances, nsquares FROM regab_prng_patterns AS ta WHERE ta.pattern_id = pattern_id_arg;
  index_value_cnt := 3^nsquares;

  RETURN QUERY
  WITH RECURSIVE index_value_range AS (
    SELECT
      0::INTEGER AS val
    UNION ALL SELECT val + 1::INTEGER AS val
    FROM index_value_range
    WHERE index_value_range.val < index_value_cnt - 1
  ),
  index_values AS (
    SELECT
      pc.i_diag3_0 AS iv_0,
      pc.i_diag3_1 AS iv_1,
      pc.i_diag3_2 AS iv_2,
      pc.i_diag3_3 AS iv_3
    FROM
      regab_prng_gp AS gp
    RIGHT JOIN
      regab_prng_gp_pattern_class AS pc
    ON
      gp.seq = pc.gp_id
    WHERE
      gp.empty_count = empty_count_arg AND
      gp.status = ANY (status_array_arg) AND
      gp.batch_id = ANY (batch_id_array_arg)
  ),
  index_freqs_tmp AS (
    SELECT
      ivr.val AS val,
      para.mirror_value AS mirror_value,
      para.principal_index_value AS principal_index_value,
      (SELECT count(1) FROM index_values WHERE iv_0 = ivr.val) AS cnt_0,
      (SELECT count(1) FROM index_values WHERE iv_1 = ivr.val) AS cnt_1,
      (SELECT count(1) FROM index_values WHERE iv_2 = ivr.val) AS cnt_2,
      (SELECT count(1) FROM index_values WHERE iv_3 = ivr.val) AS cnt_3,
      0::BIGINT AS cnt_4,
      0::BIGINT AS cnt_5,
      0::BIGINT AS cnt_6,
      0::BIGINT AS cnt_7
    FROM
      index_value_range AS ivr
    LEFT JOIN
      regab_prng_pattern_ranges AS para ON ivr.val = para.index_value
    WHERE
      para.pattern_id = pattern_id_arg
  ),
  index_freqs AS (
    SELECT
      *,
      ift.cnt_0 + ift.cnt_1 + ift.cnt_2 + ift.cnt_3 + ift.cnt_4 + ift.cnt_5 + ift.cnt_6 + ift.cnt_7 AS total_cnt
    FROM
      index_freqs_tmp AS ift
  ),
  aggregated_data AS (SELECT sum(ifr.total_cnt) AS position_cnt FROM index_freqs AS ifr)
  ---
  SELECT
    pattern_id_arg AS pattern_id,
    ifr.val AS index_value,
    ifr.mirror_value AS mirror_value,
    ifr.principal_index_value AS principal_index_value,
    ifr.cnt_0, ifr.cnt_1, ifr.cnt_2, ifr.cnt_3, ifr.cnt_4, ifr.cnt_5, ifr.cnt_6, ifr.cnt_7, ifr.total_cnt,
    ifr.total_cnt::DOUBLE PRECISION / (SELECT agd.position_cnt FROM aggregated_data AS agd) AS relative_frequency,
    (SELECT papro.index_prob_given_ec FROM regab_prng_pattern_probs AS papro
      WHERE papro.range_id = (SELECT seq FROM regab_prng_pattern_ranges AS para
                                WHERE para.pattern_id = pattern_id_arg AND para.index_value = ifr.val)
      AND papro.empty_count = empty_count_arg)
      AS probability
  FROM index_freqs AS ifr ORDER BY ifr.val ASC;

END;
$$ LANGUAGE plpgsql VOLATILE;

--
--
--
CREATE FUNCTION regab_action_extract_create_tmp_tables ()
RETURNS VOID
AS $$
BEGIN

  --
  -- TODO:
  -- The table shall be reduced to: global_variable_index, patter_id, principal_index_value, toatal_cnt.
  --
  -- In function regab_action_extract_count_pattern_freqs first:
  -- GROUP BY pattern_id, principal_index_value and SUM total_cnt ...
  -- then INSERT into this table before returning the results ...
  --
  -- Finally SORT and assign the global_variable_index.
  -- UPDATE ....
  -- ,rank() over (order by pattern_id, principal_index_value asc) as global_variable_index
  --
  -- The INDEX regab_action_extract_tmp_freqs_pid_piv is not needed.
  --
  CREATE TEMPORARY TABLE regab_action_extract_tmp_freqs (pattern_id              INTEGER,
                                                         index_value             INTEGER,
                                                         mirror_value            INTEGER,
                                                         principal_index_value   INTEGER,
                                                         cnt_0                   BIGINT,
                                                         cnt_1                   BIGINT,
                                                         cnt_2                   BIGINT,
                                                         cnt_3                   BIGINT,
                                                         cnt_4                   BIGINT,
                                                         cnt_5                   BIGINT,
                                                         cnt_6                   BIGINT,
                                                         cnt_7                   BIGINT,
                                                         total_cnt               BIGINT,
                                                         relative_frequency      DOUBLE PRECISION,
                                                         theoretical_probability DOUBLE PRECISION,
                                                         ---
                                                         CONSTRAINT rae_tmp_freqs_pk PRIMARY KEY (pattern_id, index_value)
                                                         );

  --
  -- This index is needed .... to be verified .... .
  --
  CREATE INDEX regab_action_extract_tmp_freqs_pid_piv ON regab_action_extract_tmp_freqs (pattern_id, principal_index_value);

END;
$$ LANGUAGE plpgsql VOLATILE;

COMMIT;

/*
SELECT EXISTS (
   SELECT 1
   FROM   information_schema.tables 
   WHERE  table_schema = 'reversi'
   AND    table_name = 'regab_prng_gp_h'
   );
*/
