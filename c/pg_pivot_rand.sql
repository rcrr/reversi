
-- PostgreSQL script part of the reversi program.
-- This script ...

-- Start psql by running: psql -U reversi -w -d reversi -h localhost
-- Load the file by running the command: \i pg_pivot_rand.sql
-- Or run by a shell prompt: time psql -U reversi -w -d reversi -h localhost -f pg_load_rand.sql

DROP TYPE IF EXISTS mobility_empty_frequency CASCADE;
CREATE TYPE mobility_empty_frequency AS (empty_square_count int, mobility int, frequency int);
                                                 
CREATE OR REPLACE FUNCTION pivot_rand_log() RETURNS SETOF mobility_empty_frequency AS $$
DECLARE
  r                mobility_empty_frequency;
  empties          integer ARRAY;
  lmcs             integer ARRAY;
  e                integer;
  lmc              integer;
  frequency        integer;
BEGIN

  empties := array(SELECT DISTINCT r.empty_count AS empty_square_count
                   FROM rand_log AS r
                   ORDER BY empty_square_count DESC);

  lmcs := array(SELECT DISTINCT r.legal_move_count_adjusted AS mobility
                FROM rand_log AS r
                ORDER BY mobility DESC);

  FOREACH lmc IN ARRAY lmcs
  LOOP
    FOREACH e IN ARRAY empties
    LOOP
    
      SELECT
        count(*) AS frequency
      INTO
        frequency
      FROM
        rand_log AS r
      WHERE
        r.empty_count = e AND r.legal_move_count_adjusted = lmc
      GROUP BY
        r.legal_move_count_adjusted;
    
      r := (e, lmc, frequency);
      RETURN NEXT r;
    END LOOP;
  END LOOP;
  RETURN;
END;
$$ LANGUAGE plpgsql;

SELECT * FROM pivot_rand_log();
