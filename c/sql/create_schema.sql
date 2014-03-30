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

CREATE TABLE axis_info (id      axis,
                        ordinal SMALLINT,
                        PRIMARY KEY(id));

INSERT INTO axis_info (id, ordinal) VALUES
  ('HO', 0),
  ('VE', 1),
  ('DD', 2),
  ('DU', 3);



-- DROP TABLE IF EXISTS rab_solver_log;
CREATE TABLE rab_solver_log (run_id         INTEGER,
                             call_id        INTEGER,
                             hash           BIGINT,
                             parent_hash    BIGINT,
                             blacks         BIGINT,
                             whites         BIGINT,
                             player         SMALLINT,
                             PRIMARY KEY(run_id, call_id));