#!/bin/bash
#
# pv_load_file.sh
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
#
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
# @copyright 2015 Roberto Corradini. All rights reserved.
#
# License
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
# or visit the site <http://www.gnu.org/licenses/>.

if [ "$#" -ne 3 ]; then
  echo "PostgreSQL user, dabase, and filename arguments are required."
  echo "Usage: $0 PSQL_USER PSQL_DATABASE PVE_DUMP_FILE"
  exit 1
fi

PSQL_USER=$1

PSQL_DB=$2

FILE_NAME=$3
if [ ! -f $FILE_NAME ]; then
  echo "File $FILE_NAME does not exist."
fi

TABLE_NAME=principal_variation_staging
TABLE_NAME_AND_COLUMNS="$TABLE_NAME (line_id, move_id, variant_id, next_id, game_move, head_level, rel_level, gp_hash, gp_b, gp_w, gp_p)"
READ_PVE_DUMP="../build/bin/read_pve_dump"

psql -U $PSQL_USER -w -d $PSQL_DB -h localhost <<EOF

\set ON_ERROR_STOP on

SET SCHEMA 'reversi';

SELECT COUNT(*) FROM $TABLE_NAME;

TRUNCATE TABLE $TABLE_NAME;

\COPY $TABLE_NAME_AND_COLUMNS FROM PROGRAM '$READ_PVE_DUMP -f $FILE_NAME -t' WITH (FORMAT CSV, DELIMITER ';', HEADER true);

SELECT COUNT(*) FROM $TABLE_NAME;

EOF

exit 0
