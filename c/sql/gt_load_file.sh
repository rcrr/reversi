#!/bin/bash
#
# gt_load_file.sh
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
#
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
# @copyright 2014 Roberto Corradini. All rights reserved.
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

if [ "$#" -ne 1 ]; then
  echo "Filename argument is required."
  echo "Usage: $0 FILE_TO_BE_LOADED"
  exit 1
fi

FILE_NAME=$1
if [ ! -f $FILE_NAME ]; then
  echo "File $FILE_NAME does not exist."
fi

TABLE_NAME=game_tree_log_staging

psql -U reversi -w -d reversi -h localhost <<EOF

\set ON_ERROR_STOP on

SELECT COUNT(*) FROM $TABLE_NAME;

TRUNCATE TABLE $TABLE_NAME;

\COPY $TABLE_NAME FROM $FILE_NAME WITH (FORMAT CSV, DELIMITER ';', HEADER true);

SELECT COUNT(*) FROM $TABLE_NAME;

EOF

exit 0
