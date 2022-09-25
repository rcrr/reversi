#
# regab.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022 Roberto Corradini. All rights reserved.
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
#

import reversi
import reversi.board
import reversi.pattern

from reversi.board import *
from reversi.pattern import *

import numpy as np
import pandas as pd

import psycopg2

# Connect to the REGAB tests database
conn = psycopg2.connect("dbname=tst_regab user=tst_regab host=localhost port=5432")

# Open a cursor to perform database operations
cur = conn.cursor()

cur.execute("SET search_path TO reversi;")
cur.execute("SELECT * FROM regab_prng_gp_h;")
res = cur.fetchall()

# Clese the cursor
cur.close()

# Close communication with the database
conn.close()

def compute_indexes_on_df(df : pd.DataFrame, pl : list) -> pd.DataFrame:
    if not isinstance(df, pd.DataFrame):
        raise TypeError('Argument df is not an instance of DataFrame')
    if not isinstance(pl, list):
        raise TypeError('Argument pl is not an instance of list')
    if not all([isinstance(e, Pattern) for e in pl]):
        raise TypeError('Argument pl must have all elements belonging to Pattern type')
    if not pl:
        return None

    def compute_indexes(row):
        m = row['MOVER']
        o = row['OPPONENT']
        b = Board(SquareSet.new_from_signed_int(m), SquareSet.new_from_signed_int(o))
        ret = np.concatenate(b.compute_patternlist_principal_indexes(pl))
        return ret

    def build_col_names(pl, label):
        names = []
        idx_start = 0
        for p in pl:
            idx_end = idx_start + p.n_instances
            x = ['{:2s}_{:03d}'.format(label, x) for x in range(idx_start, idx_end)]
            names = names + x
            idx_start = idx_end
        return names

    indexes = pd.DataFrame(list(df.apply(compute_indexes, axis=1))).astype(np.uint16)
    indexes.columns = build_col_names(pl, 'I0') + build_col_names(pl, 'I1')
    return indexes
