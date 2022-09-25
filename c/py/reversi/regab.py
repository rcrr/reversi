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
import reversi.cfg

from reversi.board import *
from reversi.pattern import *
from reversi.cfg import *

import numpy as np
import pandas as pd

import psycopg2 as pg

def compute_indexes_on_df(df : pd.DataFrame, pl : list) -> pd.DataFrame:
    """
    Returns a new data frame having the indexes and principal ones compute on the game
    position defined in the df argument.

    The df argument must have two colums: MOVER and OPPONENT having dtype equal to int64.
    The pl argument must be a list of Pattern objects.

    The returned data frame has columns named I0_??? for indexes and I1_??? for principals.
    Indexes are named as a progression I0_000, I0_001, I0_002, ... that is ordered as patterns
    are ordered in pl argument. The count of indexes computed is the sum over pl of the attribute
    n_instances.
    So for instance if pl is defined as [PEdge(), PDiag8()] the computed pattern are 6:
      I0_000, I0_001, I0_002, I0_003 for EDGE, and I0_004, I0_005 for DIAG8
    The same applies to the principal indexes.
    """
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

class RegabDBConnection():
    """
    A regab database connection wraps the psycopg2 connection object.
    The object could be created directly with the constructor or better using the 
    new_from_config class method.
    """
    def __init__(self, dbname: str, user: str, host: str, port='5432', password=None):
        if not isinstance(dbname, str):
            raise TypeError('Argument dbname is not an instance of str')
        if not isinstance(user, str):
            raise TypeError('Argument user is not an instance of str')
        if not isinstance(host, str):
            raise TypeError('Argument host is not an instance of str')
        if not isinstance(port, str):
            raise TypeError('Argument port is not an instance of str')
        self.dbname = dbname
        self.user = user
        self.host = host
        self.port = port
        self.password = password
        cs = 'dbname={:s} user={:s} host={:s} port={:s}'.format(self.dbname,
                                                                self.user,
                                                                self.host,
                                                                self.port)
        if self.password:
            cs = cs + ' password={:s}'.format(self.password)
        self._connect_string = cs
        self.conn = pg.connect(self._connect_string)
        with self.conn.cursor() as curs:
            curs.execute("SET search_path TO reversi;")

    @classmethod
    def new_from_config(cls, filepath: str, section: str):
        """
        Returns a new RegabDBConnection by giving a config file and a section.
        The config/section must have defined the following properties:
          dbname
          user
          host
        In case the poperties port and password are defined are used to establish the connection.
        """
        if not isinstance(section, str):
            raise TypeError('Argument section is not an instance of str')
        config = Cfg(filepath)
        dbname = config.get(section, 'dbname')
        user = config.get(section, 'user')
        host = config.get(section, 'host')
        port = config.get(section, 'port')
        password = config.get(section, 'password')
        config.free()
        if not dbname:
            raise ValueError('Config file doesn\'t have the dbname entry')
        if not user:
            raise ValueError('Config file doesn\'t have the user entry')
        if not host:
            raise ValueError('Config file doesn\'t have the host entry')
        if not port and not password:
            rc = RegabDBConnection(dbname, user, host)
        elif not password:
            rc = RegabDBConnection(dbname, user, host, port=port)
        elif not port:
            rc = RegabDBConnection(dbname, user, host, password=password)
        else:
            rc = RegabDBConnection(dbname, user, host, port=port, password=password)
        return rc

    def close(self):
        """
        Closes the db connection.
        """
        if self.conn:
            self.conn.close()

def regab_batches_as_df(rc: RegabDBConnection) -> pd.DataFrame:
    """
    Returns a dataframe having the regab batches (table regab_prng_gp_h)

    Example:
      rc = RegabDBConnection.new_from_config('cfg/regab.cfg', 'test')
      df = regab_batches_as_df(rc)
      df
          seq                   ins_time status  prng_seed   ngames  npositions
      0     1 2017-12-17 18:19:16.440571    CMP      97531        1          61
      1     3 2017-12-17 18:42:13.782953    CMP      13579  1000000    61412190
      2     4 2018-01-03 16:25:52.050240    CMP          0     1000       61356
      3     5 2018-01-03 16:27:08.188697    CMP       5577    10000      614283
      4     6 2018-01-03 16:30:24.238143    CMP        881   100000     6142003
      5     7 2018-01-03 16:38:51.097299    CMP        277       10         614
      6     8 2018-01-03 16:39:24.466546    CMP        607      100        6140
      7     9 2020-12-20 18:02:16.472520    CMP      77357  2000000   122829157
      8    10 2021-07-24 17:58:18.845845    CMP      25487   200000    12281722
      9    11 2021-07-25 20:51:04.829293    CMP      40132   400000    24566078
      10   12 2021-08-21 17:55:04.407724    CMP      33057  4000000   245656852
    """
    if not isinstance(rc, RegabDBConnection):
        raise TypeError('Argument rc is not an instance of RegabDBConnection')
    if not rc.conn.closed == 0:
        raise TypeError('The database connection is closed')
    with rc.conn.cursor() as curs:
        curs.execute("SELECT * FROM regab_prng_gp_h;")
        colnames = [d[0] for d in curs.description]
        df = pd.DataFrame(curs.fetchall(), columns=colnames)
    return df
