#
# regab.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022, 2023 Roberto Corradini. All rights reserved.
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
import duckdb

def compute_feature_values_on_df(df : pd.DataFrame, fl : list, mover='MOVER', opponent='OPPONENT') -> pd.DataFrame:
    """
    Returns a new data frame having the values of the features listed in fl computed on the game
    position defined in the df argument.

    The df argument must have two colums: MOVER and OPPONENT having dtype equal to int64.
    The fl argument must be a list of Feature objects.

    The returned data frame has columns named F_???.
    Features are named as a progression F_000, F_001, F_002, ... that is ordered as features
    are ordered in fl argument.
    """
    if not isinstance(df, pd.DataFrame):
        raise TypeError('Argument df is not an instance of DataFrame')
    if not isinstance(fl, list):
        raise TypeError('Argument fl is not an instance of list')
    if not all([isinstance(e, Feature) for e in fl]):
        raise TypeError('Argument fl must have all elements belonging to Feature type')
    if not isinstance(mover, str):
        raise TypeError('Argument mover is not an instance of str')
    if not isinstance(opponent, str):
        raise TypeError('Argument opponent is not an instance of str')
    if not mover in df.columns:
        raise ValueError('Argument df doesn\'t have a mover column')
    if not opponent in df.columns:
        raise ValueError('Argument df doesn\'t have an opponent column')
    if df[mover].dtype != 'int64':
        raise TypeError('Column mover has not the correct int64 type')
    if df[opponent].dtype != 'int64':
        raise TypeError('Column opponent has not the correct int64 type')
    if not fl:
        return None

    def compute_values(row):
        m = np.int64(row[mover])
        o = np.int64(row[opponent])
        b = Board(SquareSet.new_from_signed_int(m), SquareSet.new_from_signed_int(o))
        return b.compute_featurelist_values(fl)

    def build_col_names(fl, label):
        names = []
        idx_start = 0
        d = {}
        for f in fl:
            idx_end = idx_start + f.field_cnt
            x = ['{:1s}_{:03d}'.format(label, x) for x in range(idx_start, idx_end)]
            names = names + x
            d.update({f: x})
            idx_start = idx_end
        return names, d

    # A new dataframe with just the two int64 columns is required to avoid corruption of the data.
    df2 = df[[mover, opponent]].copy(deep=False)
    
    feature_values = pd.DataFrame(list(df2.apply(compute_values, axis=1))).astype(np.double)
    feature_values.columns, flabel_dict = build_col_names(fl, 'F')
    
    return feature_values, flabel_dict

def compute_indexes_on_df(df : pd.DataFrame, pl : list, mover='MOVER', opponent='OPPONENT') -> (pd.DataFrame, dict, dict):
    """
    Returns a tuple containing:
     - a new data frame having the indexes and principal ones computed on the game
       position defined in the df argument.
     - a dictionary having as key the pattern and as values the list of corresponding column names holding the index values
     - a second dictionary holding the principal index values

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
    if not isinstance(mover, str):
        raise TypeError('Argument mover is not an instance of str')
    if not isinstance(opponent, str):
        raise TypeError('Argument opponent is not an instance of str')
    if not mover in df.columns:
        raise ValueError('Argument df doesn\'t have a mover column')
    if not opponent in df.columns:
        raise ValueError('Argument df doesn\'t have an opponent column')
    if df[mover].dtype != 'int64':
        raise TypeError('Column mover has not the correct int64 type')
    if df[opponent].dtype != 'int64':
        raise TypeError('Column opponent has not the correct int64 type')
    if not pl:
        return None

    def compute_indexes(row):
        m = np.int64(row[mover])
        o = np.int64(row[opponent])
        b = Board(SquareSet.new_from_signed_int(m), SquareSet.new_from_signed_int(o))
        ret = np.concatenate(b.compute_patternlist_principal_indexes(pl))
        return ret

    def build_col_names(pl, label):
        names = []
        idx_start = 0
        d = {}
        for p in pl:
            idx_end = idx_start + p.n_instances
            x = ['{:2s}_{:03d}'.format(label, x) for x in range(idx_start, idx_end)]
            names = names + x
            d.update({p: x})
            idx_start = idx_end
        return names, d

    # A new dataframe with just the two int64 columns is required to avoid corruption of the data.
    df2 = df[[mover, opponent]].copy(deep=False)

    indexes = pd.DataFrame(list(df2.apply(compute_indexes, axis=1))).astype(np.int32)
    cnames_i0, plabel_dict_i0 = build_col_names(pl, 'I0')
    cnames_i1, plabel_dict_i1 = build_col_names(pl, 'I1')
    indexes.columns = cnames_i0 + cnames_i1
    return indexes, plabel_dict_i0, plabel_dict_i1

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
        if dbname != '':
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

    @classmethod
    def new_duckdb(cls, conn: duckdb.DuckDBPyConnection) -> 'RegabDBConnection':
        """
        Returns a new RegabDBConnection for testing based on duckdb.
        """
        if not isinstance(conn, duckdb.DuckDBPyConnection):
            raise TypeError('Argument conn is not an instance of duckdb.DuckDBPyConnection')
        dbname = ''
        user = ''
        host = ''
        rc = RegabDBConnection(dbname, user, host)
        rc.conn = conn
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
    with rc.conn.cursor() as curs:
        curs.execute("SELECT * FROM regab_prng_gp_h;")
        colnames = [d[0] for d in curs.description]
        df = pd.DataFrame(curs.fetchall(), columns=colnames)
    return df

def regab_gp_as_df(rc: RegabDBConnection, bid, status, ec: int, limit=None, where=None, fields=None) -> pd.DataFrame:
    """
    """
    def check_arg_status():
        if isinstance(status, list):
            if all([isinstance(x, str) for x in status]):
                ret = status
            else:
                raise TypeError('Argument status has elements not being of type str')
        elif isinstance(status, str):
            ret = [status]
        else:
            raise TypeError('Argument status must be str or a list of strs')
        if not all([len(x) == 3 for x in ret]):
            raise ValueError('Argument status must have elements of lenght equal to 3')
        return ret
    
    def check_arg_bid():
        ret = None
        if isinstance(bid, list):
            if all([isinstance(x, int) for x in bid]):
                ret = bid
            else:
                raise TypeError('Argument bid has elements not being of type int')
        elif isinstance(bid, int):
            ret = [bid]
        else:
            raise TypeError('Argument bid must be int or a list of ints')
        if not all([x >= 0 for x in ret]):
            raise ValueError('Argument bid must be equal or greather than zero')
        return ret

    def check_arg_fields():
        with rc.conn.cursor() as curs:
            curs.execute("SELECT * FROM regab_prng_gp LIMIT 0;")
            colnames = [d[0] for d in curs.description]
        base_colnames = ['seq', 'batch_id', 'status', 'mover', 'opponent', 'player', 'empty_count', 'game_value']

        if not fields:
            return base_colnames
        else:
            if isinstance(fields, str):
                if fields == '*':
                    return colnames
                else:
                    raise ValueError('Argument fields has an invalid value')
            elif isinstance(fields, list):
                if not all([isinstance(x, str) for x in fields]):
                    raise TypeError('Argument fields is a list thta must have all elements belonging to the string type')
                if len(fields) != len(list(dict.fromkeys(fields))):
                    raise ValueError('Argument fields has duplicate entries')
                if not all([x in colnames for x in fields]):
                    raise ValueError('Argument fields has entries not being a column of the game positions db table')
                return fields
            else:
                raise TypeError('Argument field must be string or a list of stings')

    if not isinstance(rc, RegabDBConnection):
        raise TypeError('Argument rc is not an instance of RegabDBConnection')
    if not isinstance(ec, int):
        raise TypeError('Argument ec is not an instance of int')
    if not ec >= 0 and ec <= 60:
        raise ValueError('Argument ec must be in range [0..60]')
    lbid = check_arg_bid()
    lbid_str = ', '.join([str(x) for x in lbid])
    lstatus = check_arg_status()
    lstatus_str = ', '.join(["'{}'".format(x) for x in lstatus])
    if limit:
        if not isinstance(limit, int):
            raise TypeError('Argument limit is not an instance of int')
        if not limit >= 0:
            raise ValueError('Argument limit must be equal or greather than zero')
    if where:
        if not isinstance(where, str):
            raise TypeError('Argument where is not an instance of str')
    selection = ', '.join(check_arg_fields())
    q = "SELECT {:s} FROM regab_prng_gp WHERE batch_id IN ({:s}) AND empty_count = {:d} AND status IN ({:s})".format(selection, lbid_str, ec, lstatus_str)
    if where:
        q = q + ' AND {:s}'.format(where)
    q = q + ' ORDER BY seq'
    if limit != None:
        q = q + ' LIMIT {:d}'.format(limit)
    q = q + ';'
    with rc.conn.cursor() as curs:
        curs.execute(q)
        colnames = [d[0] for d in curs.description]
        df = pd.DataFrame(curs.fetchall(), columns=colnames)
    return df

def regab_empty_count(ec: int) -> int:
    if not isinstance(ec, int):
        raise TypeError('Argument ec is not an instance of int')
    if not ec >= 0 and ec <= 60:
        raise ValueError('Argument ec must be in range [0..60]')
    return ec

def regab_batches(bs) -> list:
    if isinstance(bs, list):
        if not all([isinstance(x, int) for x in bs]):
            raise ValueError('Argument bs must have elements of type int')
        batches = bs
    elif isinstance(bs, int):
        batches = [bs]
    else:
        raise TypeError('Argument bs is not an instance of list or int')
    if not all([x >= 0 for x in batches]):
        raise ValueError('All batches must be int values equal or greather than zero')
    return batches

def regab_statuses(sts) -> list:
    if isinstance(sts, list):
        if not all([isinstance(x, str) for x in sts]):
            raise ValueError('Argument sts must have elements of type str')
        statuses = sts
    elif isinstance(sts, str):
        statuses = sts.split(',')
    else:
        raise TypeError('Argument sts is not an instance of list or str')
    if not all([len(x) == 3 for x in statuses]):
        raise ValueError('All statuses must be three characters strings')
    if not all([all([c.isalnum() for c in status]) for status in statuses]):
        raise ValueError('All statuses must be three characters strings')
    return statuses

def regab_features(fs) -> list:
    fset = set()
    if isinstance(fs, list):
        if all([isinstance(f, str) for f in fs]):
            for name in fs:
                feature = features_as_dict.get(name)
                if not feature:
                    raise ValueError('Name \"{:s}\" is not found as a defined feature'.format(name))
                fset.add(feature)
        elif all([isinstance(f, Feature) for f in fs]):
            fset.update(fs)
        else:
            raise TypeError('Elements in fs list must be all strings or all features')
    elif isinstance(fs, str):
        feature_names = fs.split(',')
        for name in feature_names:
            feature = features_as_dict.get(name)
            if not feature:
                raise ValueError('Name \"{:s}\" is not found as a defined feature'.format(name))
            fset.add(feature)
    elif isinstance(fs, Feature):
        fset.add(feature)
    else:
        raise TypeError('Argument fs is not an instance of Feature, list or str')
    features = sorted(list(fset), key=lambda f: f.id)
    return features

def regab_patterns(ps) -> list:
    pset = set()
    if isinstance(ps, list):
        if all([isinstance(p, str) for p in ps]):
            for name in ps:
                pattern = patterns_as_dict.get(name)
                if not pattern:
                    raise ValueError('Name \"{:s}\" is not found as a defined pattern'.format(name))
                pset.add(pattern)
        elif all([isinstance(p, Pattern) for p in ps]):
            pset.update(ps)
        else:
            raise TypeError('Elements in ps list must be all strings or all patterns')
    elif isinstance(ps, str):
        pattern_names = ps.split(',')
        for name in pattern_names:
            pattern = patterns_as_dict.get(name)
            if not pattern:
                raise ValueError('Name \"{:s}\" is not found as a defined pattern'.format(name))
            pset.add(pattern)
    elif isinstance(ps, Pattern):
        pset.add(pattern)
    else:
        raise TypeError('Argument ps is not an instance of Pattern, list or str')
    patterns = sorted(list(pset), key=lambda p: p.id)
    return patterns

def regab_patternlist_probs_as_df(rc : RegabDBConnection, patterns, ec : int, is_principal=False) -> pd.DataFrame:
    if not isinstance(rc, RegabDBConnection):
        raise TypeError('Argument rc is not an instance of RegabDBConnection')
    if isinstance(patterns, Pattern):
        patterns = [patterns]
    elif isinstance(patterns, list):
        if not all([isinstance(e, Pattern) for e in patterns]):
            raise TypeError('Argument patterns must have all elements belonging to Pattern type')
    else:
        raise TypeError('Argument patterns must be an instance of Pattern or a list of Pattern elements')
    if not isinstance(ec, int):
        raise TypeError('Argument ec is not an instance of int')
    if not ec >= 0 and ec <= 60:
        raise ValueError('Argument ec must be in range [0..60]')
    if not isinstance(is_principal, bool):
        raise TypeError('Argument is_principal is not an instance of bool')

    pids_as_str = ', '.join([str(x) for x in [p.id for p in patterns]])
    
    if is_principal:
        colnames = ['pattern_id', 'principal', 'probs']
        q = \
            """
            SELECT a.pattern_id AS pid, b.principal_index_value AS idx, sum(c.index_prob_given_ec) AS probs
            FROM regab_prng_patterns AS a
            LEFT JOIN regab_prng_pattern_ranges AS b ON a.pattern_id = b.pattern_id
            LEFT JOIN regab_prng_pattern_probs AS c ON b.seq = c.range_id
            WHERE a.pattern_id IN ({}) AND c.empty_count = {:d}
            GROUP BY a.pattern_id, b.principal_index_value
            ORDER BY a.pattern_id, b.principal_index_value;
            """.format(pids_as_str, ec)
    else:
        colnames = ['pattern_id', 'index', 'probs']
        q = \
            """
            SELECT a.pattern_id AS pid, b.index_value AS idx, c.index_prob_given_ec AS probs FROM regab_prng_patterns AS a
            LEFT JOIN regab_prng_pattern_ranges AS b ON a.pattern_id = b.pattern_id
            LEFT JOIN regab_prng_pattern_probs AS c ON b.seq = c.range_id
            WHERE a.pattern_id IN ({}) AND c.empty_count = {:d}
            ORDER BY a.pattern_id, b.index_value;
            """.format(pids_as_str, ec)

    with rc.conn.cursor() as curs:
        curs.execute(q)
        df = pd.DataFrame(curs.fetchall(), columns=colnames)
    return df
