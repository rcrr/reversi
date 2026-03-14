#
# rdata.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2026 Roberto Corradini. All rights reserved.
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

from twolm.domain import *

import psycopg2 as pg
import pandas as pd

from typing import List, Self, Callable, Any, Union, TypeVar


__all__ = ['RegabDBConnection', 'regab_gp_as_df', 'regab_gp_extract', 'RegabDataSet']


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

    def close(self):
        """
        Closes the db connection.
        """
        if self.conn:
            self.conn.close()


def regab_gp_as_df(rc: RegabDBConnection,
                   bid: Union[int, List[int]],
                   status: Union[str, List[str]],
                   ec: int,
                   limit: Union[int, None] = None,
                   where: Union[str, None] = None,
                   fields: Union[List[str], None] = None) -> pd.DataFrame:
    """
    Retrieves game positions from the database and returns them as a pandas DataFrame.

    Parameters
    ----------
    rc : RegabDBConnection
        An instance of the RegabDBConnection class representing the database connection.

    bid : List[int] or int
        A list of batch IDs or a single batch ID to filter the game positions.
        Each batch ID must be a non-negative integer.

    status : List[str] or str
        A list of status codes or a single status code to filter the game positions.
        Each status code must be a string of exactly three characters.

    ec : int
        The empty count to filter the game positions.
        Must be an integer in the range [0, 60].

    limit : int, optional
        The maximum number of rows to return.
        If specified, must be a non-negative integer.

    where : str, optional
        An additional SQL WHERE clause to further filter the game positions.
        If specified, must be a string.

    fields : List[str], optional
        A list of column names to include in the result.
        If specified, must be a list of strings representing valid column names in the `regab_prng_gp` table.
        If not specified, defaults to a base set of columns: ['seq', 'batch_id', 'status', 'mover', 'opponent', 'player', 'empty_count', 'game_value'].
        If '*', all columns are included.

    Returns
    -------
    pd.DataFrame
        A pandas DataFrame containing the filtered game positions.

    Raises
    ------
    TypeError
        If any of the arguments have an incorrect type.

    ValueError
        If any of the arguments have an invalid value.

    Exception
        If the SQL query execution fails due to an incorrect WHERE clause or other database-related issues.

    Notes
    -----
    - If the `fields` parameter is specified, it overrides the default columns.
    - If the `fields` parameter is not specified, the default columns are used.
    - If the `where` parameter is specified, it is added to the SQL query as an additional filter.
    - If the `limit` parameter is specified, it limits the number of rows returned by the query.
    """
    def _check_arg_bid():
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

    def _check_arg_status():
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

    def _check_arg_fields():
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
    if not (ec >= 0 and ec <= 60):
        raise ValueError('Argument ec must be in range [0..60]')
    lbid = _check_arg_bid()
    lbid_str = ', '.join([str(x) for x in lbid])
    lstatus = _check_arg_status()
    lstatus_str = ', '.join(["'{}'".format(x) for x in lstatus])
    if limit:
        if not isinstance(limit, int):
            raise TypeError('Argument limit is not an instance of int')
        if not limit >= 0:
            raise ValueError('Argument limit must be equal or greather than zero')
    if where:
        if not isinstance(where, str):
            raise TypeError('Argument where is not an instance of str')
    selection = ', '.join(_check_arg_fields())
    q = "SELECT {:s} FROM regab_prng_gp WHERE batch_id IN ({:s}) AND empty_count = {:d} AND status IN ({:s})".format(selection, lbid_str, ec, lstatus_str)
    if where:
        q = q + ' AND {:s}'.format(where)
    q = q + ' ORDER BY seq'
    if limit != None:
        q = q + ' LIMIT {:d}'.format(limit)
    q = q + ';'

    df = None
    with rc.conn:
        with rc.conn.cursor() as curs:
            curs.execute(q)
            colnames = [d[0] for d in curs.description]
            df = pd.DataFrame(curs.fetchall(), columns=colnames)

    return df


def regab_gp_extract(rc: RegabDBConnection,
                     bid: Union[int, List[int]],
                     status: Union[str, List[str]],
                     ec: int) -> pd.DataFrame:

    min_fields = ['mover', 'opponent', 'game_value']
    df = regab_gp_as_df(rc, bid, status, ec, limit=None, where=None, fields=min_fields)
    return df


class RegabDataSet:

    def __init__(self,
                 bid: List[int],
                 status: List[str],
                 ec: int,
                 positions: pd.DataFrame):
        
        if isinstance(bid, list):
            if not all([isinstance(x, int) for x in bid]):
                raise TypeError('Argument bid has elements not being of type int')
        else:
            raise TypeError('Argument bid must be a list of ints')
        if not all([x >= 0 for x in bid]):
            raise ValueError('Argument bid must be equal or greather than zero')

        if isinstance(status, list):
            if not all([isinstance(x, str) for x in status]):
                raise TypeError('Argument status has elements not being of type str')
        else:
            raise TypeError('Argument status must be a list of strs')
        if not all([len(x) == 3 for x in status]):
            raise ValueError('Argument status must have elements of lenght equal to 3')
        
        if not isinstance(ec, int):
            raise TypeError('Argument ec is not an instance of int')
        if not (ec >= 0 and ec <= 60):
            raise ValueError('Argument ec must be in range [0..60]')

        if not isinstance(positions, pd.DataFrame):
            raise TypeError('Argument positions is not an instance of DataFrame')
        if not len(positions.columns) == 3:
            raise ValueError('Argument positions has not 3 columns.')
        positions_expected_dtypes = ['int64', 'int64', 'int8']
        for i, expected_type in enumerate(positions_expected_dtypes):
            actual_type = positions.dtypes.iloc[i]
            if actual_type != expected_type:
                raise TypeError(f"Column '{i}' must be {expected_type}, but instead it is {actual_type}")

        self.bid = bid
        self.status = status
        self.ec = ec
        self.positions = positions
        self.len = len(positions)
