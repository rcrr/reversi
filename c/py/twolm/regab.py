#
# regab.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
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

from __future__ import annotations

from twolm.board import *
from twolm import binio

import psycopg2 as pg
import pandas as pd
import numpy as np
import numpy.typing as npt

from pathlib import Path

import hashlib
import struct

from typing import TypeAlias, Callable, Union, List, Annotated, Tuple, Type, Self

from pydantic import validate_call, ConfigDict, BeforeValidator, AfterValidator, Field
from contextvars import ContextVar
from functools import wraps



__all__ = ['GameValue', 'GameValueArray',
           'RegabDBConnection',
           'regab_gp_as_df',
           'RegabDataSet',
           'regab_extract_data_set_from_db',
           'regab_load_data_set_from_file',
           'regab_store_data_set_to_file']



#: Represents a reversi game value from -64 to +64 as an numpy.int8 scalar.
GameValue: TypeAlias = np.int8

def _validate_game_value_array(v: Any) -> Any:
    """Validator to ensure a numpy array strictly uses int8 dtype."""
    if isinstance(v, np.ndarray) and v.dtype != np.int8:
        raise ValueError(f"Array dtype must be int8, got {v.dtype}")
    return v

# A Pydantic-compatible type hint for GameValue (np.int8) arrays.
GameValueArray = Annotated[npt.NDArray[np.int8], BeforeValidator(_validate_game_value_array)]

MAGIC_NUMBER = b"RLMRDS00"

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

class RegabDBConnection:
    """
    A regab database connection wraps the psycopg2 connection object.

    Attributes
    ----------
    dbname : str
        The name of the database.
    user : str
        The username for the database connection.
    host : str
        The hostname of the database server.
    port : str
        The port number on which the database server is listening.
    password : str
        The password for the database connection.
    _connect_string : str
        The connection string used to connect to the database.
    conn : psycopg2.extensions.connection
        The psycopg2 connection object.

    Methods
    -------
    close()
        Closes the db connection.

    activate_conn()
        Establish the connection.
    """
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self, dbname: str, user: str, host: str,
                 port: str ='5432', password: str | None =None,
                 activate_conn: bool =True):
        """
        Initializes a RegabDBConnection instance.

        Parameters
        ----------
        dbname : str
            The name of the database.
        user : str
            The username for the database connection.
        host : str
            The hostname of the database server.
        port : str, optional
            The port number on which the database server is listening. Default is '5432'.
        password : str, optional
            The password for the database connection. Default is None.
        activate_conn : bool, optional
            When False do not create the database connection.

        Raises
        ------
        TypeError
            If any of the arguments have an incorrect type.
        """
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
            if activate_conn:
                self.conn = pg.connect(self._connect_string)
                with self.conn.cursor() as curs:
                    curs.execute("SET search_path TO reversi;")
            else:
                self.conn = None

    def activate_conn(self):
        if self.conn is None:
            self.conn = pg.connect(self._connect_string)
            with self.conn.cursor() as curs:
                curs.execute("SET search_path TO reversi;")
        
    def close(self):
        """
        Closes the db connection.
        """
        if self.conn:
            self.conn.close()

#: End of RegabDBConnection class.

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#: Here start the argument validation for the method: regab_gp_as_df

#: Define a ContextVar to securely hold the database reference during validation
rc_context: ContextVar[any] = ContextVar("rc_context", default=None)

#: Update the validator to read directly from the ContextVar
def _validate_db_fields(fields_input: Union[str, List[str], None]) -> List[str]:
    rc = rc_context.get()
    if rc is None:
        raise RuntimeError("Database connection context 'rc' could not be found.")
        
    with rc.conn.cursor() as curs:
        curs.execute("SELECT * FROM regab_prng_gp LIMIT 0;")
        colnames = [d[0] for d in curs.description]
        
    base_colnames = ['seq', 'batch_id', 'status', 'mover', 'opponent', 'player', 'empty_count', 'game_value']

    if not fields_input:
        return base_colnames
    if isinstance(fields_input, str):
        if fields_input == '*':
            return colnames
        raise ValueError("If 'fields' is a string, it must be '*'")
    if isinstance(fields_input, list):
        if len(fields_input) != len(set(fields_input)):
            raise ValueError("Argument 'fields' contains duplicate entries")
        invalid_cols = [x for x in fields_input if x not in colnames]
        if invalid_cols:
            raise ValueError(f"Columns do not exist in the database: {invalid_cols}")
        return fields_input

def _validate_with_db_context(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        # Dynamically extract 'rc' from positional or keyword parameters
        rc = kwargs.get('rc') if 'rc' in kwargs else (args[0] if args else None)
        
        # Set the context, execute the function, and reset it afterwards
        token = rc_context.set(rc)
        try:
            return func(*args, **kwargs)
        finally:
            rc_context.reset(token)
    return wrapper

# Helper function: converts a single int into a single-element list 
# BEFORE Pydantic runs its type validation.
def _ensure_list(v: Any) -> Any:
    """Converts a single input into a single-element list if it is not already a list."""
    return [v] if isinstance(v, (int, str)) else v

#: BeforeValidator transforms int -> List[int]
#: Field(ge=0) ensures every integer in that list is >= 0 (zero included)
BidArgument = Annotated[List[Annotated[int, Field(ge=0)]], BeforeValidator(_ensure_list)]

# Custom type for 'status': handles list conversion and checks for strings of length exactly 3
StatusArgument = Annotated[
    List[Annotated[str, Field(min_length=3, max_length=3)]], 
    BeforeValidator(_ensure_list)
]

#: Custom type for 'ec': enforces an integer strictly between 0 and 60 inclusive [0..60]
EcArgument = Annotated[int, Field(ge=0, le=60)]

#: Fields Type: Resolves database checks using AfterValidator and validation context
FieldsArgument = Annotated[Union[str, List[str], None], AfterValidator(_validate_db_fields)]

@_validate_with_db_context
@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def regab_gp_as_df(rc: RegabDBConnection,
                   bid: BidArgument,
                   status: StatusArgument,
                   ec: EcArgument,
                   limit: Union[int, None] = None,
                   where: Union[str, None] = None,
                   fields: FieldsArgument = None) -> pd.DataFrame:
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
    - The function may raise an Exception if the SQL query execution fails.
    """
            
    bid_str = ', '.join([str(x) for x in bid])
    status_str = ', '.join(["'{}'".format(x) for x in status])
    if limit:
        if not isinstance(limit, int):
            raise TypeError('Argument limit is not an instance of int')
        if not limit >= 0:
            raise ValueError('Argument limit must be equal or greather than zero')
    if where:
        if not isinstance(where, str):
            raise TypeError('Argument where is not an instance of str')
    selection = ', '.join(fields)
    q = "SELECT {:s} FROM regab_prng_gp WHERE batch_id IN ({:s}) AND empty_count = {:d} AND status IN ({:s})".format(selection, bid_str, ec, status_str)
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

#: End of regab_gp_as_df method.

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Helper function: validates that the Pandas data frame has 3 columns of type: 'int64', 'int64', 'int8'. 
# BEFORE Pydantic runs its type validation.
def _ensure_column_count_and_type(df: pd.DataFrame) -> pd.DataFrame:
    if not len(df.columns) == 3:
        raise ValueError('Argument df_mogv has not 3 columns.')
    df_mogv_expected_dtypes = ['int64', 'int64', 'int8']
    for i, expected_type in enumerate(df_mogv_expected_dtypes):
        actual_type = df.dtypes.iloc[i]
        if actual_type != expected_type:
            raise TypeError(f"Column '{i}' must be {expected_type}, but instead it is {actual_type}")
    return df

#: Custom type for 'df_mogv' argument validation.
DfMogvArgument = Annotated[pd.DataFrame, AfterValidator(_ensure_column_count_and_type)]

class RegabDataSet:
    """
    Represents a dataset containing game positions from the Reversi game.

    Attributes
    ----------
    rc : RegabDBConnection
        A database connection object
    bid : List[int]
        A list of batch IDs.
    status : List[str]
        A list of status codes, each exactly 3 characters long.
    ec : int
        The empty count, must be in the range [0, 60].
    df_mogv : pd.DataFrame
        A DataFrame containing the game positions with columns 'mover', 'opponent', and 'game_value'.
    """
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self,
                 rc: RegabDBConnection,
                 bid: BidArgument,
                 status: StatusArgument,
                 ec: EcArgument,
                 df_mogv: DfMogvArgument):
        """
        Initializes a RegabDataSet instance.

        Parameters
        ----------
        rc : RegabDBConnection
            A database connection object
        bid : List[int]
            A list of batch IDs.
        status : List[str]
            A list of status codes, each exactly 3 characters long.
        ec : int
            The empty count, must be in the range [0, 60].
        df_mogv : pd.DataFrame
            A DataFrame containing the game positions with columns 'mover', 'opponent', and 'game_value'.

        Raises
        ------
        TypeError
            If any of the arguments have an incorrect type.
        ValueError
            If any of the arguments have an invalid value.
        """
        self.rc: RegabDBConnection = rc
        self.bid: List[int]  = bid
        self.status: List[str] = status
        self.ec: int = ec
        self.df_mogv: pd.DataFrame = df_mogv
        
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def generate_positions_and_game_values(self) -> Tuple[PositionArray, GameValueArray]:
        pos_pd: pd.DataFrame = self.df_mogv
        mover = pos_pd['mover'].to_numpy().view(Bitboard)
        opponent = pos_pd['opponent'].to_numpy().view(Bitboard)
        positions = make_position(mover, opponent)
        game_values = pos_pd['game_value'].to_numpy()
        return positions, game_values

#: End of RegabDataSet class.

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def regab_extract_data_set_from_db(rc: RegabDBConnection,
                                   bid: Union[int, List[int]],
                                   status: Union[str, List[str]],
                                   ec: int) -> RegabDataSet:
    """
    Extracts data from the database and creates a RegabDataSet instance.

    Parameters
    ----------
    rc : RegabDBConnection
        An instance of the RegabDBConnection class representing the database connection.
    bid : List[int] or int
        A list of batch IDs or a single batch ID to filter the game positions.
    status : List[str] or str
        A list of status codes or a single status code to filter the game positions.
    ec : int
        The empty count to filter the game positions.

    Returns
    -------
    RegabDataSet
        An instance of RegabDataSet containing the extracted data.
    """
    selected_fields = ['mover', 'opponent', 'game_value']
    df = regab_gp_as_df(rc, bid, status, ec, limit=None, where=None, fields=selected_fields)
    df['game_value'] = df['game_value'].astype(np.int8)
    rds = RegabDataSet(rc, bid, status, ec, df)
    return rds

#: ### ### ###

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def regab_load_data_set_from_file(filename: str | Path, checksum: bool = True) -> RegabDataSet:
    """
    Loads a RegabDataSet instance from a binary file.

    Parameters
    ----------
    filename : str | Path
        The name of the file from which to load the data.
    checksum : bool, optional
        Whether to verify the SHA3-256 checksum of the file. Default is True.

    Returns
    -------
    RegabDataSet
        An instance of RegabDataSet containing the loaded data.

    Raises
    ------
    FileNotFoundError
        If the checksum file is not found when checksum verification is enabled.
    ValueError
        If the calculated checksum does not match the stored checksum.
    """

    if checksum:
        is_consistent = binio.verify_sha3_256_sidecar(filename)
        if not is_consistent:
            raise RuntimeError("The checksum file signature doesn't match with current file.")

    expected_description = "RegabDataSet binary data file"
    expected_version = 1
    
    with binio.BinaryReader(filename) as r:

        # Read the file header info
        description, version = r.read_header()
        if description != expected_description:
            ruise (RuntimeError, f"The file is not a proper {expected_description}.")
        if version != 1:
            raise (RuntimeError, f"The file version is not consistent, found {version}, expected {expected_version}")
        
        # Read the connection data.
        dbname = r.read_string()
        user = r.read_string()
        host = r.read_string()
        port = r.read_string()
        
        # Read the number of elements in bid and bid
        num_bid = r.read_u32()
        bid = [r.read_u32() for _ in range(num_bid)]

        # Read the number of elements in status
        num_status = r.read_u32()
        status = [r.read_string() for _ in range(num_status)]
        
        # Read ec
        ec = r.read_u32()

        # Read the positions and game_values
        mover = r.read_array()
        opponent = r.read_array()
        game_value = r.read_array()

    rc = RegabDBConnection(dbname, user, host, port=port, activate_conn=False)

    df_mogv = pd.DataFrame({
        'mover': mover,
        'opponent': opponent,
        'game_value': game_value
    })

    rds = RegabDataSet(rc, bid, status, ec, df_mogv)
    
    return rds

#: ### ### ###

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def regab_store_data_set_to_file(rds: RegabDataSet, filename: str | Path) -> None:
    """
    Saves the RegabDataSet instance to a binary file and calculates the SHA3-256 checksum.

    Parameters
    ----------
    filename : str | Path
        The name of the file in which to save the data.

    Returns
    -------
    None
    """

    filename = Path(filename)

    description = "RegabDataSet binary data file"
    version = 1

    with binio.BinaryWriter(filename) as w:

        #: Write header
        w.write_header(description, version)

        #: Write the connect data
        w.write_string(rds.rc.dbname)
        w.write_string(rds.rc.user)
        w.write_string(rds.rc.host)
        w.write_string(rds.rc.port)

        # Write the number of elements in bid
        num_bid = len(rds.bid)
        w.write_u32(num_bid)
        # Write the elements of bid
        for b in rds.bid:
            w.write_u32(b)
            
        # Write the number of elements in status
        num_status = len(rds.status)
        w.write_u32(num_status)
        # Write the elements of status
        for s in rds.status:
            w.write_string(s)
        
        # Write ec
        w.write_u32(rds.ec)

        # Write the data of the arrays
        w.write_array(rds.df_mogv['mover'].values)
        w.write_array(rds.df_mogv['opponent'].values)
        w.write_array(rds.df_mogv['game_value'].values)

    return

