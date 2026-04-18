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

"""
rdata module

This module provides classes and functions for managing and processing datasets related to Reversi game positions.
It includes functionality for connecting to a database, extracting game positions, storing and loading datasets,
and indexing game positions based on defined patterns.

Classes
-------
RegabDBConnection
    A regab database connection wraps the psycopg2 connection object.
RegabDataSet
    Represents a dataset containing game positions from the Reversi game.
RegabIndexedDataSet
    Represents a dataset containing game positions from the Reversi game being indexed by a defined list of patterns.

Functions
---------
regab_gp_as_df
    Retrieves game positions from the database and returns them as a pandas DataFrame.
"""

from __future__ import annotations

from twolm.domain import *

import psycopg2 as pg
import pandas as pd
import numpy as np

from pathlib import Path

from typing import List, Self, Callable, Any, Union, TypeVar

import struct
import hashlib
import logging
import os
import io


__all__ = ['RegabDBConnection', 'regab_gp_as_df', 'RegabDataSet', 'RegabIndexedDataSet',
           'verify_checksum', 'fun_builder_write_and_hash']


def fun_builder_write_and_hash(f: io.BufferedWriter, sha3_256_hash: hashlib._Hash) -> Callable[[bytes], None]:
    """
    Creates a function that writes data to a file and updates a SHA3-256 hash object.

    Parameters
    ----------
    f : io.BufferedWriter
        The file object to which data will be written.
    sha3_256_hash : hashlib._Hash
        The SHA3-256 hash object to be updated with the written data.

    Returns
    -------
    Callable[[bytes], None]
        A function that takes bytes as input, writes them to the file, and updates the hash object.

    Notes
    -----
    - The returned function `fwriter` is designed to be used for writing data to a file while simultaneously updating a hash object.
    - The function returns None.
    """

    def fwriter(data: bytes) -> None:
        """
        Writes data to a file and updates a SHA3-256 hash object.

        Parameters
        ----------
        data : bytes
            The data to be written to the file and used to update the hash object.

        Returns
        -------
        None
        """
        f.write(data)
        sha3_256_hash.update(data)
        return

    return fwriter

def verify_checksum(filename: str | Path) -> None:
    """
    Verifies the SHA3-256 checksum of a file against a stored checksum.

    Parameters
    ----------
    filename : str | Path
        The name of the file for which to verify the checksum.

    Returns
    -------
    None

    Raises
    ------
    FileNotFoundError
        If the checksum file does not exist.

    ValueError
        If the calculated checksum does not match the stored checksum.

    Notes
    -----
    - The checksum file is expected to have the same name as the input file with a ".SHA3-256" suffix.
    - The method reads the stored checksum from the checksum file and compares it with the actual checksum of the input file.
    """
    if not isinstance(filename, (str, Path)):
        raise TypeError('Argument filename is not an instance of str or Path')

    filename = Path(filename)
    
    # Construct the checksum filename
    checksum_filename = filename.with_name(filename.name + ".SHA3-256")
            
    # Check if the checksum file exists
    if not os.path.exists(checksum_filename):
        raise FileNotFoundError(f"The checksum file {checksum_filename} does not exist.")
            
    # Read the stored checksum from the file
    with open(checksum_filename, 'r') as checksum_file:
        stored_checksum = checksum_file.read().strip()
            
    # Calculate the actual checksum of the file
    sha3_256_hash = hashlib.sha3_256()
    with open(filename, 'rb') as f:
        while chunk := f.read(8192):
            sha3_256_hash.update(chunk)
    actual_checksum = sha3_256_hash.hexdigest()
            
    # Compare the stored checksum with the actual checksum
    if stored_checksum != actual_checksum:
        raise ValueError(f"The calculated checksum {actual_checksum} does not match the stored checksum {stored_checksum}.")


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
    """
    def __init__(self, dbname: str, user: str, host: str, port: str ='5432', password: str | None =None):
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

        Raises
        ------
        TypeError
            If any of the arguments have an incorrect type.
        """
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
    - The function may raise an Exception if the SQL query execution fails.
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


class RegabDataSet:
    """
    Represents a dataset containing game positions from the Reversi game.

    Attributes
    ----------
    bid : List[int]
        A list of batch IDs.
    status : List[str]
        A list of status codes, each exactly 3 characters long.
    ec : int
        The empty count, must be in the range [0, 60].
    positions : pd.DataFrame
        A DataFrame containing the game positions with columns 'mover', 'opponent', and 'game_value'.
    length : int
        The number of game positions in the dataset.
    fqcn : str
        The fully qualified class name of the RegabDataSet instance.

    Methods
    -------
    extract_from_db(rc: RegabDBConnection, bid: Union[int, List[int]], status: Union[str, List[str]], ec: int) -> RegabDataSet
        Extracts data from the database and creates a RegabDataSet instance.
    write_core_object_data(fw: Callable[[bytes], None]) -> None
        Writes the core data of the RegabDataSet instance to a binary file using the provided writer function.
    store_to_file(filename: str | Path) -> None
        Saves the RegabDataSet instance to a binary file and calculates the SHA3-256 checksum.
    load_from_file(filename: str | Path, checksum: bool = True) -> RegabDataSet
        Loads a RegabDataSet instance from a binary file.
    read_core_object_data(f: io.BufferedReader) -> RegabDataSet
        Reads the core data of a RegabDataSet instance from a binary file.
    """
    def __init__(self,
                 bid: List[int],
                 status: List[str],
                 ec: int,
                 positions: pd.DataFrame):
        """
        Initializes a RegabDataSet instance.

        Parameters
        ----------
        bid : List[int]
            A list of batch IDs.
        status : List[str]
            A list of status codes, each exactly 3 characters long.
        ec : int
            The empty count, must be in the range [0, 60].
        positions : pd.DataFrame
            A DataFrame containing the game positions with columns 'mover', 'opponent', and 'game_value'.

        Raises
        ------
        TypeError
            If any of the arguments have an incorrect type.
        ValueError
            If any of the arguments have an invalid value.
        """

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

        # Collect the fully qualified class name.
        self.fqcn: str = f"{self.__class__.__module__}.{self.__class__.__qualname__}"
        
        self.bid: List[int]  = bid
        self.status: List[str] = status
        self.ec: int = ec
        self.positions: pd.DataFrame = positions
        self.length: int = len(positions)

    @classmethod
    def extract_from_db(cls: type[Self],
                        rc: RegabDBConnection,
                        bid: Union[int, List[int]],
                        status: Union[str, List[str]],
                        ec: int) -> Self:
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
        rds = RegabDataSet(bid, status, ec, df)
        return rds

    def write_core_object_data(self, fw: Callable[[bytes], None]) -> None:
        """
        Writes the core data of the RegabDataSet instance to a binary file using the provided writer function.

        Parameters
        ----------
        fw : Callable[[bytes], None]
            A function that takes bytes as input and writes them to a file.

        Returns
        -------
        None

        Notes
        -----
        - The method writes the number of elements in `bid`, followed by the elements of `bid`.
        - It then writes the number of elements in `status`, followed by the elements of `status`.
        - The method writes the `ec` value, the `length` of the data, and the binary data for the `mover`, `opponent`, and `game_value` arrays.
        - The function returns None.
        """
                    
        # Write the number of elements in bid
        num_bid = len(self.bid)
        fw(struct.pack('<I', num_bid))
        # Write the elements of bid
        for b in self.bid:
            fw(struct.pack('<I', b))
            
        # Write the number of elements in status
        num_status = len(self.status)
        fw(struct.pack('<I', num_status))
        # Write the elements of status
        for s in self.status:
            fw(s.encode('utf-8'))
        
        # Write ec
        fw(struct.pack('<I', self.ec))
            
        # Write length
        fw(struct.pack('<I', self.length))

        # Write the data of the arrays
        fw(self.positions['mover'].values.tobytes())
        fw(self.positions['opponent'].values.tobytes())
        fw(self.positions['game_value'].values.tobytes())

        return
    
    def store_to_file(self, filename: str | Path) -> None:
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
        if not isinstance(filename, (str, Path)):
            raise TypeError('Argument filename is not an instance of str or Path')

        filename = Path(filename)
        
        # Create a SHA3-256 hash object
        sha3_256_hash = hashlib.sha3_256()

        with open(filename, 'wb') as f:

            fw = fun_builder_write_and_hash(f, sha3_256_hash)

            # Write the fully qualified class name
            fw((self.fqcn + '\n').encode('utf-8'))

            self.write_core_object_data(fw)

        # Calculate the SHA3-256 checksum
        checksum = sha3_256_hash.hexdigest()

        # Write the checksum to a separate file with the same name and ".SHA3-256" suffix
        checksum_filename = filename.with_name(filename.name + ".SHA3-256")
        with open(checksum_filename, 'w') as checksum_file:
            checksum_file.write(checksum)

    @classmethod
    def load_from_file(cls: type[Self], filename: str | Path, checksum: bool = True) -> Self:
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

        Notes
        -----
        - The function returns an instance of RegabDataSet.
        """
        if not isinstance(filename, (str, Path)):
            raise TypeError('Argument filename is not an instance of str or Path')

        if checksum:
            verify_checksum(filename)

        with open(filename, 'rb') as f:
            # Read the fully qualified class name.
            fqcn = f.readline().decode('utf-8').strip()
            expected_fqcn = 'twolm.rdata.RegabDataSet'
            if fqcn != expected_fqcn:
                raise ValueError(f"The read fqcn {fqcn} does not match the expected one {expected_fqcn}.")

            return RegabDataSet.read_core_object_data(f)

    @classmethod
    def read_core_object_data(cls: type[Self], f: io.BufferedReader) -> Self:
        """
        Reads the core data of a RegabDataSet instance from a binary file.

        Parameters
        ----------
        cls : type[Self]
            The class type of the RegabDataSet.
        f : io.BufferedReader
            The file object from which to read the data.

        Returns
        -------
        Self
            An instance of RegabDataSet containing the read data.

        Raises
        ------
        ValueError
            If the data read from the file is inconsistent or malformed.

        Notes
        -----
        - The method reads the number of batch IDs, the batch IDs themselves, the number of status codes, the status codes, the empty count, and the length of the data.
        - It then reads the binary data for the 'mover', 'opponent', and 'game_value' arrays and constructs a DataFrame from these arrays.
        - The function returns an instance of RegabDataSet.
        """

        # Read the number of elements in bid
        num_bid = struct.unpack('<I', f.read(4))[0]
        bid = [struct.unpack('<I', f.read(4))[0] for _ in range(num_bid)]
            
        # Read the number of elements in status
        num_status = struct.unpack('<I', f.read(4))[0]
        status = [f.read(3).decode('utf-8') for _ in range(num_status)]
        
        # Read ec
        ec = struct.unpack('<I', f.read(4))[0]
            
        # Read length
        length = struct.unpack('<I', f.read(4))[0]
            
        # Read the data of the arrays
        mover = np.frombuffer(f.read(length * 8), dtype=np.int64)
        opponent = np.frombuffer(f.read(length * 8), dtype=np.int64)
        game_value = np.frombuffer(f.read(length), dtype=np.int8)
        
        # Create the DataFrame
        positions = pd.DataFrame({
            'mover': mover,
            'opponent': opponent,
            'game_value': game_value
        })

        return cls(bid, status, ec, positions)

class RegabIndexedDataSet:
    """
    Represents a dataset containing game positions from the Reversi game being indexed by a defined list of patterns.

    Attributes
    ----------
    rds : RegabDataSet
        An instance of the RegabDataSet class containing the game positions.
    pset : PatternSet
        An instance of the PatternSet class containing the patterns to be used for indexing.
    indexes : np.ndarray
        The indexes array for each pattern by considering the relevant transformations.
    findexes : np.ndarray
        The flattened indexes array.
    lookup : np.ndarray
        The Lookup Table mapping: [Flattened_Column_Index, Original_Pattern_P, Original_Transform_K].
    revmap : np.ndarray
        The Reverse Lookup Table: revmap[p_idx][t_idx] = col_idx.
    pindexes : np.ndarray
        The principal indexes array.
    fqcn : str
        The fully qualified class name of the RegabIndexedDataSet instance.
    REVMAP_INVALID_VALUE : int
        The maximum value of a 32-bit unsigned integer used as an invalid value in the revmap.

    Methods
    -------
    load_from_file(filename: str | Path, checksum: bool = True) -> RegabIndexedDataSet
        Loads a RegabIndexedDataSet instance from a binary file.
    footprint() -> None
        Logs the memory footprint of the RegabIndexedDataSet instance.
    compute_indexes() -> None
        Computes the indexes array for each pattern by considering the relevant transformations.
    flatten_indexes() -> None
        Flattens the indexes array by selecting only the relevant transformations for each pattern.
    compute_principal_indexes() -> None
        Computes principal indexes and populates the self.pindexes attribute.
    compute_lookup() -> None
        Builds the Lookup Table: self.lookup.
    compute_revmap() -> None
        Builds the Reverse Lookup Table: self.revmap.
    get_pattern_columns(pattern_index: int) -> np.ndarray
        Retrieves the column indices corresponding to a specific pattern index.
    write_core_object_data(fw: Callable[[bytes], None]) -> None
        Writes the core data of the RegabIndexedDataSet instance to a binary file using the provided writer function.
    store_to_file(filename: str | Path) -> None
        Saves the RegabIndexedDataSet instance to a binary file and calculates the SHA3-256 checksum.
    """
    def __init__(self,
                 rds: RegabDataSet,
                 pset: PatternSet):
        """
        Initializes a RegabIndexedDataSet instance.

        Parameters
        ----------
        rds : RegabDataSet
            An instance of the RegabDataSet class containing the game positions.
        pset : PatternSet
            An instance of the PatternSet class containing the patterns to be used for indexing.

        Returns
        -------
        None

        Raises
        ------
        TypeError
            If the `rds` argument is not an instance of RegabDataSet.
            If the `pset` argument is not an instance of PatternSet.
        """
        if not isinstance(rds, RegabDataSet):
            raise TypeError('Argument rds is not an instance of RegabDataSet')
        if not isinstance(pset, PatternSet):
            raise TypeError('Argument pset is not an instance of PatternSet')

        self.rds = rds
        self.pset = pset
        
        # Collect the fully qualified class name.
        self.fqcn: str = f"{self.__class__.__module__}.{self.__class__.__qualname__}"
        
        self.indexes = None
        self.findexes = None
        self.lookup = None
        self.revmap = None
        self.pindexes = None

        max_uint32 = np.iinfo(np.uint32).max
        self.REVMAP_INVALID_VALUE = max_uint32

    @classmethod
    def load_from_file(cls: type[Self], filename: str | Path, checksum: bool = True) -> Self:
        """
        Loads a RegabIndexedDataSet instance from a binary file.

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
        EOFError
            If data is malformed or truncated.

        Notes
        -----
        - The function returns an instance of RegabIndexedDataSet.
        """
        if not isinstance(filename, (str, Path)):
            raise TypeError(f"Argument filename '{filename}'is not an instance of str or Path.")
        
        if checksum:
            verify_checksum(filename)

        with open(filename, 'rb') as f:
            # Read the fully qualified class name.
            fqcn = f.readline().decode('utf-8').strip()
            expected_fqcn = 'twolm.rdata.RegabIndexedDataSet'
            if fqcn != expected_fqcn:
                raise ValueError(f"The read fqcn {fqcn} does not match the expected one {expected_fqcn}.")
            
            return RegabIndexedDataSet.read_core_object_data(f)

    @classmethod
    def read_core_object_data(cls: type[Self], f: io.BufferedReader) -> Self:

        rds = RegabDataSet.read_core_object_data(f)
        pset = read_patternset_object_data(f)
        rids = RegabIndexedDataSet(rds, pset)
            
        # 1. Read the 5 flags (1 byte per flag, but using 'I' as per your write logic)
        # Total expected: 5 * 4 bytes = 20 bytes
        flags_raw = f.read(20)
        if len(flags_raw) < 20:
            raise EOFError("Failed to read header flags: File is truncated or corrupted.")
            
        # Unpack the 5 unsigned integers
        flags = struct.unpack('IIIII', flags_raw)
        f_indexes, f_findexes, f_pindexes, f_lookup, f_revmap = flags

        # 2. Read 'indexes' (3D array: P x POS x TRX)
        if f_indexes == 1:
            # Read 3 dimensions (24 bytes)
            dims = f.read(24)
            if len(dims) < 24: raise EOFError("Truncated dimensions for 'indexes'")
            p, pos, trx = struct.unpack('QQQ', dims)
            # Calculate size: each uint32 is 4 bytes
            data = f.read(p * pos * trx * 4)
            rids.indexes = np.frombuffer(data, dtype=np.uint32).reshape((p, pos, trx)).copy()
        else:
            rids.indexes = None

        # 3. Read 'findexes' (2D array: POS x C)
        if f_findexes == 1:
            dims = f.read(16)
            if len(dims) < 16: raise EOFError("Truncated dimensions for 'findexes'")
            pos, c = struct.unpack('QQ', dims)
            data = f.read(pos * c * 4)
            rids.findexes = np.frombuffer(data, dtype=np.uint32).reshape((pos, c)).copy()
        else:
            rids.findexes = None

        # 4. Read 'pindexes' (2D array: POS x C)
        if f_pindexes == 1:
            dims = f.read(16)
            if len(dims) < 16: raise EOFError("Truncated dimensions for 'pindexes'")
            pos, c = struct.unpack('QQ', dims)
            data = f.read(pos * c * 4)
            rids.pindexes = np.frombuffer(data, dtype=np.uint32).reshape((pos, c)).copy()
        else:
            rids.pindexes = None

        # 5. Read 'lookup' (2D array: ROW x COL)
        if f_lookup == 1:
            dims = f.read(16)
            if len(dims) < 16: raise EOFError("Truncated dimensions for 'lookup'")
            row, col = struct.unpack('QQ', dims)
            data = f.read(row * col * 4)
            rids.lookup = np.frombuffer(data, dtype=np.uint32).reshape((row, col)).copy()
        else:
            rids.lookup = None

        # 6. Read 'revmap' (2D array: ROW x COL)
        if f_revmap == 1:
            dims = f.read(16)
            if len(dims) < 16: raise EOFError("Truncated dimensions for 'revmap'")
            row, col = struct.unpack('QQ', dims)
            data = f.read(row * col * 4)
            rids.revmap = np.frombuffer(data, dtype=np.uint32).reshape((row, col)).copy()
        else:
            rids.revmap = None

        return rids
        

    def footprint(self) -> None:
        """
        Logs the memory footprint of the RegabIndexedDataSet instance.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Notes
        -----
        - The method logs the memory usage of the `rds` attribute, including the batch IDs, status codes, empty count, and length.
        - It also logs the memory usage of the `indexes`, `findexes`, and `pindexes` arrays if they have been computed.
        - The memory usage is reported in megabytes (MB).
        """
        rds = self.rds
        pset = self.pset
        bytes_in_mb = 1024**2
        positions_total_mb = rds.positions.memory_usage(deep=True).sum() / bytes_in_mb
        indexes_total_mb = 0
        findexes_total_mb = 0
        pindexes_total_mb = 0
        logging.info(f"RegabIndexedDataSet memory footprint:")
        logging.info(f"  rds:  [bid = {rds.bid}, status = {rds.status}, ec = {rds.ec}, lenght = {rds.length}]")
        logging.info(f"  pset: [name = {pset.name}, patterns = {pset.names()}]")
        logging.info(f"  - rds.positions : {positions_total_mb:9.2f} MB")
        if self.indexes is not None:
            indexes_total_mb = self.indexes.nbytes / bytes_in_mb
            logging.info(f"  - indexes       : {indexes_total_mb:9.2f} MB")
        if self.findexes is not None:
            findexes_total_mb = self.findexes.nbytes / bytes_in_mb            
            logging.info(f"  - findexes      : {findexes_total_mb:9.2f} MB")
        if self.pindexes is not None:
            pindexes_total_mb = self.pindexes.nbytes / bytes_in_mb            
            logging.info(f"  - pindexes      : {pindexes_total_mb:9.2f} MB")
        total_mb = positions_total_mb + indexes_total_mb + findexes_total_mb + pindexes_total_mb
        logging.info(f"    Total         : {total_mb:9.2f} MB")

    def compute_indexes(self) -> None:
        """
        Computes the indexes array for each pattern by considering the relevant transformations.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        ValueError
            If the `rds` or `pset` attributes are not properly initialized.

        Notes
        -----
        - The method computes the indexes array for each pattern by considering the relevant transformations.
        - It uses the `mover` and `opponent` positions from the `rds` attribute and the patterns from the `pset` attribute.
        - The resulting indexes are stored in the `indexes` attribute.
        """
        m = self.rds.positions['mover'].values.view(np.uint64)
        o = self.rds.positions['opponent'].values.view(np.uint64)
        m_atrxs = SquareSet.atrxs(m)
        o_atrxs = SquareSet.atrxs(o)
        pl = self.pset.patterns
        
        S_max = max(p.n_squares for p in pl)
        N = m.shape[0]
        indexes = np.empty((len(pl), N, 8), dtype=np.uint32)
        ONE = np.uint64(1)
        
        # Pre-allocated buffer to avoid cycles of memory allocation/deallocation
        bits_buffer_m = np.empty((N, 8, S_max), dtype=np.uint32)
        bits_buffer_o = np.empty((N, 8, S_max), dtype=np.uint32)

        for i, p in enumerate(pl):
            # p_m, p_o have shape = (N, 8), dtype = uint64
            p_m = pack_ss(m_atrxs, p)
            p_o = pack_ss(o_atrxs, p)
            #
            S = p.n_squares
            bits_m = bits_buffer_m[:, :, :S]
            bits_o = bits_buffer_o[:, :, :S]
            bits_m[:] = (p_m[..., np.newaxis] >> p.bit_shifts) & ONE
            bits_o[:] = (p_o[..., np.newaxis] >> p.bit_shifts) & ONE
            #
            # Calculate weighted ternary indices with p.powers_3
            # The @ operator (matrix product) sums along the last dimension (n_squares)
            # idx_m, idx_o shape: (N, 8)
            idx_m = bits_m @ p.powers_3
            idx_o = bits_o @ p.powers_3
            #
            # Combine the two states (0=empty, 1=mover, 2=opponent)
            # Ternary formula: sum(state * 3^j) = sum(mover * 3^j + 2 * opponent * 3^j)
            indexes[i] = np.uint32(2) * idx_o + idx_m

        self.indexes = indexes

    def flatten_indexes(self) -> None:
        """
        Flattens the indexes array by selecting only the relevant transformations for each pattern.
        Computes the findexes object attribute.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Notes
        -----
        - The method flattens the `indexes` array by selecting only the relevant transformations for each pattern.
        - It uses the `unique_mask_indexes` attribute of each pattern to filter the transformations.
        - The resulting flattened indexes are stored in the `findexes` attribute.
        - If the `indexes` array has not been computed yet, it will be computed before flattening.
        """

        if self.indexes is None:
            self.compute_indexes()

        # umi (Unique mask Indexes) is a list of arrays
        # e.g., [np.array([0, 1, 2, 3]), np.array([0]), ...]
        umi = [p.unique_mask_indexes for p in self.pset.patterns]
        fi_length = sum(a.shape[0] for a in umi)
        
        # P is the count of patterns in the PatternSet.
        # It must be equal to self.indexes.shape[0]
        # self.indexes shape is (P, N, K=8), where N is the count of positions.
        N = self.indexes.shape[1]
        P = len(self.pset.patterns)
        if P != self.indexes.shape[0]:
            raise ValueError(
                f"Data has an unrecoverable error: P = {P} and "
                f"self.indexes.shape[0] = {self.indexes.shape[0]} "
                "must have the same value."
            )

        # Flattening the data into shape (N, C=fi_length)
        # Iterateing over P, select all N records, and filter the K=8 transformations into Kp ones accordingly with the pattern properties.
        # Using .T (transpose) to ensure each slice is (N, Kp) before joining on axis 1
        flat_indexes = np.concatenate([self.indexes[p, :, umi[p]].T for p in range(P)], axis=1)
        if flat_indexes.shape != (N, fi_length):
            raise ValueError(
                f"Flattening the indexes run into problems. "
                f"The returned array shape is not what is expected. "
                f"Expected is ({N}, {fi_length}), returned is {flat_indexes.shape}."
            )
        
        self.findexes = flat_indexes
        
    def compute_principal_indexes(self) -> None:
        """
        Computes principal indexes and populates the self.pindexes attribute.
        Requires that self.indexes is calculated already, when not it does it.
        """
        if self.indexes is None:
            self.compute_indexes()
        
        # Pre-calculate the total number of columns
        # We sum the number of unique groups for each pattern
        total_cols = sum(len(p.unique_mask_indexes) for p in self.pset.patterns)
        N = self.indexes.shape[1]
    
        # Pre-allocate the final array (avoids hstack and extra copies)
        self.pindexes = np.empty((N, total_cols), dtype=np.uint32)

        current_col = 0
        for i, p in enumerate(self.pset.patterns):
            mi = p.mask_indexes
            
            # PATTERN ANALYSIS
            # We find unique values in the order they appear.
            # For [0, 1, 0, 1, 4, 5, 4, 5], unique_vals is [0, 1, 4, 5].
            unique_vals = np.unique(mi)
            n_groups = len(unique_vals)
            group_size = 8 // n_groups
            
            # PHYSICAL REORDERING
            # argsort handles non-contiguous groups perfectly.
            # For mi=[0, 1, 0, 1, 4, 5, 4, 5], idx_sort is [0, 2, 1, 3, 4, 6, 5, 7].
            # This physically moves columns of the same group together in memory.
            idx_sort = np.argsort(mi)
            self.pindexes[:, current_col : current_col + n_groups] = (
                self.indexes[i][:, idx_sort]
                .reshape(-1, n_groups, group_size)
                .min(axis=2)
            )
                
            current_col += n_groups

    def compute_lookup(self) -> None:
        """
        Builds the Lookup Table: self.lookup
        This table maps: [Flattened_Column_Index, Original_Pattern_P, Original_Transform_K]
        
        Usage:
        _, p_idx, t_idx = self.lookup[col_number]
        print(f"Column #: {col_number} describes Pattern: {p_idx}, Transformation: {t_idx}")
        """
        umi = [p.unique_mask_indexes for p in self.pset.patterns]
        lookup = []
        col_idx = 0
        for p_idx, m in enumerate(umi):
            for transform_idx in m:
                lookup.append([col_idx, p_idx, transform_idx])
                col_idx += 1
        lookup = np.array(lookup, dtype=np.uint32)
        self.lookup = lookup

    def compute_revmap(self) -> None:
        """
        Builds the Reverse Lookup Table: self.revmap
        Create a fast 2D map: revmap[p_idx][t_idx] = col_idx
        Initialize with REVMAP_INVALID_VALUE = max_uint32 as a dummy value.
        
        Usage:
        col_idx = revmap[p_idx, t_idx]
        """
        if self.lookup is None:
            self.compute_lookup()
        P = len(self.pset.patterns)
        revmap = np.full((P, 8), self.REVMAP_INVALID_VALUE, dtype=np.uint32)
        for col, p, t in self.lookup:
            revmap[p, t] = col
        self.revmap = revmap

    def get_pattern_columns(self, pattern_index: int) -> npt.NDArray[np.uint32]:
        """
        Retrieves the column indices corresponding to a specific pattern index.

        Parameters
        ----------
        pattern_index : int
            The index of the pattern for which to retrieve the column indices.

        Returns
        -------
        np.ndarray
            An array of column indices corresponding to the specified pattern index.

        Raises
        ------
        ValueError
            If the pattern index is out of the valid range.

        Notes
        -----
        - The method returns an array of column indices that correspond to the specified pattern index.
        - If the reverse map (`revmap`) has not been computed yet, it will be computed before retrieving the column indices.
        - The method filters out any invalid column indices (marked with `REVMAP_INVALID_VALUE`).

        Example
        -------
        >>> rid = RegabIndexedDataSet(rds, pset)
        >>> rid.compute_revmap()
        >>> col_indices = rid.get_pattern_columns(0)
        >>> print(col_indices)
        array([0, 1, 2, 3])
        """
        if self.revmap is None:
            self.compute_revmap()
        _col_idxs = self.revmap[pattern_index, :]
        col_idxs = _col_idxs[_col_idxs != self.REVMAP_INVALID_VALUE]
        return col_idxs

    def write_core_object_data(self, fw: Callable[[bytes], None]) -> None:
        """
        Writes the core data of the RegabIndexedDataSet instance to a binary file using the provided writer function.

        Parameters
        ----------
        fw : Callable[[bytes], None]
            A function that takes bytes as input and writes them to a file.

        Returns
        -------
        None

        Notes
        -----
        - The method writes the information if the attributes are populated or are None.
        - It writes the flags indicating whether `indexes`, `findexes`, `pindexes`, `lookup`, and `revmap` are populated.
        - If an attribute is populated, it writes the dimensions and the binary data for that attribute.
        - The function returns None.
        """
        
        self.rds.write_core_object_data(fw)
        write_patternset_object_data(self.pset, fw)

        # Writes the information specifying if the attributes are populated or are None.
        # 1 means populated, 0 means None.
        flag_indexes = 0
        flag_findexes = 0
        flag_pindexes = 0
        flag_lookup = 0
        flag_revmap = 0
        
        if self.indexes is not None:
            flag_indexes = 1
        if self.findexes is not None:
            flag_findexes = 1
        if self.pindexes is not None:
            flag_pindexes = 1
        if self.lookup is not None:
            flag_lookup = 1
        if self.revmap is not None:
            flag_revmap = 1

        fw(struct.pack('I', flag_indexes))
        fw(struct.pack('I', flag_findexes))
        fw(struct.pack('I', flag_pindexes))
        fw(struct.pack('I', flag_lookup))
        fw(struct.pack('I', flag_revmap))

        if flag_indexes == 1:
            p_dim, pos_dim, trx_dim = self.indexes.shape
            fw(struct.pack('QQQ', p_dim, pos_dim, trx_dim))
            fw(self.indexes.tobytes())

        if flag_findexes == 1:
            pos_dim, c_dim = self.findexes.shape
            fw(struct.pack('QQ', pos_dim, c_dim))
            fw(self.findexes.tobytes())

        if flag_pindexes == 1:
            pos_dim, c_dim = self.pindexes.shape
            fw(struct.pack('QQ', pos_dim, c_dim))
            fw(self.pindexes.tobytes())

        if flag_lookup == 1:
            row, col = self.lookup.shape
            fw(struct.pack('QQ', row, col))
            fw(self.lookup.tobytes())

        if flag_revmap == 1:
            row, col = self.revmap.shape
            fw(struct.pack('QQ', row, col))
            fw(self.revmap.tobytes())

    def store_to_file(self, filename: str | Path) -> None:
        """
        Saves the RegabIndexedDataSet instance to a binary file and calculates the SHA3-256 checksum.

        Parameters
        ----------
        filename : str | Path
            The name of the file in which to save the data.

        Returns
        -------
        None
        """
        if not isinstance(filename, (str, Path)):
            raise TypeError('Argument filename is not an instance of str or Path')

        filename = Path(filename)

        # Create a SHA3-256 hash object
        sha3_256_hash = hashlib.sha3_256()

        with open(filename, 'wb') as f:
            fw = fun_builder_write_and_hash(f, sha3_256_hash)
            fw((self.fqcn + '\n').encode('utf-8'))
            self.write_core_object_data(fw)

        # Calculate the SHA3-256 checksum
        checksum = sha3_256_hash.hexdigest()

        # Write the checksum to a separate file with the same name and ".SHA3-256" suffix
        checksum_filename = filename.with_name(filename.name + ".SHA3-256")
        with open(checksum_filename, 'w') as checksum_file:
            checksum_file.write(checksum)

def write_patternset_object_data(pset: PatternSet, fw: Callable[[bytes], None]) -> None:
    """
    Writes the core data of a PatternSet instance to a binary file using the provided writer function.

    Parameters
    ----------
    pset : PatternSet
        An instance of the PatternSet class containing the patterns to be written.
    fw : Callable[[bytes], None]
        A function that takes bytes as input and writes them to a file.

    Returns
    -------
    None

    Raises
    ------
    TypeError
        If the `pset` argument is not an instance of PatternSet.
    """
    if not isinstance(pset, PatternSet):
        raise TypeError('Argument pset is not an instance of PatternSet')

    # Write pset name
    pset_name_bytes = pset.name.encode('utf-8')
    fw(struct.pack('I', len(pset_name_bytes)))
    fw(pset_name_bytes)

    # Write pset pattern count.
    fw(struct.pack('I', len(pset.patterns)))
    
    # Write patterns
    for p in pset.patterns:
        name_bytes = p.name.encode('utf-8')
        fw(struct.pack('I', len(name_bytes)))
        fw(name_bytes)
        fw(struct.pack('Q', p.mask))
        
    # Write SHA256 string
    sha_bytes = pset.hash.encode('utf-8')
    fw(struct.pack('I', len(sha_bytes)))
    fw(sha_bytes)

def read_patternset_object_data(f: io.BufferedReader) -> PatternSet:
    """
    Reads the core data of a PatternSet instance from a binary file.

    Parameters
    ----------
    f : io.BufferedReader
        The file object from which to read the data.

    Returns
    -------
    PatternSet
        An instance of PatternSet containing the read data.
    """
    # Helper for reading strings (length + content)
    def read_string():
        length_data = f.read(4)
        if not length_data: return None
        length = struct.unpack('I', length_data)[0]
        return f.read(length).decode('utf-8')

    # 1. Read pset name
    pset_name = read_string()
    
    # 2. Read pset pattern count
    pattern_count_data = f.read(4)
    pattern_count = struct.unpack('I', pattern_count_data)[0]

    # 3. Read patterns
    pattern_data = []
    for _ in range(pattern_count):
        p_name = read_string()
        p_mask = struct.unpack('Q', f.read(8))[0]
        pattern_data.append({
            'name': p_name,
            'mask': np.uint64(p_mask)
        })

    # 4. Read SHA256 string
    sha_hash = read_string()

    patterns = [Pattern(item['name'], SquareSet(item['mask'])) for item in pattern_data]
    pset = PatternSet(pset_name, patterns)
    
    if sha_hash != pset.hash:
        raise ValueError(f"PatternSet read data is invalid: hash read from file '{sha_hash}', computed '{pset.hash}'")
    
    return pset
