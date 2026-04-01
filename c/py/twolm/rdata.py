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

from __future__ import annotations

from twolm.domain import *

import psycopg2 as pg
import pandas as pd
import numpy as np

from typing import List, Self, Callable, Any, Union, TypeVar

import struct
import hashlib
import os
import io


__all__ = ['RegabDBConnection', 'regab_gp_as_df', 'RegabDataSet']


def _fun_builder_write_and_hash(f: io.BufferedWriter, sha3_256_hash: hashlib._Hash) -> Callable[[bytes], None]:
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

def _checksum(filename: str) -> None:
    """
    Verifies the SHA3-256 checksum of a file against a stored checksum.

    Parameters
    ----------
    filename : str
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
    # Construct the checksum filename
    checksum_filename = filename + ".SHA3-256"
            
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
    """
    def __init__(self, dbname: str, user: str, host: str, port='5432', password=None):
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
    
    def store_to_file(self, filename: str) -> None:
        """
        Saves the RegabDataSet instance to a binary file and calculates the SHA3-256 checksum.

        Parameters
        ----------
        filename : str
            The name of the file in which to save the data.

        Returns
        -------
        None
        """
        # Create a SHA3-256 hash object
        sha3_256_hash = hashlib.sha3_256()

        with open(filename, 'wb') as f:

            fw = _fun_builder_write_and_hash(f, sha3_256_hash)

            # Write the fully qualified class name
            fw((self.fqcn + '\n').encode('utf-8'))

            self.write_core_object_data(fw)

        # Calculate the SHA3-256 checksum
        checksum = sha3_256_hash.hexdigest()

        # Write the checksum to a separate file with the same name and ".SHA3-256" suffix
        checksum_filename = filename + ".SHA3-256"
        with open(checksum_filename, 'w') as checksum_file:
            checksum_file.write(checksum)

    @classmethod
    def load_from_file(cls: type[Self], filename: str, checksum: bool = True) -> Self:
        """
        Loads a RegabDataSet instance from a binary file.

        Parameters
        ----------
        filename : str
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
        if checksum:
            _checksum(filename)

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
