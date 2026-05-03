#
# rlm_positions_worker.py
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

#
# Reversi Logistic Model Positions Worker
#
# To properly access the database server.
#
# ssh -N -f -L 5432:localhost:5432 username@database.server.local
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.rlm_abstract_worker import  ReversiLogisticModelWorker

from pathlib import Path

import psycopg2 as pg
import pandas as pd
import numpy as np

import struct
import hashlib
import os

__all__ = ['RLMPositionsWorker']

class RLMPositionsWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Loading game positions...")

        rds: RegabDataSet = None
        
        cache_file_path = model.cfg.base_dir / model.get_cache_file_path_for_next_level()
        ca = _is_cache_available(cache_file_path)
        model.log_event(model.Relevance.INFO, f"Cache file path: '{cache_file_path}', exists={ca}.")
        
        if ca:
            rds = RegabDataSet.load_from_file(cache_file_path)
            cc = _is_cache_consistent(model, rds)
            model.log_event(model.Relevance.INFO, f"Cache file loaded: '{cache_file_path}', is_cache_consistent={cc}.")
            if not cc:
                rds = None
                checksum_file_path = cache_file_path.with_suffix(cache_file_path.suffix + ".SHA3-256")
                cache_file_path.unlink(missing_ok=True)
                model.log_event(model.Relevance.INFO, f"Cache file {cache_file_path} deleted.")
                checksum_file_path.unlink(missing_ok=True)
                model.log_event(model.Relevance.INFO, f"Cache file checksum f{checksum_file_path} deleted.")

        if not rds:
            model.log_event(model.Relevance.INFO, f"Loading data from database...")
            rds = _load_from_db(model)
            model.log_event(model.Relevance.INFO, f"Game positions loaded. Count: {len(rds.positions):,}")
            rds.store_to_file(cache_file_path)
            model.log_event(model.Relevance.INFO, f"Cache file f{cache_file_path} written.")
        
        model.rds = rds
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Clearing game positions...")
        model.rds = None

def _is_cache_available(p: Path) -> bool:
    if p.exists():
        verify_checksum(p)
        return True
    return False

def _is_cache_consistent(model: ReversiLogisticModel, rds: RegabDataSet) -> bool:
    m = model.cfg.regab_data_set.regab_db_connection
    c = rds.rc
    cfg_data_c   = (m.dbname, m.user, m.host, m.port)
    cache_data_c = (c.dbname, c.user, c.host, int(c.port))
    is_db_conn_consistent = cfg_data_c == cache_data_c
    msg_conn = (f"is_db_conn_consistent = {is_db_conn_consistent}\n"
                f"  cfg.dbname = {repr(m.dbname)}, cache.dbname = {repr(c.dbname)}\n"
                f"  cfg.user = {repr(m.user)}, cache.user = {repr(c.user)}\n"
                f"  cfg.host = {repr(m.host)}, cache.host = {repr(c.host)}\n"
                f"  cfg.port = {repr(m.port)}, cache.port = {repr(c.port)}\n")
    m = model.cfg.regab_data_set
    c = rds
    cfg_data_q   = (m.bid, m.status, m.ec)
    cache_data_q = (c.bid, c.status, c.ec)
    is_db_query_consistent = cfg_data_q == cache_data_q
    msg_query = (f"is_db_query_consistent = {is_db_query_consistent}"
                 f"  cfg.bid = {repr(m.bid)}, cache.bid = {repr(c.bid)}"
                 f"  cfg.status = {repr(m.status)}, cache.status = {repr(c.status)}"
                 f"  cfg.ec = {repr(m.ec)}, cache.ec = {repr(c.ec)}")
    is_cache_consistent = is_db_conn_consistent and is_db_query_consistent
    model.log_event(model.Relevance.DEBUG, f"is_cache_consistent = {is_cache_consistent}.")
    if not is_db_conn_consistent:
        model.log_event(model.Relevance.DEBUG, msg_conn)
    if not is_db_query_consistent:
        model.log_event(model.Relevance.DEBUG, msg_query)
    return is_cache_consistent

def _load_from_db(model: ReversiLogisticModel) -> RegabDataSet:
    cp = model.cfg.regab_data_set.regab_db_connection
    rc = RegabDBConnection(cp.dbname, cp.user, cp.host)
    model.log_event(model.Relevance.DEBUG, f"Regab Database connection {cp.dbname, cp.user, cp.host} established succesfully.")

    cp = model.cfg.regab_data_set
    rds = RegabDataSet.extract_from_db(rc, cp.bid, cp.status, cp.ec)
    model.log_event(model.Relevance.DEBUG, f"Extracted {len(rds.positions):,} positions from the database.")
    
    rc.close()
    return rds

#########################################################################################################

MAGIC_NUMBER = b"RLMRDS00"

#########################################################################################################

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

#########################################################################################################

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
    positions : pd.DataFrame
        A DataFrame containing the game positions with columns 'mover', 'opponent', and 'game_value'.

    Class Methods
    -------------
    extract_from_db(rc: RegabDBConnection, bid: Union[int, List[int]], status: Union[str, List[str]], ec: int) -> RegabDataSet
        Extracts data from the database and creates a RegabDataSet instance.
    load_from_file(filename: str | Path, checksum: bool = True) -> RegabDataSet
        Loads a RegabDataSet instance from a binary file.

    Methods
    -------
    store_to_file(filename: str | Path) -> None
        Saves the RegabDataSet instance to a binary file and calculates the SHA3-256 checksum.

    Private Methods
    ---------------
    _write_db_connection_data(fw: Callable[[bytes], None]) -> None
        Writes the db connection header of the RegabDataSet instance to a binary file using the provided writer function.
    _write_header_data(fw: Callable[[bytes], None]) -> None
        Writes the header data of the RegabDataSet instance to a binary file using the provided writer function.
    _write_positions_data(fw: Callable[[bytes], None]) -> None
        Writes the game position data of the RegabDataSet instance to a binary file using the provided writer function.
    """
    def __init__(self,
                 rc: RegabDBConnection,
                 bid: List[int],
                 status: List[str],
                 ec: int,
                 positions: pd.DataFrame):
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
        positions : pd.DataFrame
            A DataFrame containing the game positions with columns 'mover', 'opponent', and 'game_value'.

        Raises
        ------
        TypeError
            If any of the arguments have an incorrect type.
        ValueError
            If any of the arguments have an invalid value.
        """
        if not isinstance(rc, RegabDBConnection):
            raise TypeError('Argument rc is not an instance of RegabDBConnection')
            
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
        
        self.rc: RegabDBConnection = rc
        self.bid: List[int]  = bid
        self.status: List[str] = status
        self.ec: int = ec
        self.positions: pd.DataFrame = positions

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
        rds = RegabDataSet(rc, bid, status, ec, df)
        return rds

    def _write_db_connection_data(self, fw: Callable[[bytes], None]) -> None:
        """
        Writes the database connection parameter to a binary file using the provided writer function.

        Parameters
        ----------
        fw : Callable[[bytes], None]
            A function that takes bytes as input and writes them to a file.

        Returns
        -------
        None

        Notes
        -----
        - The method applyes the write_string() utility that computes the length of the string
          write as a 4 bytes integer, then writes the string characters.
        - It writes `dbname`, `user`, `host` and `port`.
        - The password field is not saved, it can change without generating a change in the dataset.
        - The function returns None.
        """
        write_string(fw, self.rc.dbname)
        write_string(fw, self.rc.user)
        write_string(fw, self.rc.host)
        write_string(fw, self.rc.port)
        return

    def _write_header_data(self, fw: Callable[[bytes], None]) -> None:
        """
        Writes the header RegabDataSet instance to a binary file using the provided writer function.

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
        - The method writes the `ec` value.
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

        return

    def _write_positions_data(self, fw: Callable[[bytes], None]) -> None:
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
        - The `length` of the data, and the binary data for the `mover`, `opponent`, and `game_value` arrays.
        - The function returns None.
        """

        # Write length
        fw(struct.pack('<I', len(self.positions)))

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

        fqcn: str = f"{self.__class__.__module__}.{self.__class__.__qualname__}"
        
        # Create a SHA3-256 hash object
        sha3_256_hash = hashlib.sha3_256()
        

        with open(filename, 'wb') as f:

            fw = fun_builder_write_and_hash(f, sha3_256_hash)
            magic_number_buffer = struct.pack("8s", MAGIC_NUMBER)

            # Write the fully qualified class name
            fw((fqcn + '\n').encode('utf-8'))
            fw(magic_number_buffer)

            self._write_db_connection_data(fw)
            fw(magic_number_buffer)
            
            self._write_header_data(fw)
            fw(magic_number_buffer)
            
            self._write_positions_data(fw)
            fw(magic_number_buffer)

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
            expected_fqcn = 'twolm.rlm_positions_worker.RegabDataSet'
            if fqcn != expected_fqcn:
                raise ValueError(f"The read fqcn {fqcn} does not match the expected one {expected_fqcn}.")

            magic_number = f.read(8)
            if magic_number != MAGIC_NUMBER:
                raise ValueError(f"Magic number is not correct, 1st read. Expected = '{MAGIC_NUMBER}', found = '{magic_number}'")

            # Read the connection data.
            dbname = read_string(f)
            user = read_string(f)
            host = read_string(f)
            port = read_string(f)

            rc = RegabDBConnection(dbname, user, host, port=port, activate_conn=False)
            
            magic_number = f.read(8)
            if magic_number != MAGIC_NUMBER:
                raise ValueError(f"Magic number is not correct, 2nd read. Expected = '{MAGIC_NUMBER}', found = '{magic_number}'")

            # Read the number of elements in bid
            num_bid = struct.unpack('<I', f.read(4))[0]
            bid = [struct.unpack('<I', f.read(4))[0] for _ in range(num_bid)]
            
            # Read the number of elements in status
            num_status = struct.unpack('<I', f.read(4))[0]
            status = [f.read(3).decode('utf-8') for _ in range(num_status)]
        
            # Read ec
            ec = struct.unpack('<I', f.read(4))[0]
            
            magic_number = f.read(8)
            if magic_number != MAGIC_NUMBER:
                raise ValueError(f"Magic number is not correct, 3rd read. Expected = '{MAGIC_NUMBER}', found = '{magic_number}'")
            
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
            
            magic_number = f.read(8)
            if magic_number != MAGIC_NUMBER:
                raise ValueError(f"Magic number is not correct, 4th read. Expected = '{MAGIC_NUMBER}', found = '{magic_number}'")
            
            return cls(rc, bid, status, ec, positions)

#########################################################################################################

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

#########################################################################################################

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

#########################################################################################################

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

#########################################################################################################

def write_string(fw: Callable[[bytes], None], s: str) -> None:
    data = s.encode('utf-8')
    fw(struct.pack("I", len(data))) 
    fw(data)

#########################################################################################################

def read_string(f: io.BufferedReader) -> str:
    raw_len = f.read(4)
    if not raw_len: return None
    length = struct.unpack("I", raw_len)[0]
    return f.read(length).decode('utf-8')

#########################################################################################################
