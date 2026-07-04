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

from twolm.regab import *
from twolm.binio import *

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
            rds = load_regab_data_set_from_file(cache_file_path)
            cc = _is_cache_consistent(model, rds)
            model.log_event(model.Relevance.INFO, f"Cache file loaded: '{cache_file_path}', is_cache_consistent={cc}.")
            if not cc:
                rds = None
                checksum_file_path = cache_file_path.with_suffix(cache_file_path.suffix + ".SHA3-256")
                cache_file_path.unlink(missing_ok=True)
                model.log_event(model.Relevance.INFO, f"Cache file {cache_file_path} deleted.")
                checksum_file_path.unlink(missing_ok=True)
                model.log_event(model.Relevance.INFO, f"Cache file checksum {checksum_file_path} deleted.")

        if not rds:
            model.log_event(model.Relevance.INFO, f"Loading data from database...")
            rds = _load_from_db(model)
            model.log_event(model.Relevance.INFO, f"Game positions loaded. Count: {len(rds.positions):,}")
            store_regab_data_set_to_file(rds, cache_file_path)
            model.log_event(model.Relevance.INFO, f"Cache file {cache_file_path} written.")
        
        model.rds = rds
        model.log_event(model.Relevance.INFO, f"Model attribute rds has been set.")
        return
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, f"Clearing game positions, setting model attribute rds to None.")
        model.rds = None
        return

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

def _is_cache_available(p: Path) -> bool:
    if p.exists():
        legacy_verify_checksum(p)
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
    model.log_event(model.Relevance.DEBUG, f"Regab Database connection closed succesfully.")

    return rds

#########################################################################################################

MAGIC_NUMBER = b"RLMRDS00"

#########################################################################################################

def load_regab_data_set_from_file(filename: str | Path, checksum: bool = True) -> RegabDataSet:
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
        legacy_verify_checksum(filename)

    with open(filename, 'rb') as f:
        # Read the fully qualified class name.
        fqcn = f.readline().decode('utf-8').strip()
        expected_fqcn = 'twolm.regab.RegabDataSet'
        if fqcn != expected_fqcn:
            raise ValueError(f"The read fqcn {fqcn} does not match the expected one {expected_fqcn}.")

        magic_number = f.read(8)
        if magic_number != MAGIC_NUMBER:
            raise ValueError(f"Magic number is not correct, 1st read. Expected = '{MAGIC_NUMBER}', found = '{magic_number}'")

        # Read the connection data.
        dbname = legacy_read_string(f)
        user = legacy_read_string(f)
        host = legacy_read_string(f)
        port = legacy_read_string(f)

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
            
        return RegabDataSet(rc, bid, status, ec, positions)

#: ### ### ###

def store_regab_data_set_to_file(rds: RegabDataSet, filename: str | Path) -> None:
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

    fqcn: str = f"{rds.__class__.__module__}.{rds.__class__.__qualname__}"
        
    # Create a SHA3-256 hash object
    sha3_256_hash = hashlib.sha3_256()
        
    with open(filename, 'wb') as f:

        fw = legacy_fun_builder_write_and_hash(f, sha3_256_hash)
        magic_number_buffer = struct.pack("8s", MAGIC_NUMBER)

        # Write the fully qualified class name
        fw((fqcn + '\n').encode('utf-8'))
        fw(magic_number_buffer)

        _write_rds_db_connection_data(rds, fw)
        fw(magic_number_buffer)

        _write_rds_header_data(rds, fw)
        fw(magic_number_buffer)

        _write_rds_positions_data(rds, fw)
        fw(magic_number_buffer)

    # Calculate the SHA3-256 checksum
    checksum = sha3_256_hash.hexdigest()

    # Write the checksum to a separate file with the same name and ".SHA3-256" suffix
    checksum_filename = filename.with_name(filename.name + ".SHA3-256")
    with open(checksum_filename, 'w') as checksum_file:
        checksum_file.write(checksum)
        
    return

#: ### ### ###

def _write_rds_db_connection_data(rds: RegabDataSet, fw: Callable[[bytes], None]) -> None:
    """
    Writes the database connection parameter to a binary file using the provided writer function.

    Parameters
    ----------
    rds: RegabDataSet
        The object to store on file
    fw : Callable[[bytes], None]
        A function that takes bytes as input and writes them to a file.

    Returns
    -------
    None

    Notes
    -----
    - The method applyes the legacy_write_string() utility that computes the length of the string
        write as a 4 bytes integer, then writes the string characters.
    - It writes `dbname`, `user`, `host` and `port`.
    - The password field is not saved, it can change without generating a change in the dataset.
    - The function returns None.
    """
    legacy_write_string(fw, rds.rc.dbname)
    legacy_write_string(fw, rds.rc.user)
    legacy_write_string(fw, rds.rc.host)
    legacy_write_string(fw, rds.rc.port)
    return

def _write_rds_header_data(rds: RegabDataSet, fw: Callable[[bytes], None]) -> None:
    """
    Writes the header RegabDataSet instance to a binary file using the provided writer function.

    Parameters
    ----------
    rds: RegabDataSet
        The object to store on file
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
    num_bid = len(rds.bid)
    fw(struct.pack('<I', num_bid))
    # Write the elements of bid
    for b in rds.bid:
        fw(struct.pack('<I', b))
            
    # Write the number of elements in status
    num_status = len(rds.status)
    fw(struct.pack('<I', num_status))
    # Write the elements of status
    for s in rds.status:
        fw(s.encode('utf-8'))
        
    # Write ec
    fw(struct.pack('<I', rds.ec))

    return

def _write_rds_positions_data(rds: RegabDataSet, fw: Callable[[bytes], None]) -> None:
    """
    Writes the core data of the RegabDataSet instance to a binary file using the provided writer function.
    
    Parameters
    ----------
    rds: RegabDataSet
        The object to store on file
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
    fw(struct.pack('<I', len(rds.positions)))

    # Write the data of the arrays
    fw(rds.positions['mover'].values.tobytes())
    fw(rds.positions['opponent'].values.tobytes())
    fw(rds.positions['game_value'].values.tobytes())

    return
