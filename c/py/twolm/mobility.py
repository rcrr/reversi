#
# mobility.py
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

from typing import List, Union, IO

from twolm.board import *
from twolm.mobility import *

import sys
import io
import hashlib

import numpy as np
from numba import njit, prange

from functools import reduce
import pandas as pd

from pydantic import validate_call, ConfigDict



__all__ = ['Mobility',
           'MobilitySet',
           'mobilities']



class Mobility:
    """
    The Mobility class is a measure of the available moves for the mover, masked by a defined set.
    Otherwise is the measure of the pseudo-moves available to the opponent as if she would have to move.
    When both mask and amask are defined, it is the difference between moves and anti-moves.
    """
    
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self, name: str, mask: Bitboard, amask: Bitboard):
        """
        Initializes a new Mobility feature with the given name, mask, and anti-mask.
        The mask is the set of squares counted as being legal moves.
        The amask is the set of squares counted as the actual mover would be the opponent. 
        
        Args:
            name (str): The human-readable label for the mobility (e.g., "LMC", "ALMC").
            mask (Bitboard): The 64-bit mask defining the squares belonging to this mobility feature.
            amask (Bitboard): The 64-bit mask defining the squares belonging to this anti-mobility feature.
        """

        self.name = name
        self.mask = mask
        self.amask = amask

        self.n_configurations = bitboard_count(mask) + bitboard_count(amask)
        self.n_instances = 1

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def print(self, output: Union[IO, io.StringIO] = sys.stdout) -> None:
        """
        Prints the mobility to stdout as default, or a specific IO.
        """
        prt = lambda msg: print(msg, file=output)
        prt(f"[Mobility: name = {self.name:8s}, mask = 0x{self.mask:016x}, amask = 0x{self.amask:016x}]")

        
#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

class MobilitySet:
    """
    The MobilitySet class represents a collection of Mobility objects.
    It uses the tuple (mast, amask) as a unique key.
    Mobilities are stored in a ascending sorted order based on their unique key. 
    """

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self, name: str, mobilities: List[Mobility]):
        """
        Initializes a new MobilitySet instance with the given name and list of mobilities.
        
        Args:
            name (str): The human-readable label for the set of mobilities.
            mobilities (List[Mobility]): A list of Mobility objects to be included in the set.
        
        Raises:
            ValueError: If there are duplicate names in the mobilities list.
            ValueError: If there are duplicate (mask, amask) pairs in the mobilities list.
        """
        # Extract and check for duplicate names
        names = [m.name for m in mobilities]
        if len(names) != len(set(names)):
            raise ValueError('Mobilities list contains duplicate names')
        
        # Extract composite keys as tuples and check for duplicates
        # Tuples are naturally hashable and comparable in Python
        keys = [(m.mask, m.amask) for m in mobilities]
        if len(keys) != len(set(keys)):
            raise ValueError('Mobilities list contains duplicate (mask, amask) pairs')
        
        # Sort mobilities by composite key value
        # Python sorts tuples lexicographically: first by mask, then by amask
        self.mobilities = sorted(mobilities, key=lambda m: (m.mask, m.amask))
        
        # Create hash of sorted composite keys
        # Concatenate bytes of both Bitboards sequentially for each element
        hash_input = b''.join(
            m.mask.tobytes() + m.amask.tobytes() 
            for m in self.mobilities
        )
        self.hash = hashlib.sha256(hash_input).hexdigest()
        
        self.name = name

    def names(self) -> List[str]:
        """
        Returns a list of names of the mobilities in the set.
        
        Returns:
            List[str]: A list of mobility names.
        """
        return [mobility.name for mobility in self.mobilities]

    def masks(self) -> np.ndarray:
        """
        Returns a 2D numpy array (shape = (N, 2)) of the mask values of the mobilities in the set.
        
        Returns:
            np.ndarray: A numpy array of mask values.
        """
        return np.array([[m.mask, m.amask] for m in self.mobilities], dtype=Bitboard)

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def print_summary(self, output: Union[IO, io.StringIO] = sys.stdout) -> None:
        """
        Prints a summary representation of the mobility set to stdout as default, or a specific IO.
        The summary includes the name, hash, and basic mobility information.
        """
        prt = lambda msg: print(msg, file=output)
        prt(f"MobilitySet: name = {self.name}, lenght = {len(self.mobilities)}, hash = {self.hash}")
        for m in self.mobilities:
            prt(f"  Mobility: name = {m.name:8s}, mask = 0x{m.mask:016x}, amask = 0x{m.amask:016x}")

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    
def mobilities(lms: np.ndarray) -> np.ndarray:

    mobs_def = {
        'corners': 'A1',
        'c_squares': 'B1',
        'x_squares': 'B2',
        'a_squares': 'C1',
        'r2b_squares': 'C2',
        'r2a_squares': 'D2',
        'r3b_squares': 'C3',
        'r3a_squares': 'D3',
    }

    get_uint64 = lambda sq: Bitboard(1) << square_from_str(sq)
    mobs_sq_mask = {key: get_uint64(val) for key, val in mobs_def.items()}
    mobs_8_masks = {key: bitboard_transformations(val) for key, val in mobs_sq_mask.items()}
    mobs_1_mask = {
        key: np.bitwise_or.reduce(bitboard_transformations(val))
        for key, val in mobs_sq_mask.items()
    }
    mobs = {'full': 0xFFFFFFFFFFFFFFFF, **mobs_1_mask}

    # Now in mobs I have a dictionary k:v where k is the string 'name' and v is the mobility mask.
    df = pd.DataFrame(list(mobs.items()), columns=['name', 'mask'])
    df['name'] = df['name'].astype(str)
    df['mask'] = df['mask'].astype(np.uint64)
    
    masks_array = df['mask'].to_numpy()
    
    # 1. We use the bitwise AND between the input lms and the masks.
    # We take advantage of broadcasting: 
    # lms[:, None] transforms lms into (N, 1)
    # masks_array is (9,)
    # The result will be (N, 9)
    intersections = lms[:, np.newaxis] & masks_array

    # 2. Count the active bits (popcount) for each intersection.
    # In Python/NumPy, the fastest way for bit_count on uint64:
    v_bit_count = np.vectorize(lambda x: int(x).bit_count())
    mobs_values = v_bit_count(intersections).astype(np.int32)
    
    # 3. Create a temporary DataFrame for statistics
    # Use the mask names defined in your original df
    mobs_df = pd.DataFrame(mobs_values, columns=df['name'].values)
    
    return mobs_df


