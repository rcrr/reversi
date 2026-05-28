#
# pattern.py
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

from typing import TypeAlias

import numpy as np

from numba import njit, prange
from enum import Enum


__all__ = ['Pattern']


class PatternType(Enum):
    TYPE_0 = (
        ("I0", "T1", "T2", "T3", "T4", "T5", "T6", "T7"), 
        {"type": "0", "sample_mask": 0x0000000000000107, "sample_name": "ELLE", "level": 1, "n_instances": 8, "n_stabilizers": 1}
    )
    TYPE_1 = (
        ("I0", "T1", "S0", "S1", "T4", "T5", "S4", "S5"), 
        {"type": "1", "sample_mask": 0x0000000c30000000, "sample_name": "SNAKE", "level": 2, "n_instances": 4, "n_stabilizers": 2}
    )
    TYPE_2 = (
        ("I0", "T1", "T2", "T3", "S0", "S1", "S2", "S3"),
        {"type": "2", "sample_mask": 0x00000000000000ff, "sample_name": "EDGE", "level": 2, "n_instances": 4, "n_stabilizers": 2}
    )
    TYPE_3 = (
        ("I0", "T1", "T2", "T3", "S1", "S2", "S3", "S0"),
        {"type": "3", "sample_mask": 0x0000000000010204, "sample_name": "DIAG3", "level": 2, "n_instances": 4, "n_stabilizers": 2}
     )
    TYPE_3D = (
        ("I0", "T1", "T2", "T3", "I1", "I2", "I3", "I0"),
        {"type": "3D", "sample_mask": 0x0000000000000001, "sample_name": "DOTA1", "level": 2, "n_instances": 4, "n_stabilizers": 2}        
    )
    TYPE_4 = (
        ("I0", "T1", "T2", "T3", "S2", "S3", "S0", "S1"),
        {"type": "4", "sample_mask": 0x010100c1c1000101, "sample_name": "TAU", "level": 2, "n_instances": 4, "n_stabilizers": 2}
    )
    TYPE_5 = (
        ("I0", "T1", "T2", "T3", "S3", "S0", "S1", "S2"),
        {"type": "5", "sample_mask": 0x010204081020c0c0, "sample_name": "MACE", "level": 2, "n_instances": 4, "n_stabilizers": 2}
    )
    TYPE_6 = (
        ("I0", "S0", "S0", "S0", "T4", "S4", "S4", "S4"),
        {"type": "6", "sample_mask": 0x000008381c100000, "sample_name": "COREA", "level": 4, "n_instances": 2, "n_stabilizers": 4}
    )
    TYPE_7 = (
        ("I0", "T1", "S0", "S1", "S1", "S0", "S1", "S0"),
        {"type": "7", "sample_mask": 0x030304081020c0c0, "sample_name": "BARBEL", "level": 4, "n_instances": 2, "n_stabilizers": 4}
    )
    TYPE_7D = (
        ("I0", "T1", "S0", "S1", "I3", "I0", "I1", "I2"),
        {"type": "7D", "sample_mask": 0x0102040810204080, "sample_name": "DIAG8", "level": 4, "n_instances": 2, "n_stabilizers": 4}
    )
    TYPE_8 = (
        ("I0", "T1", "S0", "S1", "S0", "S1", "S0", "S1"),
        {"type": "8", "sample_mask": 0x0000003c3c000000, "sample_name": "RCT2X4", "level": 4, "n_instances": 2, "n_stabilizers": 4}
    )
    TYPE_9 = (
        ("I0", "S0", "S0", "S0", "S0", "S0", "S0", "S0"),
        {"type": "9", "sample_mask": 0x0000001818000000, "sample_name": "CORE", "level": 8, "n_instances": 1, "n_stabilizers": 8}
    )

    def __init__(self, fingerprint, data):
        self.fingerprint = tuple(fingerprint)
        self.data = data

    @classmethod
    def from_fingerprint(cls, fingerprint):
        if not hasattr(cls, "_lookup"):
            cls._lookup = {item.fingerprint: item for item in cls}
        return cls._lookup.get(tuple(fingerprint))



class Pattern:
    """
    The Pattern class represents a specific spatial configuration of squares (a "pattern") on a Reversi bitboard.
    It encapsulates the logic for bit manipulation, symmetry detection, and canonical form validation,
    which are essential for building Pattern Databases (PDB) or training evaluation functions.

    Attributes
    
    1. Basic Metadata
    name (str): A human-readable label for the pattern (e.g., "EDG8", "DIAG8", "CORNER").
    mask (Bitboard): The primary 64-bit mask defining the squares belonging to this pattern.

    2. Geometric & Complexity Properties
    n_squares (int): The number of bits set in the mask (dimensions of the pattern).
    n_configurations (int): The total state space size, representing all possible combinations of Empty, Black, and White discs within the pattern.
    squares (list): A list of the bit indices (0-63) of the pattern squares, sorted in descending priority.
    snames (list): Coordinate names (e.g., "A1", "B2") of the squares in the pattern.

    3. Transformation & Instance Logic
    tmasks (list): A list of 8 bitmasks generated by applying all Reversi symmetries (rotations and reflections) to the original mask.
    unique_masks (list): The subset of tmasks containing only unique bitmasks. Used to identify distinct instances of the pattern on the board.
    mask_indexes (np.ndarray, uint8): An array of 8 indices mapping each of the 8 possible transformations to its "first seen" index in tmasks.
                                      This identifies which symmetries produce identical masks.
    unique_mask_indexes (np.ndarray, uint8): The indices of the transformations that produce unique masks.
    n_instances (int): The number of unique physical locations/orientations this pattern occupies (cardinality of the orbit |Orb|).
    n_stabilizers (int): The number of transformations that leave the mask bitwise identical (order of the stabilizer |Stab|).

    4. Transformation Functions
    trans_fs (list): The set of functions used to transform the pattern squares.
    anti_trans_fs (list): The inverse functions used to map transformed squares back to the original configuration.

    5. Symmetry & Automorphism Logic
    unique_symmetric_instance_indexes (list): Indices of transformations that leave the mask bitwise identical but might
                                              permute the internal order of the squares.
                                              These represent the Automorphism Group of the pattern.
    symmetry_fs (list): The specific transformation functions that result in a bitwise identical mask,
                        used to reduce parameters in the regression model or neural network.

    6. Optimization Plans
    pack_plan (tuple[npt.NDArray[np.uint64],
               npt.NDArray[np.uint64],
               npt.NDArray[np.uint64]]): A precomputed sequence of bitwise operations (masks and shifts) used to "pack"
                                         the scattered bits of a 64-bit board into a contiguous integer index.
                                         This is critical for high-speed evaluation.
                                         The tuple contains three arrays:
                                          - p_masks (np.ndarray[np.uint64]): The block masks used for packing.
                                          - u_masks (np.ndarray[np.uint64]): The block masks used for unpacking.
                                          - p_shifts (np.ndarray[np.uint64]): The shift amounts used for packing.
    7. Transformed Cell Names
    snames_t (list[list[str]]): A 2D array of shape (8, n_squares) containing the coordinate names of pattern squares
                                after applying each of the 8 possible transformations.
                                Each row snames_t[i] contains the names of the squares as they appear in the pattern
                                after transformation i, preserving the original ordering of squares within the pattern.

    8. Transformed Cell Indices
    squares_t (list[list[int]]): A 2D array of shape (8, n_squares) containing the indices of pattern squares
                                 after applying each of the 8 possible transformations.
                                 Each row squares_t[i] contains the indices of the squares as they appear in the pattern
                                 after transformation i, preserving the original ordering of squares within the pattern.

    9. Transformed Cell Indices Sorted
    squares_ts (list[list[int]]): A 2D array of shape (8, n_squares) containing the indices of pattern squares
                                  after applying each of the 8 possible transformations, sorted.
                                  Each row squares_ts[i] contains the indices of the squares as they appear in the pattern
                                  after transformation i, sorted in ascending order.

    10. Fingerprint
    fingerprint (list[str]): A list of 8 strings that represent the configuration of the pattern under each of the 8 transformations.
                             Each string can be "I0", "T1", "S0", etc., indicating whether the transformation is identical, transposed,
                             or symmetric relative to the original configuration.

    11. Type Information
    type_info (PatternType): An object of PatternType that represents the type of pattern, based on its fingerprint.

    12. Powers of 3
    powers_3 (np.ndarray[np.uint32]): Precomputed powers of 3 up to n_squares, used for index computation.

    13. Bit Shifts
    bit_shifts (np.ndarray[np.uint32]): Precomputed bit shifts for each square in the pattern, used for index computation.

    14. Principal Index Dictionary
    principal_index_dict (Union[npt.NDArray[np.uint32], None]): A dictionary mapping each configuration index to its principal index.

    15. Principal Indexes
    principal_indexes (Union[npt.NDArray[np.uint32], None]): An array of unique principal indexes.

    16. Principal Index Count
    principal_index_count (Union[int, None]): The count of unique principal indexes.
    """

    def __init__(self, name: str, mask: Bitboard):
        """
        Initializes a new Pattern instance with the given name and mask.
        
        Args:
            name (str): The human-readable label for the pattern (e.g., "EDG8", "DIAG8", "CORNER").
            mask (Bitboard): The primary 64-bit mask defining the squares belonging to this pattern.
        
        Raises:
            TypeError: If the mask is not an instance of Bitboard.
            TypeError: If the name is not an instance of str.
            ValueError: If the mask is not the principal mask among its transformations.
        """
        if not isinstance(name, str):
            raise TypeError('Argument name is not an instance of str')
        if not isinstance(mask, Bitboard):
            raise TypeError('Argument mask is not an instance of Bitboard')

        # Da AGGIUSTARE ...
        self.name = name
        self.mask = mask
        #self.n_squares = mask.count()
        self.n_squares = bitboard_count(mask)
        self.n_configurations = 3 ** int(self.n_squares)
        self.tmasks = mask.transformations()
        self.squares = mask.to_square_list()[::-1]
        self.snames = mask.to_string_list()[::-1]
        self.unique_masks = list(dict.fromkeys(self.tmasks))
        self.mask_to_index = {m: i for i, m in reversed(list(enumerate(self.tmasks)))}
        self.mask_indexes = np.zeros(8, dtype=np.uint8)
        for i, tmask in enumerate(self.tmasks):
            self.mask_indexes[i] = self.mask_to_index[tmask]
        self.unique_mask_indexes = np.array(list(dict.fromkeys(self.mask_indexes)))
        # n_instances corresponds to the cardinality of the orbit |Orb(P)|
        self.n_instances = len(self.unique_masks)
        # n_stabilizers corresponds to the order of the stabilizer |Stab(P)|
        self.n_stabilizers = 8 // self.n_instances
        self.trans_fs = [SquareSet.trans_fs[i] for i in self.unique_mask_indexes]
        self.anti_trans_fs = [SquareSet.anti_trans_fs[i] for i in self.unique_mask_indexes]
