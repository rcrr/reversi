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

from typing import TypeAlias, List, Tuple, Union, IO, Annotated

import sys
import io
import time
import hashlib

import numpy as np
import numpy.typing as npt

import numba as nb
from numba import njit, prange

from enum import Enum

from pydantic import validate_call, ConfigDict, BeforeValidator



__all__ = ['Index',
           'Pattern', 'sample_pattern_data',
           'PatternSet']



#: Represents a reversi pattern index.
Index: TypeAlias = np.uint32

def validate_index_array(v: any) -> any:
    """Validator to ensure a numpy array strictly uses uint32 dtype."""
    if isinstance(v, np.ndarray) and v.dtype != np.uint32:
        raise ValueError(f"Array dtype must be uint32, got {v.dtype}")
    return v

#: A Pydantic-compatible type hint for Index (np.uint32) arrays.
IndexArray = Annotated[npt.NDArray[np.uint32], BeforeValidator(validate_index_array)]

#: Pattern having more than 18 n_squares are not fitting the uint32 space.
MAX_NUMBER_OF_SQUARE_FOR_PATTERN = 18



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
    name (str): A human-readable label for the pattern (e.g., "EDGE", "DIAG8", "CORNER").
    mask (Bitboard): The primary 64-bit mask defining the squares belonging to this pattern.

    2. Geometric & Complexity Properties
    n_squares (np.uint8): The number of bits set in the mask (dimensions of the pattern).
    n_configurations (int): The total state space size, representing all possible combinations of Empty, Black, and White discs within the pattern.
                            It holds that: n_configurations = pow(3, n_squares)
    squares (List[Square]): A list of the bit indices (0-63) of the pattern squares, sorted in descending priority.
    snames (List[str]): Coordinate names (e.g., "A1", "B2") of the squares in the pattern.

    3. Transformation & Instance Logic
    tmasks (np.ndarray(8,), dtype=Bitboard): An 1D array of 8 bitmasks generated by applying all Reversi symmetries (rotations and reflections) to the original mask.
    unique_masks (List[Bitboard]): The subset of tmasks containing only unique bitmasks. Used to identify distinct instances of the pattern on the board.
    mask_indexes (np.ndarray(8,), dtype=uint8): An array of 8 indices mapping each of the 8 possible transformations to its "first seen" index in tmasks.
                                                This identifies which symmetries produce identical masks.
    unique_mask_indexes (np.ndarray(n_instances,), dtype=uint8): The indices of the transformations that produce unique masks.
    mask_to_index (Dict[Bitboard, int]): Maps the instance mask to the transformation index.
    n_instances (int): The number of unique physical locations/orientations this pattern occupies (cardinality of the orbit |Orb|).
    n_stabilizers (int): The number of transformations that leave the mask bitwise identical (order of the stabilizer |Stab|).
                         It holds that: n_instances = 8 / n_stabilizers

    4. Transformation Functions
    trans_fs (List[Callable]): The list of functions used to transform the pattern squares.
                               It holds that: len(trans_fs) = n_instances
    anti_trans_fs (List[Callable]): The inverse functions used to map transformed squares back to the original configuration.
                                    It holds that: len(anti_trans_fs) = n_instances

    5. Symmetry & Automorphism Logic
    unique_symmetric_instance_indexes (List[int]): Indices of transformations that leave the mask bitwise identical but might
                                                   permute the internal order of the squares.
                                                   These represent the Automorphism Group of the pattern.
                                                   It holds that: len (unique_symmetric_instance_indexes) = n_stabilizers - 1
    symmetry_fs (List[Callable]): The specific transformation functions that result in a bitwise identical mask,
                                  used to reduce parameters in the regression model or neural network.
                                  It holds that: len(symmetry_fs) = len (unique_symmetric_instance_indexes)
    anti_symmetry_fs (List[Callable]): The anti transformation functions for the patetrn symmetries.

    6. Optimization Plans
    pack_plan (Tuple[BitboardArray, BitboardArray, npt.NDArray[np.uint8]]) :
         A precomputed sequence of bitwise operations (masks and shifts) used to "pack"
         the scattered bits of a 64-bit board into a contiguous integer index.
         This is critical for high-speed evaluation.
         The tuple contains three arrays:
          - p_masks:  The block masks used for packing.
          - u_masks:  The block masks used for unpacking.
          - p_shifts: The shift amounts used for packing.
    
    7. Transformed Cell Names
    snames_t (List[List[str]]): A list having 8 entries. Each Entry has lenght equal to n_squares.
                                It contains the coordinate names of pattern squares
                                after applying each of the 8 possible transformations.
                                Each row snames_t[i] contains the names of the squares as they appear in the pattern
                                after transformation i, preserving the original ordering of squares within the pattern.

    8. Transformed Cell Indices
    squares_t (List[List[int]]): It has the same structure of snames_t, having the Square name replaced by its int representation.

    9. Transformed Cell Indices Sorted
    squares_ts (List[List[int]]): It is squares_t having the squares in each patetrn instance being sorted in ascending order.

    10. Fingerprint
    fingerprint (List[str]): A list of 8 strings that represent the configuration of the pattern under each of the 8 transformations.
                             Each string can be "I0", "T1", "S0", etc., indicating whether the transformation is identical, transposed,
                             or symmetric relative to the original configuration.

    11. Type Information
    type_info (PatternType): An object of PatternType that represents the type of pattern, based on its fingerprint.

    12. Powers of 3
    powers_3 (np.ndarray(n_squares,), dtype=Index): Precomputed powers of 3 up to n_squares, used for index computation.

    13. Bit Shifts
    bit_shifts (np.ndarray(n_squares,), dtype=Index): Precomputed bit shifts for each square in the pattern, used for index computation.

    14. Labels
    trans_fs_labels (np.ndarray(n_instances,), dtype=str): Array of names of transformation functions.
    anti_trans_fs_labels (np.ndarray(n_instances,), dtype=str): Array of names of anti transformation functions.
    symmetry_fs_labels (np.ndarray(n_stabilizers - 1,), dtype=str): Array of names of symmetry functions.
    
    15. Principal Index Dictionary
    principal_index_dict (Union[npt.NDArray[Index], None]): A dictionary mapping each configuration index to its principal index.

    16. Principal Indexes
    principal_indexes (Union[npt.NDArray[Index], None]): An array of unique principal indexes.

    17. Principal Index Count
    principal_index_count (Union[int, None]): The count of unique principal indexes.
    """

    @classmethod
    def mdp_csv_file(cls, patterns: list[Pattern], filename: str) -> None:
        """
        Writes a CSV file containing the MDP records of the provided patterns.
        
        Args:
            patterns: A list of Pattern objects to be written to the file.
            filename: The path to the output CSV file.
            
        Raises:
            TypeError: If patterns is not a list or if any element is not a Pattern instance.
            TypeError: If filename is not a string.
        """
        if not isinstance(patterns, list):
            raise TypeError("Argument patterns must be a list.")
        if not isinstance(filename, str):
            raise TypeError("Argument filename must be a string.")
        if not all(isinstance(p, Pattern) for p in patterns):
            raise TypeError("All elements in patterns must be Pattern instances.")

        with open(filename, 'w') as f:
            header = (
                "name,mask,"
                "n_squares,n_configurations,n_instances,n_stabilizers,"
                "cells,tmasks,mask_indexes,unique_masks,unique_mask_indexes,tr,at,sf,"
                "cells_t0,cells_t1,cells_t2,cells_t3,cells_t4,cells_t5,cells_t6,cells_t7,"
                "fingerprint,type"
            )
            f.write(header + "\n")
            for pattern in patterns:
                f.write(pattern.mdp_record() + '\n')

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

        self.name = name
        self.mask = mask
        self.n_squares = bitboard_count(mask)
        if self.n_squares > MAX_NUMBER_OF_SQUARE_FOR_PATTERN:
            raise ValueError(f"More than 18 squares is not supported! n_square = {self.n_squares}. Aborting!")
        self.n_configurations = 3 ** int(self.n_squares)
        self.tmasks = bitboard_transformations(mask)
        self.squares = bitboard_to_square_list(mask)[::-1]        
        self.snames = bitboard_to_string_list(mask)[::-1]
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
        self.trans_fs = [bitboard_trans_fs[i] for i in self.unique_mask_indexes]
        self.anti_trans_fs = [bitboard_anti_trans_fs[i] for i in self.unique_mask_indexes]

        @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
        def _check_mask_is_principal() -> int:
            """
            Checks if the mask is the principal mask among its transformations.
            The principal mask is the one having the lower value of the highest bit set.
        
            Returns:
                int: The index of the offending transformation if the mask is not principal, otherwise 0.
            """
            instance: int = 0
            highest_cell_instance_0 = bitboard_bsr(Bitboard(self.tmasks[0]))
            for i in range(1, 8):
                highest_cell = bitboard_bsr(Bitboard(self.tmasks[i]))
                if highest_cell < highest_cell_instance_0:
                    instance = i
                    break
            return instance

        principal_instance = _check_mask_is_principal()
        if principal_instance != 0:
            mesg = (
                "Argument mask is not principal, check transformations.\n"
                f"  Transformed masks: [{', '.join(f'0x{x:016X}' for x in self.tmasks)}]\n"
                f"  Principal instance is: #{principal_instance}, value: {self.tmasks[principal_instance]:016x}"
            )
            raise ValueError(mesg)
        
        @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
        def _precompute_pack_unpack_plans() -> Tuple[BitboardArray,
                                                     BitboardArray,
                                                     npt.NDArray[np.uint8]]:
            """
            Precomputes the pack and unpack plans for the pattern.
            These plans are used to compress and decompress the square set according to the pattern's mask.
        
            Returns:
                    A tuple containing three numpy arrays:
                    - p_masks:  The block masks used for packing.
                    - u_masks:  The block masks used for unpacking.
                    - p_shifts: The shift amounts used for packing.
            """
            mask = self.mask
            one = Bitboard(1)
            all_ones = Bitboard(0xFFFFFFFFFFFFFFFF)
            dest_pos = 0
            source_pos = 0

            p_masks, u_masks, p_shifts = [], [], []

            while source_pos < 64:
                while source_pos < 64 and not (mask & (one << source_pos)):
                    source_pos += 1
                if source_pos >= 64: break
        
                start_source = source_pos
                while source_pos < 64 and (mask & (np.uint64(1) << source_pos)):
                    source_pos += 1
                block_len = source_pos - start_source

                pack_mask = (all_ones >> (64 - block_len)) << start_source
                shift_amount = start_source - dest_pos
                p_masks.append(pack_mask)
                p_shifts.append(shift_amount)

                unpack_mask = (all_ones >> (64 - block_len)) << dest_pos
                u_masks.append(unpack_mask)

                dest_pos += block_len

            return (np.array(p_masks, dtype=Bitboard),
                    np.array(u_masks, dtype=Bitboard),
                    np.array(p_shifts, dtype=np.uint8))

            
        self.pack_plan = _precompute_pack_unpack_plans()
        
        # - Symmetries ...

        @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
        def _get_position_mapping(symms: List[List[int]]) -> List[int]:
            """
            Generates a mapping of unique symmetry positions.
        
            Args:
                symms: A list of lists, where each inner list represents a permutation of cell positions
                       resulting from a symmetry transformation.
        
            Returns:
                A list of integers representing the unique position mapping for each symmetry.
            """
            seen = {}
            res = []
            for i, arr in enumerate(symms):
                content = tuple(arr)
                if content not in seen:
                    seen[content] = i
                res.append(seen[content])
            return res

        symmetric_instance_indexes = np.where(self.mask_indexes == 0)[0]

        symms = []
        for i, instance_idx in enumerate(symmetric_instance_indexes):
            tfn = bitboard_trans_fs[instance_idx]
            atfn = bitboard_anti_trans_fs[instance_idx]
            exchanges = np.zeros(self.n_squares, dtype=np.uint8)
            for j in range(self.n_squares):
                sq = Square(j)
                sqm = square_as_bitboard(sq)
                sqm_up = self._unpack_bb(sqm)
                m = tfn(sqm_up)
                e = self._pack_bb(m)
                idx = bitboard_bsr(e)
                exchanges[j] = idx
            symms.append(exchanges)
        pos_map = _get_position_mapping(symms)
        unique_pos_map = set(pos_map)
        unique_pos_map.discard(0)
        self.unique_symmetric_instance_indexes = [int(x) for i, x in enumerate(symmetric_instance_indexes) if i in unique_pos_map]
        self.symmetry_fs = [bitboard_trans_fs[idx] for idx in self.unique_symmetric_instance_indexes]
        self.anti_symmetry_fs = [bitboard_anti_trans_fs[idx] for idx in self.unique_symmetric_instance_indexes]

        # - Symmetries.

        # Compute transformed cell names for each instance
        self._compute_transformed_cells()
        self.squares_ts = [sorted(l) for l in self.squares_t]

        # - Fingerprint
        self.fingerprint: list[str | None] = [None] * 8
        self.fingerprint[0] = "I0"
        for i in range (1, 8):
            transformed_squares = self.squares_t[i]
            sorted_transformed_squares = self.squares_ts[i]
            found_i = False
            found_s = False
            pos_i = -1
            pos_s = -1
            for j in range (0, i):
                if not found_i and transformed_squares == self.squares_t[j]:
                    found_i = True
                    pos_i = j
                if not found_s and sorted_transformed_squares == self.squares_ts[j]:
                    found_s = True
                    pos_s = j
            if found_i:
                marker = f"I{pos_i}"
            elif found_s:
                marker = f"S{pos_s}"
            else:
                marker = f"T{i}"
            self.fingerprint[i] = marker
        # - Fingerprint.

        self.type_info = PatternType.from_fingerprint(self.fingerprint)
        
        if self.type_info is None:
            message = (
                f"\n"
                f"  Name = {self.name}, mask = 0x{self.mask:016x}\n"
                f"  Fingerprint: [{', '.join(f'{fp}' for fp in self.fingerprint)}]\n"
            )
            raise ValueError(f"The pattern fingerprint has not been found: {message}")

        # Used by the compute_indexes_on_board method. 
        self.powers_3 = 3 ** np.arange(self.n_squares, dtype=Index)
        self.bit_shifts = np.arange(self.n_squares, dtype=Index)

        # Used for printing information on the pattern
        self.trans_fs_labels = [bitboard_transformation_labels[i] for i in self.unique_mask_indexes]
        self.anti_trans_fs_labels = [bitboard_anti_transformation_labels[i] for i in self.unique_mask_indexes]
        self.symmetry_fs_labels = [bitboard_transformation_labels[i] for i in self.unique_symmetric_instance_indexes]

        # Get computed only if used.
        self.principal_index_dict: Union[npt.NDArray[Index], None] = None
        self.principal_indexes: Union[npt.NDArray[Index], None] = None
        self.principal_index_count: Union[int, None] = None
        
         # Invariance check
        self._check_invariances()

### End of __init__

    def _compute_transformed_cells(self) -> None:
        """
        Computes the ordered cell names for each transformation instance.
        """
        # Get the original cell names in order
        original_cells = self.snames
        
        # Initialize list for each transformation
        self.squares_t = [[] for _ in range(8)]
        self.snames_t = [[] for _ in range(8)]
        
        # Apply each transformation to the entire pattern mask
        transformed_masks = bitboard_transformations(self.mask)
        
        # For each original cell, determine where it maps to in each transformed pattern
        for i, cell_name in enumerate(original_cells):
            # Convert cell name to square index
            sq = square_from_str(cell_name)
            # Create a square set with just this square
            sq_set = square_as_bitboard(sq)
            
            # For each transformation, find where this square maps to
            for j in range(8):
                # Apply transformation to the square
                transformed_sq = bitboard_trans_fs[j](sq_set)
                # Get the square index from the transformed mask
                if transformed_sq != 0:
                    # Find the bit position
                    pos = bitboard_bsr(transformed_sq)
                    transformed_cell = Square(pos)
                    # Convert back to square name
                    transformed_cell_name = square_to_str(Square(pos)).lower()
                else:
                    transformed_cell = None
                    transformed_cell_name = ""
                if transformed_cell is not None:
                    self.squares_t[j].append(int(transformed_cell))
                self.snames_t[j].append(transformed_cell_name)

    def _check_invariances(self) -> None:
        """
        Performs internal consistency checks on symmetry mappings and pack/unpack logic.
        Raises AssertionError if any invariance is violated.
        """
        # 1. Check consistency between mask_indexes and unique_masks
        # The number of unique values in mask_indexes must match len(unique_masks)
        if len(set(self.mask_indexes)) != self.n_instances:
            raise AssertionError("Inconsistency between mask_indexes and n_instances.")
        
        # 2. Check that unique_mask_indexes points to the correct unique masks
        for i, m_idx in enumerate(self.unique_mask_indexes):
            if self.tmasks[m_idx] != self.unique_masks[i]:
                raise AssertionError(f"Unique mask mismatch at index {m_idx}.")

        # 3. Check Symmetry Functions (Automorphisms)
        # For each symmetry_f, transforming the principal mask must yield the same mask
        for i, f_idx in enumerate(self.unique_symmetric_instance_indexes):
            f = self.symmetry_fs[i]
            transformed_mask = f(self.mask)
            if transformed_mask != self.mask:
                raise AssertionError(f"Symmetry function at index {f_idx} does not preserve the mask.")

        # 4. Check Mapping logic: symmetry_fs must be a subset of the first 8 transformations
        if not set(self.symmetry_fs).issubset(set(bitboard_trans_fs)):
             raise AssertionError("symmetry_fs contains unknown transformation functions.")

    def mdp_record(self) -> str:
        """
        Returns a string representation of the pattern in CSV format.
        Fields are separated by commas.
        """
        ptype = self.type_info.data["type"]
        return (
            f"{self.name},{self.mask:016X},{self.n_squares},{self.n_configurations}"
            f",{self.n_instances},{self.n_stabilizers},{':'.join(self.snames)}"
            f",{':'.join(f'{m:016X}' for m in self.tmasks)}"
            f",{':'.join(str(i) for i in self.mask_indexes)}"
            f",{':'.join(f'{m:016X}' for m in self.unique_masks)}"
            f",{':'.join(str(i) for i in self.unique_mask_indexes)}"
            f",{':'.join(f'{fn}' for fn in self.trans_fs_labels)}"
            f",{':'.join(f'{fn}' for fn in self.anti_trans_fs_labels)}"
            f",{':'.join(f'{fn}' for fn in self.symmetry_fs_labels)}"
            f",{','.join(':'.join(self.snames_t[i]) for i in range(8))}"
            f",{':'.join(f'{fp}' for fp in self.fingerprint)}"
            f",{ptype}"
        )

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def print(self, output: Union[IO, io.StringIO] = sys.stdout) -> None:
        """
        Prints a detailed representation of the pattern to stdout as default, or a specific IO.
        Includes information about the pattern's name, mask, number of squares, number of configurations,
        number of instances, number of stabilizers, type, cells, transformed masks, mask indexes,
        unique masks, unique mask indexes, transformation functions, anti-transformation functions,
        symmetry functions, transformed cells, transformed sorted cells, and fingerprint.
        """
        prt = lambda msg: print(msg, file=output)
        ptype = self.type_info.data["type"]
        prt(f"[Pattern: name = {self.name}, mask = 0x{self.mask:016x}]")
        prt(f"  [n_squares = {self.n_squares}, n_configurations = {self.n_configurations}, n_instances = {self.n_instances}, n_stabilizers = {self.n_stabilizers}, type = {ptype}]")
        prt(f"  Cells:                [{', '.join(f'{cn}' for cn in self.snames)}]")
        prt(f"  Transformed masks:    [{', '.join(f'0x{x:016X}' for x in self.tmasks)}]")
        prt(f"  Mask indexes:         [{', '.join(f'{x}' for x in self.mask_indexes)}]")
        prt(f"  Unique masks:         [{', '.join(f'0x{x:016X}' for x in self.unique_masks)}]")
        prt(f"  Unique mask indexes:  [{', '.join(f'{x}' for x in self.unique_mask_indexes)}]")
        prt(f"  Transf. functions:    [{', '.join(f'{fn}' for fn in self.trans_fs_labels)}]")
        prt(f"  Anti-transf. f.:      [{', '.join(f'{fn}' for fn in self.anti_trans_fs_labels)}]")
        prt(f"  Symmetry functions:   [{', '.join(f'{fn}' for fn in self.symmetry_fs_labels)}]")
        prt(f"  Transformed cells:    [{', '.join(f'{l}' for l in self.squares_t)}]")
        prt(f"  Transf. sorted cells: [{', '.join(f'{l}' for l in self.squares_ts)}]")
        prt(f"  Fingerprint:          [{', '.join(f'{fp}' for fp in self.fingerprint)}]")

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def _pack_bb(self, bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
        """
        Compresses a bitboard into a packed representation using a Numba-accelerated PEXT simulation.
    
        Args:
            bb: The input bitboard tensor or single value to be packed.
            
        Returns:
            The packed bitboard indices.
        """
        pack_masks, _, pack_shifts = self.pack_plan
        
        # Ensure data is passed as a NumPy array for Numba compatibility
        bb_arr = np.asarray(bb, dtype=np.uint64)
        
        result = _numba_pack_bb_kernel(bb_arr, pack_masks, pack_shifts)
        
        # If input was a scalar/object, match your original type expectations if needed
        return result if isinstance(bb, np.ndarray) else Bitboard(result.item())
        
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def _unpack_bb(self, packed_bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
        """
        Executes the inverse of _pack_bb using a parallelized Numba PDEP simulation.
            
        Args:
            packed_bb: The array or scalar of compressed bits.
            
        Returns:
            The restored bitboard configurations matching the input structure type.
        """
        _, unpack_masks, pack_shifts = self.pack_plan
        
        # Ensure correct NumPy type formatting before calling Numba
        packed_arr = np.asarray(packed_bb, dtype=np.uint64)
        
        # Call the parallelized Numba unpacking kernel
        result = _numba_unpack_bb_kernel(packed_arr, unpack_masks, pack_shifts)
        
        # Match the scalar or array output format of the input
        return result if isinstance(packed_bb, np.ndarray) else np.uint64(result)

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def compute_indexes_on_position(self, pos: PositionField | PositionArray) -> npt.NDArray[Index]:
        """
        Computes the pattern indexes on the given game position or array of positions.

        This method applies anti-transformations (symmetries) to the game position,
        filters them using unique mask indexes, packs the resulting configurations,
        and converts them into numeric identifiers (base-3 indexing).

        Args:
            pos: A single game Position or a 1D NumPy array of Positions.

        Returns:
            A 1D array of uint32 indexes if 'pos' is a scalar, or a 2D array of shape 
            (N, M_unique) if 'pos' is an array of N positions.
        """
        is_scalar = not isinstance(pos, np.ndarray)

        m = np.atleast_1d(pos['mover'])
        o = np.atleast_1d(pos['opponent'])
        
        # 1. High-speed C-layout anti-transformations
        atrs_m = bitboard_anti_transformations(m)
        atrs_o = bitboard_anti_transformations(o)

        # 2. Extract unique symmetries along the column axis safely
        sym_m = atrs_m[:, self.unique_mask_indexes]
        sym_o = atrs_o[:, self.unique_mask_indexes]
    
        # 3. Pack bitboards using your fast pattern architecture logic
        packed_m = self._pack_bb(sym_m)
        packed_o = self._pack_bb(sym_o)
    
        # 4. Compute base-3 integer mapping via the 1D flat execution layer
        ret = _numba_computes_indexes_kernel(
            packed_m, 
            packed_o, 
            self.bit_shifts, 
            self.powers_3
        )
    
        if is_scalar:
            return ret[0] # Return as scalar view if input was a single field
        return ret

    def compute_principal_index_dict(self) -> None:
        """
        Computes the dictionary of principal indexes for the pattern.
        This dictionary maps each configuration index to its principal index,
        which is the smallest index among all its symmetric configurations.
        The method also computes the unique principal indexes and their count.
        """
        def _symmetry_transformations(ss_array: npt.NDArray[Bitboard], 
                                     symmetry_functions: List[Callable[[npt.NDArray[np.uint64]], npt.NDArray[np.uint64]]]) -> npt.NDArray[np.uint64]:
            # Apply each function to the entire array and collect the results in a list
            symmetries = [f(ss_array) for f in symmetry_functions]
            # Stack the columns into a single matrix N x X
            return np.column_stack(symmetries)
        
        def _compute_mover_opponent_by_index_value(n_squares: int) -> Tuple[BitboardArray, BitboardArray]:
            N = 3 ** n_squares
            indices = np.arange(N, dtype=Index)
            powers = 3 ** np.arange(n_squares, dtype=Index)
            digits = (indices[:, None] // powers) % 3    
            bit_powers = Bitboard(1) << np.arange(n_squares, dtype=Bitboard)
            m = np.sum((digits == 1) * bit_powers, axis=1, dtype=Bitboard)
            o = np.sum((digits == 2) * bit_powers, axis=1, dtype=Bitboard)
            return m, o

        def _compute_indexes_on_ss_packed_tensor(p: Pattern, 
                                                 m: npt.NDArray[np.uint64], 
                                                 o: npt.NDArray[np.uint64]
                                                 ) -> npt.NDArray[np.uint32]:
            """
            Computes the indexes of the pattern on a packed tensor of square sets for both mover and opponent.
            
            Arguments m and o must have the same shape.
            The shape of the result matches the shape of m and o. Possible shapes are scalar, 1D array, 2D array.
            
            Args:
                m (npt.NDArray[np.uint64]): The packed square sets for the mover.
                o (npt.NDArray[np.uint64]): The packed square sets for the opponent.

            Returns:
                npt.NDArray[np.uint32]: An array of indexes representing the pattern configurations on the packed tensor.
            """
            combined = np.stack([m, o])
            bits = (combined[..., np.newaxis] >> p.bit_shifts) & 1
            idxs = bits @ p.powers_3
            indexes = (idxs[0] + 2 * idxs[1]).astype(np.uint32)
            return indexes

        # -1- Compute the packed configurations of mover and opponent.
        #     This code is executed the same for each pattern, so it could be factored and memoized, doing it for the largest one.
        #     It is not done because this computation is not part of the data pipeline.
        m, o = _compute_mover_opponent_by_index_value(int(self.n_squares))
        
        # -2- Unpack the two square sets to get the instance 0 of the pattern with the given index.
        m_unpacked, o_unpacked = self._unpack_bb(m), self._unpack_bb(o)
        
        # -3- Apply the symmetry operations
        symm_tr_fs = [bitboard_ro000] + self.symmetry_fs
        m_syms = _symmetry_transformations(m_unpacked, symm_tr_fs)
        o_syms = _symmetry_transformations(o_unpacked, symm_tr_fs)

        # -4- Pack the results
        m_syms_packed = self._pack_bb(m_syms)
        o_syms_packed = self._pack_bb(o_syms)
        
        # -5- Calculate the indexes of the symmetric instances
        sym_indexes = _compute_indexes_on_ss_packed_tensor(self, m_syms_packed, o_syms_packed)

        # -6- Take the lowest available value and insert it into principal_index_dict[index]
        self.principal_index_dict = sym_indexes.min(axis=1)
        self.principal_indexes = np.unique(self.principal_index_dict)
        self.principal_index_count = len(self.principal_indexes)
        
        return

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def convert_to_principal_index(self, index: IndexArray) -> IndexArray:
        """
        Converts a given configuration index to its principal index using the pattern's principal index dictionary.
        If the dictionary is not yet computed, it computes it first.
        """
        if not self.principal_index_dict:
            self.compute_principal_index_dict()
        principal_index = self.principal_index_dict[index]
        return principal_index

#### End of Pattern class.

@nb.njit(parallel=True, fastmath=True, cache=True)
def _numba_pack_bb_kernel(bb_array, pack_masks, pack_shifts):
    """
    Numba-accelerated PEXT simulation using direct loop fusion.
    Eliminates intermediate array allocations caused by NumPy broadcasting.
    """
    # Flattens the input view to easily iterate over any multidimensional shape
    flat_bb = bb_array.ravel()
    num_elements = flat_bb.size
    num_planes = pack_masks.shape[0]
    
    # Pre-allocate output array using np.empty (no initialization overhead)
    out = np.empty(num_elements, dtype=Bitboard)

    # CASE 1: Single-plane optimization (e.g., EDGE pattern)
    if num_planes == 1:
        mask = pack_masks[0]
        shift = pack_shifts[0]
        # nb.prange automatically distributes iterations across threads
        for i in nb.prange(num_elements):
            out[i] = (flat_bb[i] & mask) >> shift
            
    # CASE 2: Multi-plane optimization (e.g., CORNER, DIAG8)
    else:
        for i in nb.prange(num_elements):
            val = flat_bb[i]
            acc = np.uint64(0)
            for p in range(num_planes):
                acc |= (val & pack_masks[p]) >> pack_shifts[p]
            out[i] = acc
        
    return out.reshape(bb_array.shape)

@nb.njit(parallel=True, fastmath=True, cache=True)
def _numba_unpack_bb_kernel(packed_array, unpack_masks, pack_shifts):
    """
    Multi-core accelerated PDEP simulation.
    Optimized for reverse bitboard distribution across millions of positions.
    """
    flat_packed = packed_array.ravel()
    num_elements = flat_packed.size
    num_planes = unpack_masks.size  # Total number of bits/planes in the plan
    
    # Pre-allocate output array instantly using np.empty
    out = np.empty(num_elements, dtype=np.uint64)
    
    # CASE 1: Single-plane optimization (e.g., EDGE pattern)
    if num_planes == 1:
        mask = unpack_masks[0]
        shift = pack_shifts[0]
        for i in nb.prange(num_elements):
            out[i] = (flat_packed[i] & mask) << shift
            
    # CASE 2: Multi-plane optimization (e.g., CORNER, DIAG8)
    else:
        for i in nb.prange(num_elements):
            val = flat_packed[i]
            acc = np.uint64(0)
            for p in range(num_planes):
                acc |= (val & unpack_masks[p]) << pack_shifts[p]
            out[i] = acc
            
    return out.reshape(packed_array.shape)

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_computes_indexes_kernel(packed_m, packed_o, bit_shifts, powers_3):
    """
    Computes base-3 indices using a flattened 1D architecture to maximize CPU cache.
    """
    B = len(bit_shifts)
    
    if packed_m.ndim == 1:
        # 1D Array Input Case (Scalar Position Field)
        M = packed_m.shape[0]
        out = np.zeros(M, dtype=Index)
        for j in range(M):
            val_m = packed_m[j]
            val_o = packed_o[j]
            idx_sum = Index(0)
            for b in range(B):
                shift = bit_shifts[b]
                bit_m = (val_m >> shift) & Bitboard(1)
                bit_o = (val_o >> shift) & Bitboard(1)
                idx_sum += (bit_m * powers_3[b]) + (bit_o * (Index(2) * powers_3[b]))
            out[j] = idx_sum
        return out
    else:
        # 2D Array Input Case (N positions, M_unique symmetries)
        N, M = packed_m.shape
        # Flatten output array for continuous L1/L2 cache efficiency
        total_elements = N * M
        out_flat = np.zeros(total_elements, dtype=Index)
        
        # Flatten input views completely to eliminate non-contiguous stride overheads
        flat_m = packed_m.ravel()
        flat_o = packed_o.ravel()
        
        for i in nb.prange(total_elements):
            val_m = flat_m[i]
            val_o = flat_o[i]
            idx_sum = Index(0)
            for b in range(B):
                shift = bit_shifts[b]
                bit_m = (val_m >> shift) & Bitboard(1)
                bit_o = (val_o >> shift) & Bitboard(1)
                idx_sum += (bit_m * powers_3[b]) + (bit_o * (Index(2) * powers_3[b]))
            out_flat[i] = idx_sum
            
        # Reshape back to (N, M) instantaneously at the C-level with zero data copying
        return out_flat.reshape((N, M))

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#:
#: A list of Patterns.
#:
#: Use it to generate the mdp.csv file used by the LaTeX document:
#:
#: $ cd $(REVERSI_HOME)/c
#: $ source py/.reversi_venv/bin/activate
#: $ PYTHONPATH="./py" python3
#: >>> from twolm.board import *
#: >>> from twolm.pattern import *
#: >>> sample_patterns = [Pattern(name, Bitboard(mask)) for name, mask in sample_pattern_data]
#: >>> Pattern.mdp_csv_file(sample_patterns, 'build/tmp/mdp_test.csv')
#:
sample_pattern_data = [
    ('ELLE',   0x0000000000000107),
    ('SNAKE',  0x0000000C30000000),
    ('EDGE',   0x00000000000000FF),
    ('R2',     0x000000000000FF00),
    ('R3',     0x0000000000FF0000),
    ('R4',     0x00000000FF000000),
    ('XEDGE',  0x00000000000042FF),
    ('DIAG3',  0x0000000000010204),
    ('DIAG4',  0x0000000001020408),
    ('DIAG5',  0x0000000102040810),
    ('DIAG6',  0x0000010204081020),
    ('DIAG7',  0x0001020408102040),
    ('DIAG8',  0x0102040810204080),
    ('CORNER', 0x0000000000070707),
    ('2X5COR', 0x0000000000001F1F),
    ('2X6COR', 0x0000000000003F3F),
    ('RCT2X4', 0x0000003C3C000000),
    ('CASTLE', 0x000000000000C3FF),
    ('BARBEL', 0x030304081020C0C0),
    ('MACE',   0x010204081020C0C0),
    ('FOURC',  0x8100000000000081),
    ('CORE',   0x0000001818000000),
    ('CORED',  0x0000241818240000),
    ('COREA',  0x000008381C100000),
    ('WHIRL',  0x83800000000001C1),
    ('TAU',    0x010100C1C1000101),
    ('DOTA1',  0x0000000000000001),
    ('DOTB1',  0x0000000000000002),
    ('TWOND',  0x0000000000000201),
]

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

class PatternSet:
    """
    The PatternSet class represents a collection of Pattern objects.
    It uses the mask attribute of Pattern as a unique key.
    Patterns are stored in a sorted order based on the uint64 value of their mask.

    Attributes:
    name (str): A human-readable label for the set of patterns.
    patterns (List[Pattern]): A list of Pattern objects sorted by their mask values.
    hash (str): A SHA256 hash of the sorted mask values, serving as a unique identifier for the set.

    Methods:
    names: Returns a list of names of the patterns in the set.
    masks: Returns a numpy array of the mask values of the patterns in the set.
    log_summary: Logs (INFO level) a summary of the set including the name, hash, and basic pattern information.
    log: Logs (INFO level) a detailed summary of the set including the name, hash, and full pattern information.
    """

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self, name: str, patterns: List[Pattern]):
        """
        Initializes a new PatternSet instance with the given name and list of patterns.
        
        Args:
            name (str): The human-readable label for the set of patterns.
            patterns (List[Pattern]): A list of Pattern objects to be included in the set.
        
        Raises:
            TypeError: If the name is not a string.
            TypeError: If the patterns list contains non-Pattern objects.
            ValueError: If there are duplicate masks in the patterns list.
        """
        
        # Extract masks and check for duplicates
        masks = [pattern.mask for pattern in patterns]
        if len(masks) != len(set(masks)):
            raise ValueError('Patterns list contains duplicate masks')
        
        # Sort patterns by mask value
        self.patterns = sorted(patterns, key=lambda p: p.mask)
        
        # Create hash of sorted masks
        hash_input = b''.join(pattern.mask.tobytes() for pattern in self.patterns)
        self.hash = hashlib.sha256(hash_input).hexdigest()
        
        self.name = name

    def names(self) -> List[str]:
        """
        Returns a list of names of the patterns in the set.
        
        Returns:
            List[str]: A list of pattern names.
        """
        return [pattern.name for pattern in self.patterns]

    def masks(self) -> np.ndarray:
        """
        Returns a numpy array of the mask values of the patterns in the set.
        
        Returns:
            np.ndarray: A numpy array of mask values.
        """
        return np.array([pattern.mask for pattern in self.patterns], dtype=np.uint64)

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def print_summary(self, output: Union[IO, io.StringIO] = sys.stdout) -> None:
        """
        Prints a summary representation of the pattern set to stdout as default, or a specific IO.
        The summary includes the name, hash, and basic pattern information.
        """
        prt = lambda msg: print(msg, file=output)
        prt(f"PatternSet: name = {self.name}, lenght = {len(self.patterns)}, hash = {self.hash}")
        for p in self.patterns:
            prt(f"  Pattern: name = {p.name}, mask = 0x{p.mask:016x}")

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def print(self, output: Union[IO, io.StringIO] = sys.stdout) -> None:
        """
        Prints a detailed summary of the set including the name, hash, and full pattern information.
        """
        print(f"PatternSet: name = {self.name}, lenght = {len(self.patterns)}, hash = {self.hash}", file=output)
        for pattern in self.patterns:
            pattern.print(output=output)

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def compute_principal_indexes(self, m: BitboardArray, o: BitboardArray) -> IndexArray:
        """
        Computes principal indexes for all patterns in the set.
        Packs and transforms structural configurations via an end-to-end Numba pipeline.
        """
        if m.shape != o.shape:
            raise ValueError(f"Arguments o and m must have the same shape. Got m.shape = {m.shape}, o.shape = {o.shape}")
        
        N = m.shape[0]
        L = len(self.patterns)
        
        # 1. High-speed C-layout anti-transformations
        m_atrxs = bitboard_anti_transformations(m)
        o_atrxs = bitboard_anti_transformations(o)
        
        # 2. Extract layout dimensions and structural invariants
        S_max = max(p.n_squares for p in self.patterns)
        P_max = max(p.pack_plan[0].shape[0] for p in self.patterns) # Max configuration planes
        
        powers_3 = np.array([3**j for j in range(S_max)], dtype=Index)
        
        # Pre-allocate structures for unified vectorized stack execution
        n_squares_arr = np.array([p.n_squares for p in self.patterns], dtype=Index)
        num_planes_arr = np.empty(L, dtype=Index)
        
        bit_shifts_matrix = np.zeros((L, S_max), dtype=Index)
        idx_sort_matrix = np.zeros((L, 8), dtype=Index)
        
        n_groups_arr = np.empty(L, dtype=Index)
        group_size_arr = np.empty(L, dtype=Index)
        col_offsets = np.empty(L, dtype=Index)
        
        pack_masks_3d = np.zeros((L, P_max), dtype=Bitboard)
        pack_shifts_3d = np.zeros((L, P_max), dtype=Bitboard)
        
        current_col = 0
        for i, p in enumerate(self.patterns):
            S = p.n_squares
            bit_shifts_matrix[i, :S] = p.bit_shifts
            
            # Extract grouping properties for the reduction step
            mi = p.mask_indexes
            unique_vals = np.unique(mi)
            n_groups = len(unique_vals)
            
            n_groups_arr[i] = n_groups
            group_size_arr[i] = 8 // n_groups
            idx_sort_matrix[i, :] = np.argsort(mi)
            col_offsets[i] = current_col
            
            # Unpack PEXT simulation attributes from each pattern's pack_plan
            p_masks, _, p_shifts = p.pack_plan
            num_planes = p_masks.shape[0]
            num_planes_arr[i] = num_planes
            
            pack_masks_3d[i, :num_planes] = p_masks
            pack_shifts_3d[i, :num_planes] = p_shifts
            
            current_col += n_groups

        # 3. Stream data straight into the optimized Numba kernel
        return _numba_compute_principal_indexes_kernel(
            m_atrxs,
            o_atrxs,
            n_squares_arr,
            bit_shifts_matrix,
            powers_3,
            n_groups_arr,
            group_size_arr,
            idx_sort_matrix,
            current_col,
            col_offsets,
            num_planes_arr,
            pack_masks_3d,
            pack_shifts_3d
        )


#### End of PatternSet class.
        
@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_compute_principal_indexes_kernel(
    m_atrxs: np.ndarray,          # Shape: (N, 8), dtype: Bitboard
    o_atrxs: np.ndarray,          # Shape: (N, 8), dtype: Bitboard
    n_squares_arr: np.ndarray,    # Shape: (L,), dtype: Index
    bit_shifts_matrix: np.ndarray,# Shape: (L, S_max), dtype: Index
    powers_3: np.ndarray,         # Shape: (S_max,), dtype: Index
    n_groups_arr: np.ndarray,     # Shape: (L,), dtype: Index
    group_size_arr: np.ndarray,   # Shape: (L,), dtype: Index
    idx_sort_matrix: np.ndarray,  # Shape: (L, 8), dtype: Index
    total_cols: int,
    col_offsets: np.ndarray,      # Shape: (L,), dtype: Index
    num_planes_arr: np.ndarray,   # Shape: (L,), dtype: Index
    pack_masks_3d: np.ndarray,    # Shape: (L, P_max), dtype: Bitboard
    pack_shifts_3d: np.ndarray    # Shape: (L, P_max), dtype: Bitboard
) -> np.ndarray:
    """
    Fused Othello Pattern pipeline engine. 
    Processes positions (N) and patterns (L) concurrently. Instantly executes
    PEXT compression, base-3 translation, and reduction min loops in CPU registers.
    """
    BITBOARD_ZERO = Bitboard(0)
    BITBOARD_ONE = Bitboard(1)
    INDEX_TWO = Index(2)
    INDEX_ZERO = Index(0)
    
    N = m_atrxs.shape[0]
    L = n_squares_arr.shape[0]
    
    # Pre-allocate the massive contiguous block to avoid memory fragmentation
    out = np.empty((N, total_cols), dtype=Index)
    
    # Parallelize at the position level to ensure maximum CPU core saturation
    for n in nb.prange(N):
        # Local thread-safe array for the 8 calculated symmetry indices
        local_symmetry_indexes = np.empty(8, dtype=Index)
        
        for i in range(L):
            S = n_squares_arr[i]
            n_groups = n_groups_arr[i]
            group_size = group_size_arr[i]
            col_offset = col_offsets[i]
            num_planes = num_planes_arr[i]
            
            # --- LOOP FUSION 1: Integrated PEXT Simulation Layer ---
            for s in range(8):
                val_m = m_atrxs[n, s]
                val_o = o_atrxs[n, s]
                
                packed_m = BITBOARD_ZERO
                packed_o = BITBOARD_ZERO
                
                # Inline simulation of the _numba_pack_bb_kernel multi-plane logic
                for p in range(num_planes):
                    mask = pack_masks_3d[i, p]
                    shift = pack_shifts_3d[i, p]
                    packed_m |= (val_m & mask) >> shift
                    packed_o |= (val_o & mask) >> shift
                
                # --- LOOP FUSION 2: Ternary Base-3 Indexing Layer ---
                idx_m = INDEX_ZERO
                idx_o = INDEX_ZERO
                
                for b in range(S):
                    shift_val = bit_shifts_matrix[i, b]
                    p3 = powers_3[b]
                    
                    bit_m = (packed_m >> shift_val) & BITBOARD_ONE
                    bit_o = (packed_o >> shift_val) & BITBOARD_ONE
                    
                    idx_m += np.uint32(bit_m) * p3
                    idx_o += np.uint32(bit_o) * p3
                    
                local_symmetry_indexes[s] = (INDEX_TWO * idx_o) + idx_m

            # --- LOOP FUSION 3: Structural Group Minimum Reduction Layer ---
            for g in range(n_groups):
                min_val = np.uint32(4294967295) # Initialize with UINT32_MAX
                start_idx = g * group_size
                
                for k in range(group_size):
                    sorted_s = idx_sort_matrix[i, start_idx + k]
                    val = local_symmetry_indexes[sorted_s]
                    if val < min_val:
                        min_val = val
                        
                # Direct streaming write into the contiguous final output grid
                out[n, col_offset + g] = min_val
                
    return out
