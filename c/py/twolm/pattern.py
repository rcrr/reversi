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

from typing import TypeAlias, List, Tuple, Union, IO

import sys
import io
import time        

import numpy as np
import numpy.typing as npt

import numba as nb
from numba import njit, prange

from enum import Enum

from pydantic import validate_call, ConfigDict, BeforeValidator



__all__ = ['Pattern', 'sample_pattern_data']



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
    powers_3 (np.ndarray(n_squares,), dtype=np.uint32): Precomputed powers of 3 up to n_squares, used for index computation.

    13. Bit Shifts
    bit_shifts (np.ndarray(n_squares,), dtype=np.uint32): Precomputed bit shifts for each square in the pattern, used for index computation.

    14. Labels
    trans_fs_labels (np.ndarray(n_instances,), dtype=str): Array of names of transformation functions.
    anti_trans_fs_labels (np.ndarray(n_instances,), dtype=str): Array of names of anti transformation functions.
    symmetry_fs_labels (np.ndarray(n_stabilizers - 1,), dtype=str): Array of names of symmetry functions.
    
    15. Principal Index Dictionary
    principal_index_dict (Union[npt.NDArray[np.uint32], None]): A dictionary mapping each configuration index to its principal index.

    16. Principal Indexes
    principal_indexes (Union[npt.NDArray[np.uint32], None]): An array of unique principal indexes.

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
        self.powers_3 = 3 ** np.arange(self.n_squares, dtype=np.uint32)
        self.bit_shifts = np.arange(self.n_squares, dtype=np.uint32)

        # Used for printing information on the pattern
        self.trans_fs_labels = [bitboard_transformation_labels[i] for i in self.unique_mask_indexes]
        self.anti_trans_fs_labels = [bitboard_anti_transformation_labels[i] for i in self.unique_mask_indexes]
        self.symmetry_fs_labels = [bitboard_transformation_labels[i] for i in self.unique_symmetric_instance_indexes]

        # Get computed only if used.
        self.principal_index_dict: Union[npt.NDArray[np.uint32], None] = None
        self.principal_indexes: Union[npt.NDArray[np.uint32], None] = None
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

    #
    # Questo metodo va bene cosi. Poi ottimizziamo quello per i principal indexes.
    # Bisogna fare un unico blocco FUSO di numba ....
    #
    #: [PERF Pattern.compute_indexes_on_position, step 1] Processed 10,000,000 positions in 0.4710s (21,231,594 b/s)
    #: [PERF Pattern.compute_indexes_on_position, step 2] Processed 10,000,000 positions in 0.3227s (30,986,569 b/s)
    #: [PERF Pattern.compute_indexes_on_position, step 3] Processed 10,000,000 positions in 0.8050s (12,423,041 b/s)
    #: [PERF Pattern.compute_indexes_on_position, step 4] Processed 10,000,000 positions in 0.2499s (40,014,095 b/s)
    #: [PERF Pattern.compute_indexes_on_position] Processed 10,000,000 positions in 2.1014s (4,758,714 b/s)
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def compute_indexes_on_position(self, pos: PositionField | PositionArray) -> npt.NDArray[np.uint32]:
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
        N = len(pos)
        start_time = time.perf_counter()
        atrs_m = bitboard_anti_transformations(m)
        atrs_o = bitboard_anti_transformations(o)
        end_time = time.perf_counter()        
        duration = end_time - start_time
        positions_per_sec = N / duration
        print(f"\n[PERF Pattern.compute_indexes_on_position, step 1] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")

        # 2. Extract unique symmetries along the column axis safely
        start_time = time.perf_counter()
        sym_m = atrs_m[:, self.unique_mask_indexes]
        sym_o = atrs_o[:, self.unique_mask_indexes]
        end_time = time.perf_counter()        
        duration = end_time - start_time
        positions_per_sec = N / duration
        print(f"\n[PERF Pattern.compute_indexes_on_position, step 2] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")
    
        # 3. Pack bitboards using your fast pattern architecture logic
        start_time = time.perf_counter()
        packed_m = self._pack_bb(sym_m)
        packed_o = self._pack_bb(sym_o)
        end_time = time.perf_counter()        
        duration = end_time - start_time
        positions_per_sec = N / duration
        print(f"\n[PERF Pattern.compute_indexes_on_position, step 3] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")
    
        # 4. Compute base-3 integer mapping via the 1D flat execution layer
        start_time = time.perf_counter()
        ret = _numba_computes_indexes_kernel(
            packed_m, 
            packed_o, 
            self.bit_shifts, 
            self.powers_3
        )
        end_time = time.perf_counter()        
        duration = end_time - start_time
        positions_per_sec = N / duration
        print(f"\n[PERF Pattern.compute_indexes_on_position, step 4] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")
    
        if is_scalar:
            return ret[0] # Return as scalar view if input was a single field
        return ret
    
#### End of pattern Class.

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
        out = np.zeros(M, dtype=np.uint32)
        for j in range(M):
            val_m = packed_m[j]
            val_o = packed_o[j]
            idx_sum = np.uint32(0)
            for b in range(B):
                shift = bit_shifts[b]
                bit_m = (val_m >> shift) & np.uint64(1)
                bit_o = (val_o >> shift) & np.uint64(1)
                idx_sum += (bit_m * powers_3[b]) + (bit_o * (np.uint32(2) * powers_3[b]))
            out[j] = idx_sum
        return out
    else:
        # 2D Array Input Case (N positions, M_unique symmetries)
        N, M = packed_m.shape
        # Flatten output array for continuous L1/L2 cache efficiency
        total_elements = N * M
        out_flat = np.zeros(total_elements, dtype=np.uint32)
        
        # Flatten input views completely to eliminate non-contiguous stride overheads
        flat_m = packed_m.ravel()
        flat_o = packed_o.ravel()
        
        for i in nb.prange(total_elements):
            val_m = flat_m[i]
            val_o = flat_o[i]
            idx_sum = np.uint32(0)
            for b in range(B):
                shift = bit_shifts[b]
                bit_m = (val_m >> shift) & np.uint64(1)
                bit_o = (val_o >> shift) & np.uint64(1)
                idx_sum += (bit_m * powers_3[b]) + (bit_o * (np.uint32(2) * powers_3[b]))
            out_flat[i] = idx_sum
            
        # Reshape back to (N, M) instantaneously at the C-level with zero data copying
        return out_flat.reshape((N, M))



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

#
# To do:
#
# - compute_indexes_on_board
# - compute_indexes_on_ss_packed_tensor
# - compute_principal_index_dict
# - convert_to_principal_index
# - _compute_mover_opponent_by_index_value
#
# - class PatternSet
#
