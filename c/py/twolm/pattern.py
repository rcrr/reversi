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

from typing import TypeAlias, List, Tuple

import numpy as np
import numpy.typing as npt

from numba import njit, prange
from enum import Enum

from pydantic import validate_call, ConfigDict, BeforeValidator



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
    pack_plan (Tuple[BitboardArray, BitboardArray, npt.NDArray[np.uint8]]) :
         A precomputed sequence of bitwise operations (masks and shifts) used to "pack"
         the scattered bits of a 64-bit board into a contiguous integer index.
         This is critical for high-speed evaluation.
         The tuple contains three arrays:
          - p_masks:  The block masks used for packing.
          - u_masks:  The block masks used for unpacking.
          - p_shifts: The shift amounts used for packing.
    
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
                sqm_up = Bitboard(unpack_ss(sqm, self))
                m = Bitboard(tfn(sqm_up))
                e = Bitboard(pack_ss(m, self))
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

#### End of pattern Class.


def pack_ss(ss_array: npt.NDArray[np.uint64], p: Pattern) -> npt.NDArray[np.uint64]:
    """
    Compresses a square set according to the mask defined by this pattern.
    
    Args:
        ss_array (npt.NDArray[np.uint64]): The input square set tensor to be packed.
        p (Pattern): The Pattern object containing the pack_plan used for compression.
    
    Returns:
        npt.NDArray[np.uint64]: The packed square set as a compressed bit array.
    """
    pack_masks, _, pack_shifts = p.pack_plan
    masked = (ss_array[..., np.newaxis] & pack_masks) >> pack_shifts
    return np.bitwise_or.reduce(masked, axis=-1)

def unpack_ss(packed_array: npt.NDArray[np.uint64], p: Pattern) -> npt.NDArray[np.uint64]:
    """
    Executes the inverse of pack_ss (PDEP simulation).
    Takes packed bits and distributes them back to their original 
    positions on the bitboard using the precomputed pack_plan.
    
    Args:
        packed_array: The array of compressed bits (result of a pack_ss operation).
        p: The Pattern object containing the pack_plan.
    
    Returns:
        npt.NDArray[np.uint64]: The unpacked square set with bits restored to their original positions.
    """
    _, unpack_masks, pack_shifts = p.pack_plan
    # 1. Caching local references to avoid attribute lookup in high-frequency calls
    u_masks = unpack_masks    # Shape (P,)
    u_shifts = pack_shifts  # Shape (P,)

    # 2. Vectorized extraction and restoration
    # (N, M, 1) & (1, 1, P) << (1, 1, P) -> (N, M, P)
    # We use broadcasting to process all blocks of the pack_plan at once
    unpacked_blocks = (packed_array[..., np.newaxis] & u_masks) << u_shifts
    
    # 3. Bitwise OR reduction across the plan axis (last axis)
    return np.bitwise_or.reduce(unpacked_blocks, axis=-1)
