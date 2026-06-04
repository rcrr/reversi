#
# board.py
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

from typing import TypeAlias, Annotated, List, Any

import numpy as np
import numpy.typing as npt
from numba import njit, prange

import sys
import io
from typing import IO, Union

from pydantic import validate_call, ConfigDict, BeforeValidator, SkipValidation


__all__ = ['Square', 'SquareArray',
           'square_from_str', 'square_to_str', 'square_as_bitboard',
           'square_validate_range',
           'Move', 'MoveArray',
           'move_from_str', 'move_to_str', 'move_as_bitboard',
           'move_validate_range',
           'Bitboard', 'BitboardArray',
           'bitboard_from_signed_int', 'bitboard_to_signed_int', 'bitboard_from_hex_str',
           'bitboard_bsr', 'bitboard_count',
           'bitboard_transformations', 'bitboard_print',
           'bitboard_fa1h8', 'bitboard_fh1a8', 'bitboard_fhori', 'bitboard_fvert',
           'bitboard_ro000', 'bitboard_ro090', 'bitboard_ro180', 'bitboard_ro270',
           'bitboard_to_square_list', 'bitboard_to_square_array', 'bitboard_to_string_list',
           'bitboard_trans_fs', 'bitboard_anti_trans_fs',
           'bitboard_transformation_labels', 'bitboard_anti_transformation_labels',
           'Position', 'PositionArray',
           'make_position', 'position_eq', 'position_print', 'position_empties']



#: Represents a reversi board square indexed from 0 (A1) to 63 (H8) as a uint8 scalar.
Square: TypeAlias = np.uint8

__square_names__ = [
    'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
    'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
    'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
    'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
    'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5',
    'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6',
    'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7',
    'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'H8'
]

__square_max_value__ = len(__square_names__)

def validate_square_array(v: any) -> any:
    """Validator to ensure a numpy array strictly uses uint8 dtype."""
    if isinstance(v, np.ndarray) and v.dtype != np.uint8:
        raise ValueError(f"Array dtype must be uint8, got {v.dtype}")
    return v

# A Pydantic-compatible type hint for Square (np.uint8) arrays.
SquareArray = Annotated[npt.NDArray[np.uint8], BeforeValidator(validate_square_array)]



#: Moves are the actions during the game.
#: On top of the 64 moves corresponding to fill a corresponding square with a disk,
#: a few more are defined:
#: - PA : Pass - It is still a well defined by the game rule action
#: - NA : Not available - Used as a flag status by the program
#: - UN : Unknown - Used when for instance it is unknown the parent move of a given game position
Move: TypeAlias = np.uint8

__move_names__ = __square_names__ + ['PA', 'NA', 'UN']

__move_max_value__ = len(__move_names__)

# A Pydantic-compatible type hint for Move (np.uint8) arrays.
MoveArray = Annotated[npt.NDArray[np.uint8], BeforeValidator(validate_square_array)]



#: A set of squares.
#: It is implemented as a bit-board of 64 bit using numpy.uint64 as alias type.
Bitboard: TypeAlias = np.uint64

def validate_bitboard_array(v: any) -> any:
    """Validator to ensure a numpy array strictly uses uint64 dtype."""
    if isinstance(v, np.ndarray) and v.dtype != np.uint64:
        raise ValueError(f"Array dtype must be uint64, got {v.dtype}")
    return v

#: A Pydantic-compatible type hint for Bitboard (np.uint64) arrays.
BitboardArray = Annotated[npt.NDArray[np.uint64], BeforeValidator(validate_bitboard_array)]



#: A game position is a structured native NumPy dtype.
#: Look into test_board, class TestPositionCreation, for more details in how to use it.
Position = np.dtype([('mover', Bitboard), ('opponent', Bitboard)])

Position = np.dtype([('mover', Bitboard), ('opponent', Bitboard)])
PositionField = Annotated[np.void, SkipValidation[Any]]


def validate_position_array(v: any) -> any:
    """Validates and coerces 2D uint64 arrays into a 1D Position structured array."""
    if isinstance(v, np.ndarray) and v.dtype != Position:
        if v.dtype == np.uint64 and v.ndim == 2 and v.shape[1] == 2:
            return v.view(Position).reshape(-1)
        raise ValueError(f"Array dtype must be {Position}, got {v.dtype}")
    return v

PositionArray = Annotated[npt.NDArray[any], BeforeValidator(validate_position_array)]


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def make_position(mover: Union[Bitboard, BitboardArray, List], 
                  opponent: Union[Bitboard, BitboardArray, List]
                  ) -> Union[np.void, PositionArray]:
    """
    Creates either a single Position scalar or a PositionArray from vectors/lists.
    Guarantees zero-copy view casting on the combined data where applicable.
    """
    # ARRAY / LIST CASE
    if isinstance(mover, (np.ndarray, list)) or isinstance(opponent, (np.ndarray, list)):
        # Enforce identical length check before any allocation
        if len(mover) != len(opponent):
            raise ValueError(
                f"Length mismatch: 'mover' has length {len(mover)}, "
                f"but 'opponent' has length {len(opponent)}."
            )
            
        # Convert to arrays if they are Python lists (no-op if already NumPy arrays)
        m_arr = np.asarray(mover, dtype=Bitboard)
        o_arr = np.asarray(opponent, dtype=Bitboard)
        
        # Allocate a contiguous 2D array block to interleave the data in C memory
        data = np.empty((len(m_arr), 2), dtype=Bitboard)
        data[:, 0] = m_arr
        data[:, 1] = o_arr
        
        # Interpret the memory block as a 1D structured array (Zero-Copy View)
        return data.view(Position).reshape(-1)
        
    # SCALAR CASE
    return np.void((mover, opponent), dtype=Position)



#: Square methods.

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def square_from_str(name: str) -> Square:
    """
    Creates a new Square instance from a string representation.
    """
    try:
        index = __square_names__.index(name)
        return Square(index)
    except ValueError:
        raise ValueError(f"Invalid name: '{name}'. Must be one of {__square_names__}")

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def square_to_str(sq: Square) -> str:
    """
    Returns the string representation of the square.
    """
    return __square_names__[sq]

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def square_as_bitboard(sq: Square) -> Bitboard:
    """
    Converts the square to a bitboard containing only this square.
    """
    return Bitboard(Bitboard(1) << sq)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def square_validate_range(sq: Square | SquareArray) -> None:
    if not isinstance(sq, np.ndarray):
        if not sq < 64:
            raise ValueError(f"Value must be in the range [0..63].")
    else:
        if (sq >= 64).any():
            raise ValueError("All values in the array must be in the range [0..63].")
    return



#: Move methods.

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def move_from_str(name: str) -> Move:
    """
    Creates a new Move instance from a string representation.
    """
    try:
        index = __move_names__.index(name)
        return Move(index)
    except ValueError:
        raise ValueError(f"Invalid name: '{name}'. Must be one of {__move_names__}")

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def move_to_str(mo: Move) -> str:
    """
    Returns the string representation of the move.
    """
    return __move_names__[mo]

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def move_as_bitboard(mo: Move) -> Bitboard:
    """
    Converts the move to a bitboard containing only the square indicated by the move.
    """
    return Bitboard(Bitboard(1) << mo)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def move_validate_range(mo: Move | MoveArray) -> None:
    if not isinstance(mo, np.ndarray):
        if not mo < __move_max_value__:
            raise ValueError(f"Value must be in the range [0..{__move_max_value__}].")
    else:
        if (mo >= __move_max_value__).any():
            raise ValueError(f"All values in the array must be in the range [0..{__move_max_value__}].")
    return



#: Bitboard methods.

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_from_signed_int(i: np.int64 | npt.NDArray[np.int64]) -> Bitboard | BitboardArray:
    """
    Returns a new Bitboard from a numpy.int64 value given as argument.
    Works on scalar and 1D array inputs.
    """
    
    # Standardize input by checking if it came in as a scalar or an array
    is_scalar = not isinstance(i, np.ndarray)
    i_arr = np.atleast_1d(i)
    
    # Check layout constraints (Pydantic validates types, but we handle dimensions)
    if i_arr.ndim != 1:
        raise ValueError(f"When the argument i is an array, it must be 1D, got shape {i_arr.shape}")
    
    # Adapt output representation to match original scalar or matrix input context
    if is_scalar:
        return i.view(Bitboard)[()]
    
    if i.dtype != np.int64:
        raise ValueError(f"Array dtype must be int64, got {i.dtype}")
        
    return i.view(Bitboard)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_to_signed_int(bb: Bitboard | BitboardArray) -> np.int64 | npt.NDArray[np.int64]:
    """
    Returns the signed int 64 bit long representation of the bitboard.
    It is useful to store and retrieve the value from PostgreSQL databases.
    Works on scalar and 1D array inputs.
    """
    
    # Standardize input by checking if it came in as a scalar or an array
    is_scalar = not isinstance(bb, np.ndarray)
    bb_arr = np.atleast_1d(bb)
    
    # Check layout constraints (Pydantic validates types, but we handle dimensions)
    if bb_arr.ndim != 1:
        raise ValueError(f"When the argument bb is an array, it must be 1D, got shape {bb_arr.shape}")
    
    # Adapt output representation to match original scalar or matrix input context
    if is_scalar:
        return bb.view(np.int64)[()]
    
    if bb_arr.dtype != np.uint64:
        raise ValueError(f"Array dtype must be uint64, got {bb.dtype}")
        
    return bb.view(np.int64)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_from_hex_str(h: str | npt.NDArray[str]) -> Bitboard | BitboardArray:
    """
    Returns a new Bitboard object from the hexadecimal string given as argument.
    The h string must be 16 character long having values in the range [0..F].
    Characters could be uppercase or lovercase.
    Note that the argument string is not prefixed with '0x'.
    Works on scalar and 1D array inputs.
    """
    
    # Standardize input by checking if it came in as a scalar or an array
    is_scalar = not isinstance(h, np.ndarray)
    h_arr = np.atleast_1d(h)
    
    # Check layout constraints (Pydantic validates types, but we handle dimensions)
    if h_arr.ndim != 1:
        raise ValueError(f"When the argument h is an array, it must be 1D, got shape {h_arr.shape}")

    # SCALAR CASE
    if is_scalar:
        if len(h) != 16:
            raise ValueError('Argument h must be a string of length 16')
        return Bitboard(int(h, 16))

    # ARRAY CASE
    # 1. Vectorized string length check using NumPy's character operations
    # This ensures every string in the array is exactly 16 characters long
    if not np.all(np.char.str_len(h_arr) == 16):
        raise ValueError("All hex strings in the array must be exactly 16 characters long")

    # 2. Vectorize the 'int(x, 16)' base conversion function
    # np.frompyfunc converts a standard Python function into an optimized C-level loop
    # Arguments: (function, number_of_inputs, number_of_outputs)
    vectorized_hex_parser = np.frompyfunc(lambda x: int(x, 16), 1, 1)

    # 3. Apply the vectorized parser and cast the resulting object array to pure uint64
    result_array: BitboardArray = vectorized_hex_parser(h_arr).astype(np.uint64)
    
    return result_array
    
@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_bsr(bb: Bitboard) -> int:
    """
    Bit Scan Reverse.
    Returns the index of the MOST significant set bit.
    """
    n = int(bb)
    if n == 0: return -1
    return n.bit_length() - 1

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_count(bb: Bitboard) -> int:
    """
    Returns the number of squares in the set.
    """
    bit_count = np.bitwise_count(bb)
    return bit_count

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_print(bb: Bitboard, output: Union[IO, io.StringIO] = sys.stdout) -> None:
    """
    Prints a 2D representation of the bitboard to the given output.
    """
    sq = Bitboard(1)
    print('  a b c d e f g h', file=output)
    for row in range(0,8):
        print(row + 1, end = '', file=output)
        for col in range(0,8):
            if sq & bb != 0:
                c = 'x'
            else:
                c = '.'
            print(' ' + c, end = '', file=output)
            sq <<= 1
        print(file=output)
    return

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_transformations(bb: Bitboard | BitboardArray) -> BitboardArray:
    """
    Applays the eight D8 transformations on the input bitboards.
    
     - 0 -> 0 : ro000
     - 0 -> 1 : ro090
     - 0 -> 2 : ro180
     - 0 -> 3 : ro270
     - 0 -> 4 : fvert
     - 0 -> 5 : fh1a8
     - 0 -> 6 : fhori
     - 0 -> 7 : fa1h8

    When input is a scalar Bitboard output is an arry of Bitboards having shape (8,).
    When input is an array of Bitboards of lenght N, the output has shape (N, 8).
    """
    # Standardize input by checking if it came in as a scalar or an array
    is_scalar = not isinstance(bb, np.ndarray)
    bb_arr = np.atleast_1d(bb)

    # Check layout constraints (Pydantic validates types, but we handle dimensions)
    if bb_arr.ndim != 1:
        raise ValueError(f"When the argument bb is an array, it must be 1D, got shape {bb_arr.shape}")

    # Allocate memory for output transformations matrix
    n = bb_arr.shape[0]
    ts = np.empty((n, 8), dtype=Bitboard, order='F')

    # Pre-calculate common intermediate horizontal reflection transformation
    fh = bitboard_fhori(bb_arr)

    # Fast row-wise contiguous assignments
    ts[:, 0] = bb_arr
    ts[:, 1] = bitboard_fa1h8(fh)
    ts[:, 2] = bitboard_fvert(fh)
    ts[:, 3] = bitboard_fh1a8(fh)
    ts[:, 4] = bitboard_fvert(bb_arr)
    ts[:, 5] = bitboard_fh1a8(bb_arr)
    ts[:, 6] = fh
    ts[:, 7] = bitboard_fa1h8(bb_arr)
    
    # Adapt output representation to match original scalar or matrix input context
    if is_scalar:
        return ts.flatten().view(Bitboard)
        
    return ts.view(Bitboard)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_fa1h8(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Reflects a bitboard, or an array of bitboards, on the diagonal a1-h8.
    Square h1 is mapped to a8 and vice versa.

    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . . . . . . . .
    . 2  . 1 . . . 1 . .    1 1 1 1 1 1 1 1
    . 3  . 1 . . . 1 . .    1 . . . 1 . . .
    . 4  . 1 . . 1 . . .    1 . . . 1 1 . .
    . 5  . 1 1 1 . . . .    1 . . 1 . . 1 .
    . 6  . 1 . 1 . . . .    . 1 1 . . . . 1
    . 7  . 1 . . 1 . . .    . . . . . . . .
    . 8  . 1 . . . 1 . .    . . . . . . . .

    """
    k1 = Bitboard(0x5500550055005500)
    k2 = Bitboard(0x3333000033330000)
    k4 = Bitboard(0x0f0f0f0f00000000)
    s = bb
    t =      k4 & (s ^ (s << 28))
    s = s ^       (t ^ (t >> 28))
    t =      k2 & (s ^ (s << 14))
    s = s ^       (t ^ (t >> 14))
    t =      k1 & (s ^ (s << 7))
    s = s ^       (t ^ (t >> 7))
    return s

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_fh1a8(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Reflects a bitboard, or an array of bitboards, on the diagonal h1-a8.
    Square h1 is mapped to a8 and vice versa.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . . . . . . . .
    . 2  . 1 . . . 1 . .    . . . . . . . .
    . 3  . 1 . . . 1 . .    1 . . . . 1 1 .
    . 4  . 1 . . 1 . . .    . 1 . . 1 . . 1
    . 5  . 1 1 1 . . . .    . . 1 1 . . . 1
    . 6  . 1 . 1 . . . .    . . . 1 . . . 1
    . 7  . 1 . . 1 . . .    1 1 1 1 1 1 1 1
    . 8  . 1 . . . 1 . .    . . . . . . . .
    
    """
    k1 = Bitboard(0xaa00aa00aa00aa00)
    k2 = Bitboard(0xcccc0000cccc0000)
    k4 = Bitboard(0xf0f0f0f00f0f0f0f)
    s = bb
    t =            s ^ (s << 36)
    s = s ^ (k4 & (t ^ (s >> 36)))
    t =      k2 & (s ^ (s << 18))
    s = s ^       (t ^ (t >> 18))
    t =      k1 & (s ^ (s << 9))
    s = s ^       (t ^ (t >> 9))
    return s

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_fhori(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Reflects the bitboard, or the array of bitboards, horizontally (on the horizontal axis).
    Row 1 is mapped to row 8 and vice versa.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . 1 . . . 1 . .
    . 2  . 1 . . . 1 . .    . 1 . . 1 . . .
    . 3  . 1 . . . 1 . .    . 1 . 1 . . . .
    . 4  . 1 . . 1 . . .    . 1 1 1 . . . .
    . 5  . 1 1 1 . . . .    . 1 . . 1 . . .
    . 6  . 1 . 1 . . . .    . 1 . . . 1 . .
    . 7  . 1 . . 1 . . .    . 1 . . . 1 . .
    . 8  . 1 . . . 1 . .    . 1 1 1 1 . . .
    
    """
    mask56 = Bitboard(0xFF00000000000000)
    mask48 = Bitboard(0x00FF000000000000)
    mask40 = Bitboard(0x0000FF0000000000)
    mask32 = Bitboard(0x000000FF00000000)
    mask24 = Bitboard(0x00000000FF000000)
    mask16 = Bitboard(0x0000000000FF0000)
    mask08 = Bitboard(0x000000000000FF00)
    mask00 = Bitboard(0x00000000000000FF)
    s = bb
    s = (((s << 56) & mask56) |
         ((s << 40) & mask48) |
         ((s << 24) & mask40) |
         ((s <<  8) & mask32) |
         ((s >>  8) & mask24) |
         ((s >> 24) & mask16) |
         ((s >> 40) & mask08) |
         ((s >> 56) & mask00))
    return s

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_fvert(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Reflects the bitboard, or the array of bitboards, vertically (on the vertical axis).
    Column a is mapped to column h and vice versa.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . . . 1 1 1 1 .
    . 2  . 1 . . . 1 . .    . . 1 . . . 1 .
    . 3  . 1 . . . 1 . .    . . 1 . . . 1 .
    . 4  . 1 . . 1 . . .    . . . 1 . . 1 .
    . 5  . 1 1 1 . . . .    . . . . 1 1 1 .
    . 6  . 1 . 1 . . . .    . . . . 1 . 1 .
    . 7  . 1 . . 1 . . .    . . . 1 . . 1 .
    . 8  . 1 . . . 1 . .    . . 1 . . . 1 .
    
    """
    k1 = Bitboard(0x5555555555555555)
    k2 = Bitboard(0x3333333333333333)
    k4 = Bitboard(0x0f0f0f0f0f0f0f0f)
    s = bb
    s = ((s >> 1) & k1) | ((s & k1) << 1)
    s = ((s >> 2) & k2) | ((s & k2) << 2)
    s = ((s >> 4) & k4) | ((s & k4) << 4)
    return s

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_ro000(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Returns the bitboard, or the array of bitboards, as it is (rotaded by zero degree).
    Conceptually it applies a rotation of zero degrees.
    It is the identity transformation.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . 1 1 1 1 . . .
    . 2  . 1 . . . 1 . .    . 1 . . . 1 . .
    . 3  . 1 . . . 1 . .    . 1 . . . 1 . .
    . 4  . 1 . . 1 . . .    . 1 . . 1 . . .
    . 5  . 1 1 1 . . . .    . 1 1 1 . . . .
    . 6  . 1 . 1 . . . .    . 1 . 1 . . . .
    . 7  . 1 . . 1 . . .    . 1 . . 1 . . .
    . 8  . 1 . . . 1 . .    . 1 . . . 1 . .
    
    """
    return bb

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_ro090(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Rotates the bitboard, or the array of bitboards, by 90 degrees clockwise.
    Square a1 is mapped to h1, and b1 is mapped to h2.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . . . . . . . .
    . 2  . 1 . . . 1 . .    1 1 1 1 1 1 1 1
    . 3  . 1 . . . 1 . .    . . . 1 . . . 1
    . 4  . 1 . . 1 . . .    . . 1 1 . . . 1
    . 5  . 1 1 1 . . . .    . 1 . . 1 . . 1
    . 6  . 1 . 1 . . . .    1 . . . . 1 1 .
    . 7  . 1 . . 1 . . .    . . . . . . . .
    . 8  . 1 . . . 1 . .    . . . . . . . .
    
    """
    
    return bitboard_fhori(bitboard_fh1a8(bb))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_ro180(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Rotates the bitboard, or the array of bitboards, by 180 degrees.
    Square a1 is mapped to h8, and b1 is mapped to g8.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . . 1 . . . 1 .
    . 2  . 1 . . . 1 . .    . . . 1 . . 1 .
    . 3  . 1 . . . 1 . .    . . . . 1 . 1 .
    . 4  . 1 . . 1 . . .    . . . . 1 1 1 .
    . 5  . 1 1 1 . . . .    . . . 1 . . 1 .
    . 6  . 1 . 1 . . . .    . . 1 . . . 1 .
    . 7  . 1 . . 1 . . .    . . 1 . . . 1 .
    . 8  . 1 . . . 1 . .    . . . 1 1 1 1 .
    
    """
    
    return bitboard_fvert(bitboard_fhori(bb))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_ro270(bb: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Rotates the bitboard, or the array of bitboards, by 270 degrees clockwise.
    Square a1 is mapped to a8, and b1 is mapped to a7.
        
    .    ---- input ----    ---- output ---
    .
    .    a b c d e f g h    a b c d e f g h
    .
    . 1  . 1 1 1 1 . . .    . . . . . . . .
    . 2  . 1 . . . 1 . .    . . . . . . . .
    . 3  . 1 . . . 1 . .    . 1 1 . . . . 1
    . 4  . 1 . . 1 . . .    1 . . 1 . . 1 .
    . 5  . 1 1 1 . . . .    1 . . . 1 1 . .
    . 6  . 1 . 1 . . . .    1 . . . 1 . . .
    . 7  . 1 . . 1 . . .    1 1 1 1 1 1 1 1
    . 8  . 1 . . . 1 . .    . . . . . . . .
    
    """
    
    return bitboard_fh1a8(bitboard_fhori(bb))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_to_square_list(bb: Bitboard) -> List[Square]:
    """
    Returns the bitboard represented as a list of squares.
    Squares are ordered from larger (H8) to smaller (A1).
    """

    squares = []

    if not bb == 0:
        while bb:
            sq = bitboard_bsr(bb)
            squares.append(Square(sq))
            bb = Bitboard(bb ^ (Bitboard(1) << sq))
            
    return squares

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_to_square_array(bb: Bitboard) -> SquareArray:
    """
    Returns the square set as a numpy array of squares.
    """
    return np.array(bitboard_to_square_list(bb), dtype=Square)
        
@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_to_string_list(bb: Bitboard) -> List[str]:
    """
    Returns the bitboard as a list of strings.
    """
    squares = bitboard_to_square_list(bb)
    return [square_to_str(sq) for sq in squares]

#: Transformation Functions
#:   bitboard_trans_fs (list): The set of functions used to transform the bitboard.
#:   bitboard_anti_trans_fs (list): The inverse functions used to map back to the original configuration.
bitboard_trans_fs = np.array(
    [bitboard_ro000,
     bitboard_ro090,
     bitboard_ro180,
     bitboard_ro270,
     bitboard_fvert,
     bitboard_fh1a8,
     bitboard_fhori,
     bitboard_fa1h8,
     ])

#: Anti Transformation Functions
#:   bitboard_anti_trans_fs (list): The inverse functions used to map back to the original configuration.
bitboard_anti_trans_fs = np.array(
    [bitboard_ro000,
     bitboard_ro270,
     bitboard_ro180,
     bitboard_ro090,
     bitboard_fvert,
     bitboard_fh1a8,
     bitboard_fhori,
     bitboard_fa1h8,
     ])

#: Transformation Labels
#:   bitboard_transformation_labels: The set of labels describing the transformations of the bitboard.
bitboard_transformation_labels = np.array(
    ['ro000',
     'ro090',
     'ro180',
     'ro270',
     'fvert',
     'fh1a8',
     'fhori',
     'fa1h8',
     ])

#: Anti Transformation Labels
#:   bitboard_anti_transformation_labels: The set of labels describing the inverse transformations of the bitboard.
bitboard_anti_transformation_labels = np.array(
    ['ro000',
     'ro270',
     'ro180',
     'ro090',
     'fvert',
     'fh1a8',
     'fhori',
     'fa1h8',
     ])



#: Position methods.

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_eq(pos_a: PositionField | PositionArray, pos_b: PositionField | PositionArray) -> bool:
    return np.array_equal(pos_a, pos_b)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_print(p: PositionField, output: Union[IO, io.StringIO] = sys.stdout) -> None:
    """
    Prints a 2D representation of the position to the given output.
    """
    sq = Bitboard(1)
    print('  a b c d e f g h', file=output)
    for row in range(0,8):
        print(row + 1, end = '', file=output)
        for col in range(0,8):
            if sq & p['mover'] != 0:
                c = '@'
            elif sq & p['opponent'] != 0:
                c = 'O'
            else:
                c = '.'
            print(' ' + c, end = '', file=output)
            sq <<= 1
        print(file=output)
    return

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_empties(p: PositionField | PositionArray) -> Bitboard | BitboardArray:
    """
    Returns the set of empty squares in the game position.
    Works on scalar and 1D array inputs.
    """
    return ~(p['mover'] | p['opponent'])

def position_legal_moves(p: PositionField) -> Bitboard:
    pass

def position_legal_moves_count(p: PositionField) -> int:
    pass

def position_flips(p: PositionField, move: Bitboard) -> (Bitboard, PositionField):
    pass

def position_make_move(p: PositionField, move: Bitboard) -> PositionField:
    pass

def position_count_difference(p: PositionField) -> int:
    pass

def position_final_value(p: PositionField) -> int:
    pass

def position_has_to_pass(p: PositionField) -> bool:
    pass

def position_is_game_over(p: PositionField) -> bool:
    pass

def position_is_move_legal(p: PositionField) -> bool:
    pass

def position_fa1h8(p: PositionField) -> PositionField:
    pass

def position_fh1a8(p: PositionField) -> PositionField:
    pass

def position_fhori(p: PositionField) -> PositionField:
    pass

def position_fvert(p: PositionField) -> PositionField:
    pass

def position_ro000(p: PositionField) -> PositionField:
    pass

def position_ro090(p: PositionField) -> PositionField:
    pass

def position_ro180(p: PositionField) -> PositionField:
    pass

def position_ro270(p: PositionField) -> PositionField:
    pass

def position_transformations(p: PositionField) -> PositionArray:
    pass

def position_anti_transformations(p: PositionField) -> PositionArray:
    pass
