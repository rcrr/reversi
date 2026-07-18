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

from typing import TypeAlias, Annotated, List, Any, Tuple

import numpy as np
import numpy.typing as npt

import numba as nb
from numba import types
from numba.extending import overload

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
           'bitboard_transformations', 'bitboard_anti_transformations',  'bitboard_print',
           'bitboard_fa1h8', 'bitboard_fh1a8', 'bitboard_fhori', 'bitboard_fvert',
           'bitboard_ro000', 'bitboard_ro090', 'bitboard_ro180', 'bitboard_ro270',
           'bitboard_to_square_list', 'bitboard_to_square_array', 'bitboard_to_string_list',
           'bitboard_trans_fs', 'bitboard_anti_trans_fs',
           'bitboard_transformation_labels', 'bitboard_anti_transformation_labels',
           'Position', 'PositionField', 'PositionArray',
           'position_collisions', 'position_check_collisions',
           'make_position', 'position_eq', 'position_print', 'position_empties',
           'position_legal_moves', 'legal_moves', 'position_legal_moves_count',
           'position_flips', 'position_make_move', 'position_count_difference',
           'position_final_value', 'position_has_to_pass', 'position_is_game_over',
           'position_is_move_legal',
           'position_fa1h8', 'position_fh1a8', 'position_fhori', 'position_fvert',
           'position_ro000', 'position_ro090', 'position_ro180', 'position_ro270',
           'position_transformations', 'position_anti_transformations']



#: Represents a reversi board square indexed from 0 (A1) to 63 (H8) as a uint8 scalar.
Square: TypeAlias = np.uint8

def validate_square_array(v: any) -> any:
    """Validator to ensure a numpy array strictly uses uint8 dtype."""
    if isinstance(v, np.ndarray) and v.dtype != np.uint8:
        raise ValueError(f"Array dtype must be uint8, got {v.dtype}")
    return v

# A Pydantic-compatible type hint for Square (np.uint8) arrays.
SquareArray = Annotated[npt.NDArray[np.uint8], BeforeValidator(validate_square_array)]

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
def make_position(mover: Union[Bitboard, np.ndarray, List], 
                  opponent: Union[Bitboard, np.ndarray, List]
                  ) -> Union[np.void, np.ndarray]:
    """
    Creates a single Position scalar, a 1D PositionArray, or a 2D PositionArray.
    Guarantees zero-copy view casting on the combined data across all dimensionalities.
    """
    # ARRAY / LIST / MULTI-DIMENSIONAL CASE
    if isinstance(mover, (np.ndarray, list)) or isinstance(opponent, (np.ndarray, list)):
        # Convert to arrays to safely inspect shapes and sizes
        m_arr = np.asarray(mover, dtype=Bitboard)
        o_arr = np.asarray(opponent, dtype=Bitboard)
        
        # Enforce identical shape check across all dimensions
        if m_arr.shape != o_arr.shape:
            raise ValueError(
                f"Shape mismatch: 'mover' has shape {m_arr.shape}, "
                f"but 'opponent' has shape {o_arr.shape}."
            )
            
        # Capture the original structural shape (works for 1D, 2D, or higher)
        original_shape = m_arr.shape
        
        # Build the shape for the continuous memory block: append an axis of size 2 at the end
        # e.g., if original is (M, N), data_shape becomes (M, N, 2)
        data_shape = original_shape + (2,)
        
        # Allocate a contiguous C-memory block
        data = np.empty(data_shape, dtype=Bitboard)
        
        # Interleave the fields along the newly added trailing axis
        # Using ellipsis (...) dynamically handles any number of dimensions (1D, 2D, etc.)
        data[..., 0] = m_arr
        data[..., 1] = o_arr
        
        # Interpret the memory block as a structured array (Zero-Copy View)
        # The trailing dimension of size 2 collapses into the custom Position dtype
        return data.view(Position).reshape(original_shape)
        
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

    ts = _numba_bitboard_transformations(bb_arr)

    # Adapt output representation to match original scalar or matrix input context
    if is_scalar:
        return ts.flatten().view(Bitboard)
        
    return ts.view(Bitboard)

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_bitboard_transformations(bb_arr: BitboardArray) -> BitboardArray:
    
    # Allocate memory for output transformations matrix
    n = bb_arr.shape[0]
    ts = np.empty((n, 8), dtype=Bitboard)

    for i in nb.prange(n):

        value = bb_arr[i]
        fh = _numba_fhori_scalar_impl(value)

        ts[i, 0] = value
        ts[i, 1] = _numba_fa1h8_scalar_impl(fh)
        ts[i, 2] = _numba_fvert_scalar_impl(fh)
        ts[i, 3] = _numba_fh1a8_scalar_impl(fh)
        ts[i, 4] = _numba_fvert_scalar_impl(value)
        ts[i, 5] = _numba_fh1a8_scalar_impl(value)
        ts[i, 6] = fh
        ts[i, 7] = _numba_fa1h8_scalar_impl(value)
        
    return ts

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_anti_transformations(bb: Bitboard | BitboardArray) -> BitboardArray:
    """
    Applays the eight D8 anti-transformations on the input bitboards.
    
     - 0 -> 0 : ro000
     - 0 -> 1 : ro270
     - 0 -> 2 : ro180
     - 0 -> 3 : ro180
     - 0 -> 4 : fvert
     - 0 -> 5 : fh1a8
     - 0 -> 6 : fhori
     - 0 -> 7 : fa1h8

    When input is a scalar Bitboard output is an arry of Bitboards having shape (8,).
    When input is an array of Bitboards of lenght N, the output has shape (N, 8).
    
    Applies the anti-transformations by reversing index 1 and 3 of bitboard_transformations.
    Operates in-place on the generated matrix for maximum efficiency.
    """
    # Standardize input by checking if it came in as a scalar or an array
    is_scalar = not isinstance(bb, np.ndarray)
    bb_arr = np.atleast_1d(bb)

    # Check layout constraints (Pydantic validates types, but we handle dimensions)
    if bb_arr.ndim != 1:
        raise ValueError(f"When the argument bb is an array, it must be 1D, got shape {bb_arr.shape}")

    ts = _numba_bitboard_anti_transformations(bb_arr)

    # Adapt output representation to match original scalar or matrix input context
    if is_scalar:
        return ts.flatten().view(Bitboard)
        
    return ts.view(Bitboard)

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_bitboard_anti_transformations(bb_arr: BitboardArray) -> BitboardArray:
    
    # Allocate memory for output transformations matrix
    n = bb_arr.shape[0]
    ts = np.empty((n, 8), dtype=Bitboard)

    for i in nb.prange(n):

        value = bb_arr[i]
        fh = _numba_fhori_scalar_impl(value)

        ts[i, 0] = value
        ts[i, 1] = _numba_fh1a8_scalar_impl(fh)
        ts[i, 2] = _numba_fvert_scalar_impl(fh)
        ts[i, 3] = _numba_fa1h8_scalar_impl(fh)
        ts[i, 4] = _numba_fvert_scalar_impl(value)
        ts[i, 5] = _numba_fh1a8_scalar_impl(value)
        ts[i, 6] = fh
        ts[i, 7] = _numba_fa1h8_scalar_impl(value)
        
    return ts

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
    if isinstance(bb, np.ndarray):
        return _numba_fa1h8_array_impl(bb)
    else:
        return Bitboard(_numba_fa1h8_scalar_impl(bb))

@overload(bitboard_fa1h8)
def _numba_ol_bitboard_fa1h8(bb):
    if isinstance(bb, types.Array):
        return lambda bb: _numba_fa1h8_array_impl(bb)
    else:
        return lambda bb: _numba_fa1h8_scalar_impl(bb)

@nb.njit(inline='always')
def _numba_fa1h8_scalar_impl(s):
    k1 = Bitboard(0x5500550055005500)
    k2 = Bitboard(0x3333000033330000)
    k4 = Bitboard(0x0f0f0f0f00000000)    
    t =      k4 & (s ^ (s << 28))
    s = s ^       (t ^ (t >> 28))
    t =      k2 & (s ^ (s << 14))
    s = s ^       (t ^ (t >> 14))
    t =      k1 & (s ^ (s << 7))
    s = s ^       (t ^ (t >> 7))
    return s

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_fa1h8_array_impl(arr):
    N = arr.shape[0]
    out = np.empty(N, dtype=Bitboard)
    for i in nb.prange(N):
        out[i] = _numba_fa1h8_scalar_impl(arr[i])
    return out

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
    if isinstance(bb, np.ndarray):
        return _numba_fh1a8_array_impl(bb)
    else:
        return Bitboard(_numba_fh1a8_scalar_impl(bb))

@overload(bitboard_fh1a8)
def _numba_ol_bitboard_fh1a8(bb):
    if isinstance(bb, types.Array):
        return lambda bb: _numba_fh1a8_array_impl(bb)
    else:
        return lambda bb: _numba_fh1a8_scalar_impl(bb)

@nb.njit(inline='always')
def _numba_fh1a8_scalar_impl(s):    
    k1 = Bitboard(0xaa00aa00aa00aa00)
    k2 = Bitboard(0xcccc0000cccc0000)
    k4 = Bitboard(0xf0f0f0f00f0f0f0f)
    t =            s ^ (s << 36)
    s = s ^ (k4 & (t ^ (s >> 36)))
    t =      k2 & (s ^ (s << 18))
    s = s ^       (t ^ (t >> 18))
    t =      k1 & (s ^ (s << 9))
    s = s ^       (t ^ (t >> 9))
    return s

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_fh1a8_array_impl(arr):
    N = arr.shape[0]
    out = np.empty(N, dtype=Bitboard)
    for i in nb.prange(N):
        out[i] = _numba_fh1a8_scalar_impl(arr[i])
    return out

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
    if isinstance(bb, np.ndarray):
        return _numba_fhori_array_impl(bb)
    else:
        return Bitboard(_numba_fhori_scalar_impl(bb))

@overload(bitboard_fhori)
def _numba_ol_bitboard_fhori(bb):
    if isinstance(bb, types.Array):
        return lambda bb: _numba_fhori_array_impl(bb)
    else:
        return lambda bb: _numba_fhori_scalar_impl(bb)

@nb.njit(inline='always')
def _numba_fhori_scalar_impl(s):
    mask56 = Bitboard(0xFF00000000000000)
    mask48 = Bitboard(0x00FF000000000000)
    mask40 = Bitboard(0x0000FF0000000000)
    mask32 = Bitboard(0x000000FF00000000)
    mask24 = Bitboard(0x00000000FF000000)
    mask16 = Bitboard(0x0000000000FF0000)
    mask08 = Bitboard(0x000000000000FF00)
    mask00 = Bitboard(0x00000000000000FF)
    s = (((s << 56) & mask56) |
         ((s << 40) & mask48) |
         ((s << 24) & mask40) |
         ((s <<  8) & mask32) |
         ((s >>  8) & mask24) |
         ((s >> 24) & mask16) |
         ((s >> 40) & mask08) |
         ((s >> 56) & mask00))
    return s

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_fhori_array_impl(arr):
    N = arr.shape[0]
    out = np.empty(N, dtype=Bitboard)
    for i in nb.prange(N):
        out[i] = _numba_fhori_scalar_impl(arr[i])
    return out

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
    if isinstance(bb, np.ndarray):
        return _numba_fvert_array_impl(bb)
    else:
        return Bitboard(_numba_fvert_scalar_impl(bb))

@overload(bitboard_fvert)
def _numba_ol_bitboard_fvert(bb):
    if isinstance(bb, types.Array):
        return lambda bb: _numba_fvert_array_impl(bb)
    else:
        return lambda bb: _numba_fvert_scalar_impl(bb)

@nb.njit(inline='always')
def _numba_fvert_scalar_impl(s):
    k1 = Bitboard(0x5555555555555555)
    k2 = Bitboard(0x3333333333333333)
    k4 = Bitboard(0x0f0f0f0f0f0f0f0f)
    s = ((s >> 1) & k1) | ((s & k1) << 1)
    s = ((s >> 2) & k2) | ((s & k2) << 2)
    s = ((s >> 4) & k4) | ((s & k4) << 4)
    return s

@nb.njit(parallel=True, cache=True, fastmath=True)
def _numba_fvert_array_impl(arr):
    N = arr.shape[0]
    out = np.empty(N, dtype=Bitboard)
    for i in nb.prange(N):
        out[i] = _numba_fvert_scalar_impl(arr[i])
    return out

#: -------------------------------------

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
    
    return bitboard_fh1a8(bitboard_fa1h8(bb))

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

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_collisions(p: PositionField | PositionArray) -> Bitboard | BitboardArray:
    """
    Returns the set of overlapping squares between mover and opponent.
    Given the observation that squares cannot be taken by both players,
    it is an invariant that must be always zero (the empty set).
    Works on scalar and 1D array inputs.
    """
    return p['mover'] & p['opponent']

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_check_collisions(p: PositionField | PositionArray) -> None:
    """
    Checks for overlapping squares between mover and opponent.
    Given the observation that squares cannot be taken by both players,
    it is an invariant that must be always zero (the empty set).
    When a collision is found raises a ValueError.
    Works on scalar and 1D array inputs.
    """
    collisions = position_collisions(p)
    if np.any(collisions):
        # Convert to 1D safely to support both scalar and array inputs without IndexError
        collisions_1d = np.atleast_1d(collisions)
        
        total_size = collisions_1d.size
        nonzero_indices = np.nonzero(collisions_1d)[0]
        num_collisions = nonzero_indices.size  # Faster than len() on numpy arrays
        first_collision_idx = nonzero_indices[0]
        
        error_message = (
            f"Validation Failed: Detected {num_collisions} collision(s) "
            f"out of {total_size} total elements. "
            f"First collision occurred at index: {first_collision_idx}."
        )
        raise ValueError(error_message)

#: legal_moves code starts here.

#: Constants for bitboard manipulation
_all_squares                 = Bitboard(0xFFFFFFFFFFFFFFFF)
_all_squares_except_column_a = Bitboard(0xFEFEFEFEFEFEFEFE)
_all_squares_except_column_h = Bitboard(0x7F7F7F7F7F7F7F7F)

#: Pre-calculating masks and slides to avoid overhead inside the JIT function
_mask_l = np.array([
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares,
    _all_squares_except_column_a
], dtype=Bitboard)
_mask_r = np.array([
    _all_squares_except_column_h,
    _all_squares_except_column_a,
    _all_squares,
    _all_squares_except_column_h
], dtype=Bitboard)
_slide_1 = np.array([1, 7, 8, 9], dtype=np.uint8)
_slide_2 = _slide_1 * 2
_slide_4 = _slide_1 * 4

#: Internal use function.
@nb.njit(parallel=True, cache=True)
def _vectorized_legal_moves(movers: np.ndarray, opponents: np.ndarray) -> np.ndarray:
    """
    Vectorized calculation of Reversi legal moves using Kogge-Stone algorithm.
    Optimized for large arrays (10M+ elements) using Numba JIT and multi-threading.
    """
    n = movers.shape[0]
    results = np.empty(n, dtype=Bitboard)

    # Parallel loop across all board instances
    for i in nb.prange(n):
        mover = Bitboard(movers[i])
        opponent = Bitboard(opponents[i])
        empties = Bitboard(~(mover | opponent))
        
        legal_result = Bitboard(0)
        
        # Iterate through the 4 main directions (each handles both sides: Left/Right, Up/Down, etc.)
        for d in range(4):
            # --- Directional Forward Shift (Left-style) ---
            g = mover
            p = opponent & _mask_l[d]

            g |= p & (g << _slide_1[d])
            p &= p << _slide_1[d]
            g |= p & (g << _slide_2[d])
            p &= p << _slide_2[d]
            g |= p & (g << _slide_4[d])

            # Resulting moves must be in an empty square and not be the generator itself
            g_final = empties & _mask_l[d] & ((g & ~mover) << _slide_1[d])
            legal_result |= g_final

            # --- Directional Backward Shift (Right-style) ---
            g = mover
            p = opponent & _mask_r[d]

            g |= p & (g >> _slide_1[d])
            p &= p >> _slide_1[d]
            g |= p & (g >> _slide_2[d])
            p &= p >> _slide_2[d]
            g |= p & (g >> _slide_4[d])

            g_final = empties & _mask_r[d] & ((g & ~mover) >> _slide_1[d])
            legal_result |= g_final
            
        results[i] = legal_result
        
    return results

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def legal_moves(mover: Bitboard | BitboardArray,
                opponent: Bitboard | BitboardArray) -> Bitboard | BitboardArray:
    """
    Calculate Reversi legal moves for a single or multiple board configurations.
    """
    # Check if inputs are scalars
    if isinstance(mover, Bitboard) and isinstance(opponent, Bitboard):
        # Wrap the scalar inputs into numpy arrays
        movers = np.array([mover], dtype=Bitboard)
        opponents = np.array([opponent], dtype=Bitboard)
        
        # Call the vectorized function
        result = _vectorized_legal_moves(movers, opponents)
        
        # Return the result as a scalar
        return result[0]
    elif isinstance(mover, np.ndarray) and isinstance(opponent, np.ndarray):
        # Ensure the arrays have the same shape
        if mover.shape != opponent.shape:
            raise ValueError("The shapes of mover and opponent arrays must be the same.")
        
        # Ensure the dtype of the arrays is Bitboard
        if mover.dtype != Bitboard or opponent.dtype != Bitboard:
            raise TypeError("The dtype of mover and opponent arrays must be Bitboard.")
        
        # Call the vectorized function directly
        return _vectorized_legal_moves(mover, opponent)
    else:
        raise TypeError("Inputs must be either both Bitboard or both npt.NDArray[Bitboard].")

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_legal_moves(p: PositionField | PositionArray) -> Bitboard | BitboardArray:
    return legal_moves(p['mover'], p['opponent'])

#: legal_move code ends here.

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_legal_moves_count(p: PositionField) -> int:
    """
    Returns the count of legal moves for the game position.
    """
    lms = position_legal_moves(p)
    lmc = bitboard_count(lms)
    return lmc

#: make_move code starts here.
#:
#: make_move and flips functions are strictly scalar.
#: If we whould need to develop a version vectorized for numpy arrays
#: a few steps are required:
#:  - transform _kogge_stone_mm as done for _kogge_stone_lms
#:    split the loop on the 8 directions into two loops on 4 ...
#:    add the numba tag ... @njit(parallel=True, cache=True) ...
#:    consider to parallelize the kogge_stone code using prange ...
#:  - Find a way to check for PASS moves not as a special case ...
#:    if not possible, the PASS moves has to be filter out from the kogge_stone call

_mm_mask = np.array([
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares,
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares_except_column_a,
    _all_squares,
    _all_squares_except_column_h,
], dtype=Bitboard)

_mm_slide_up_1 = np.array([1,  7,  8,  9,  0,  0,  0,  0], dtype=np.uint8)
_mm_slide_dw_1 = np.array([0,  0,  0,  0,  1,  7,  8,  9], dtype=np.uint8)
_mm_slide_up_2 = np.array([2, 14, 16, 18,  0,  0,  0,  0], dtype=np.uint8)
_mm_slide_dw_2 = np.array([0,  0,  0,  0,  2, 14, 16, 18], dtype=np.uint8)
_mm_slide_up_4 = np.array([4, 28, 32, 36,  0,  0,  0,  0], dtype=np.uint8)
_mm_slide_dw_4 = np.array([0,  0,  0,  0,  4, 28, 32, 36], dtype=np.uint8)

def _kogge_stone_mm(generator: Bitboard,
                    propagator: Bitboard,
                    blocker: Bitboard) -> Bitboard:
    g: Bitboard
    p: Bitboard
    gt: Bitboard
    accumulator: Bitboard

    accumulator = Bitboard(0)

    for i in range(8):
        g = generator
        p = propagator & _mm_mask[i]

        g |= p & ((g << _mm_slide_up_1[i]) >> _mm_slide_dw_1[i])
        p &=     ((p << _mm_slide_up_1[i]) >> _mm_slide_dw_1[i])

        g |= p & ((g << _mm_slide_up_2[i]) >> _mm_slide_dw_2[i])
        p &=     ((p << _mm_slide_up_2[i]) >> _mm_slide_dw_2[i])

        # Flipped disks plus move, but only if this direction really flips.
        g |= p & ((g << _mm_slide_up_4[i]) >> _mm_slide_dw_4[i])

        # Flip blocker. It is the bracketing square belonging to mover.
        gt = blocker & _mm_mask[i] & ((g << _mm_slide_up_1[i]) >> _mm_slide_dw_1[i])

        # Accumulates flipped disks only if there is a blocker (true flipping direction).
        # In C: (g & -(gt > 0))
        if gt > 0:
            accumulator |= g

    return Bitboard(accumulator & ~generator)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_flips(p: PositionField, move: Bitboard) -> Tuple[Bitboard, PositionField]:
    """
    Computes the flip set.
    Return flips as well as the updated game position.
    The moving player places a disc in the square identified by the move parameter.
    The flipped squares are returned.
    When the move is not legal the empy set is returned.
    When move is 0ULL, 0ULL is returned.
    The updated board is also returned as second element of the tuple when the move is legal,
    None otherwise.
    """
    if bitboard_count(move) != 1:
        return Bitboard(0x0000000000000000), None
    m = p['mover']
    o = p['opponent']
    flips = _kogge_stone_mm(move, o, m)
    if flips != 0x0000000000000000:
        updated = make_position(o & ~flips, m | flips | move)
    else:
        updated = None
    return flips, updated

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_make_move(p: PositionField, move: Move) -> PositionField:
    """
    Executes a game move.
    The move must be in the range A1, ... H8, PA. [0..64].
    When 64 (PA) is passed the player and opponent bitboard are swapped, it is not verified
    that pass is the only legal move.
    When an illegal move is passed the same board is returned.
    """
    if move == 64: # PASS
        return make_position(p['opponent'], p['mover'])
    if move > 64:
        raise ValueError(f"Argument move {int(move)} is out of range [0..64]")
    m = move_as_bitboard(move)
    _, updated = position_flips(p, m)
    return updated

#: make_move code ends here.

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_count_difference(p: PositionField) -> int:
    """
    Returns the disk difference between the player and her opponent.
    """
    mc = bitboard_count(p['mover'])
    oc = bitboard_count(p['opponent'])
    return int(mc) - int(oc)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_final_value(p: PositionField) -> int:
    """
    Used for the score at the end of the game.
    Returns the disk difference between the player and her opponent,
    assigning the empty squares to the player having most discs.

    From the web site of the World Othello Federation,
    World Othello Chanpionship Rules, scoring:
    "At the end of the game, if both players have completed their moves in
    the allowed time, the winner is the player with the greater number of
    discs of his colour on the board at the end. The official score of the
    game will be determined by counting up the discs of each colour on the
    board, counting empty squares for the winner. In the event of a draw,
    the score will always be 32-32".
    """
    mc = bitboard_count(p['mover'])
    oc = bitboard_count(p['opponent'])
    diff = int(mc) - int(oc)
    if diff == 0: return 0
    ec = 64 - (mc + oc)
    if diff > 0:
        delta = diff + ec
    else:
        delta = diff - ec
    return delta

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_has_to_pass(p: PositionField) -> bool:
    """
    Returns true if the mover player does't have any legal move.
    """
    lmc = position_legal_moves_count(p)
    if lmc == 0:
        return True
    else:
        return False

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_is_game_over(p: PositionField) -> bool:
    """
    Returns true if the game is over.
    """
    lmc = position_legal_moves_count(p)
    if lmc > 0: return False
    next_board = position_make_move(p, move_from_str('PA'))
    next_lmc = position_legal_moves_count(next_board)
    if next_lmc > 0: return False
    return True

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_is_move_legal(p: PositionField, move: Move) -> bool:
    """
    Returns true if the move is legal.
    """
    if move > 64: return False
    lms = position_legal_moves(p)
    mbb = move_as_bitboard(move)
    if mbb == 0x0000000000000000: # Pass move
        if mbb == lms:
            return True
        else:
            return False
    if (mbb & lms) == mbb:
        return True
    return False

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_fa1h8(p: PositionField) -> PositionField:
    """
    Reflects the game position on the diagonal a1-h8.
    """
    return make_position(bitboard_fa1h8(p['mover']), bitboard_fa1h8(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_fh1a8(p: PositionField) -> PositionField:
    """
    Reflects the game position on the diagonal h1-a8.
    """
    return make_position(bitboard_fh1a8(p['mover']), bitboard_fh1a8(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_fhori(p: PositionField) -> PositionField:
    """
    Reflects the game position horizontally (on the horizontal axis).
    """
    return make_position(bitboard_fhori(p['mover']), bitboard_fhori(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_fvert(p: PositionField) -> PositionField:
    """
    Reflects the game position vertically (on the vertical axis).
    """
    return make_position(bitboard_fvert(p['mover']), bitboard_fvert(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_ro000(p: PositionField) -> PositionField:
    """
    Returns the game position unchanged, as it is.
    """
    return p.copy()

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_ro090(p: PositionField) -> PositionField:
    """
    Rotates the game position by 90 degrees clockwise.
    """
    return make_position(bitboard_ro090(p['mover']), bitboard_ro090(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_ro180(p: PositionField) -> PositionField:
    """
    Rotates the game position by 180 degrees.
    """
    return make_position(bitboard_ro180(p['mover']), bitboard_ro180(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_ro270(p: PositionField) -> PositionField:
    """
    Rotates the game position by 270 degrees clockwise.
    """
    return make_position(bitboard_ro270(p['mover']), bitboard_ro270(p['opponent']))

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_transformations(p: PositionField | PositionArray) -> PositionArray:
    """
    Applays the eight D8 transformations on the input game positions.
    
     - 0 -> 0 : ro000
     - 0 -> 1 : ro090
     - 0 -> 2 : ro180
     - 0 -> 3 : ro270
     - 0 -> 4 : fvert
     - 0 -> 5 : fh1a8
     - 0 -> 6 : fhori
     - 0 -> 7 : fa1h8

    When input is a scalar Position output is an arry of Positions having shape (8,).
    When input is an array of Positions of lenght N, the output has shape (N, 8).
    """
    mtrs = bitboard_transformations(p['mover'])
    otrs = bitboard_transformations(p['opponent'])
    return make_position(mtrs, otrs)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def position_anti_transformations(p: PositionField | PositionArray) -> PositionArray:
    """
    Applays the eight D8 anti-transformations on the input game positions.
    
     - 0 -> 0 : ro000
     - 0 -> 1 : ro270
     - 0 -> 2 : ro180
     - 0 -> 3 : ro090
     - 0 -> 4 : fvert
     - 0 -> 5 : fh1a8
     - 0 -> 6 : fhori
     - 0 -> 7 : fa1h8

    When input is a scalar Position output is an arry of Positions having shape (8,).
    When input is an array of Positions of lenght N, the output has shape (N, 8).
    """
    mtrs = bitboard_anti_transformations(p['mover'])
    otrs = bitboard_anti_transformations(p['opponent'])
    return make_position(mtrs, otrs)
