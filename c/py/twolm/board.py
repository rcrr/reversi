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

from typing import TypeAlias, Annotated

import numpy as np
import numpy.typing as npt
from numba import njit, prange

import sys
import io
from typing import IO, Union

from pydantic import validate_call, ConfigDict, BeforeValidator


__all__ = ['Bitboard', 'bitboard_from_signed_int', 'bitboard_bsr', 'bitboard_count',
           'bitboard_transformations', 'bitboard_print',
           'bitboard_fa1h8', 'bitboard_fh1a8', 'bitboard_fhori', 'bitboard_fvert',
           'bitboard_ro000', 'bitboard_ro090', 'bitboard_ro180', 'bitboard_ro270']


Bitboard: TypeAlias = np.uint64

def validate_uint64_array(v: any) -> any:
    """Validator to ensure a numpy array strictly uses uint64 dtype."""
    if isinstance(v, np.ndarray) and v.dtype != np.uint64:
        raise ValueError(f"Array dtype must be uint64, got {v.dtype}")
    return v

# A robust Pydantic-compatible type hint for uint64 arrays
BitboardArray = Annotated[npt.NDArray[np.uint64], BeforeValidator(validate_uint64_array)]

def bitboard_from_signed_int(i: np.int64 | npt.NDArray[np.int64]) -> Bitboard | npt.NDArray[Bitboard]:
    if isinstance(i, np.ndarray) and i.dtype == np.int64:
        return i.view(np.uint64)
    elif isinstance(i, np.int64):
        return np.array(i).view(np.uint64)[()]
    else:
        raise TypeError(f"The argument i has the wrong type: {type(i)}")

def bitboard_bsr(bb: Bitboard | npt.NDArray[Bitboard]) -> np.int8 | npt.NDArray[np.int8]:
    """
    Bit Scan Reverse.
    Returns the index of the MOST significant set bit as np.int8 (scalar or array).
    """
    if isinstance(bb, np.ndarray) and bb.dtype == Bitboard:
        # Bit-smearing
        a = bb.copy()
        a |= a >> 1
        a |= a >> 2
        a |= a >> 4
        a |= a >> 8
        a |= a >> 16
        a |= a >> 32
        
        # Count bits, subtract 1, cast to int8
        result = np.bitwise_count(a).astype(np.int8) - np.int8(1)
        
        # Enforce -1 for zeros
        return np.where(bb == np.uint64(0), np.int8(-1), result)
        
    elif isinstance(bb, np.uint64):
        n = int(bb)
        if n == 0:
            return np.int8(-1)
        return np.int8(n.bit_length() - 1)
        
    else:
        raise TypeError(f"The argument bb must be Bitboard (np.uint64) or npt.NDArray[Bitboard], got {type(bb)}")

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def bitboard_count(bb: Bitboard) -> int:
    """
    Returns the number of squares in the set.

    Returns:
        int: The number of squares in the set.
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
    # 1. Standardize input by checking if it came in as a scalar or an array
    is_scalar = not isinstance(bb, np.ndarray)
    bb_arr = np.atleast_1d(bb)

    # 2. Check layout constraints (Pydantic validates types, but we handle dimensions)
    if bb_arr.ndim != 1:
        raise ValueError(f"When the argument bb is an array, it must be 1D, got shape {bb_arr.shape}")

    # 3. Allocate memory for output transformations matrix
    n = bb_arr.shape[0]
    ts = np.empty((8, n), dtype=Bitboard)

    # 4. Pre-calculate common intermediate horizontal reflection transformation
    fh = bitboard_fhori(bb_arr)

    # Fast row-wise contiguous assignments
    ts[0, :] = bb_arr
    ts[1, :] = bitboard_fa1h8(fh)
    ts[2, :] = bitboard_fvert(fh)
    ts[3, :] = bitboard_fh1a8(fh)
    ts[4, :] = bitboard_fvert(bb_arr)
    ts[5, :] = bitboard_fh1a8(bb_arr)
    ts[6, :] = fh
    ts[7, :] = bitboard_fa1h8(bb_arr)

    # Transpose back to (n, 8) at zero CPU cost
    ts_out = ts.T
    
    # 6. Adapt output representation to match original scalar or matrix input context
    if is_scalar:
        return ts_out.flatten().view(Bitboard)
        
    return ts_out.view(Bitboard)

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
