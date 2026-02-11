#
# domain.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2025, 2026 Roberto Corradini. All rights reserved.
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
# To use the module interactively do:
#
# $ cd $(REVERSI_HOME)/c
# $ source py/.reversi_venv/bin/activate
# $ PYTHONPATH="./py" python3
# >>> from twoml.domain import *
# >>> ar = SquareSet(0x22120a0e1222221e)
# >>> ar
# SquareSet(2455035802420388382)
# >>> ar.print()
#   a b c d e f g h
# 1 . x x x x . . .
# 2 . x . . . x . .
# 3 . x . . . x . .
# 4 . x . . x . . .
# 5 . x x x . . . .
# 6 . x . x . . . .
# 7 . x . . x . . .
# 8 . x . . . x . .
#

from __future__ import annotations

import numpy as np

from typing import Self, Callable, Any, Union, TypeVar

__all__ = ['Square', 'Move', 'SquareSet', 'Board', 'Pattern', 'pack_ss', 'unpack_ss']

class Square(np.uint8):
    """
    Squares are the 64 positions on the board (A1-H8)
    """

    __names__ = [
        'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
        'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
        'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
        'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
        'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5',
        'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6',
        'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7',
        'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'H8'
    ]

    __max_value__ = len(__names__)
    
    @classmethod
    def new_from_str(cls: type[Self], name: str) -> Self:
        try:
            index = cls.__names__.index(name)
            return cls(index)
        except ValueError:
            raise ValueError(f"Invalid name: '{name}'. Must be one of {cls.__names__}")

    def __new__(cls: type[Self], value: np.uint8) -> Self:
        assert 0 <= value < cls.__max_value__, f"Value must be in the range [0..{cls.__max_value__}]"
        return super().__new__(cls, value)

    def to_str(self) -> str:
        return self.__names__[self]

    def as_square_set(self) -> SquareSet:
        return SquareSet(np.uint64(1) << self)

class Move(Square):
    """
    Moves are the actions during the game.
    On top of the 64 moves corresponding to fill a corresponding square with a disk,
    a few more are defined:
    - PA : Pass - It is still a well defined by the game rule action
    - NA : Not available - Used as a flag status by the program
    - UN : Unknown - Used when for instance it is unknown the parent move of a given game position
    """
    
    __names__ = Square.__names__ + ['PA', 'NA', 'UN']
    __max_value__ = len(__names__)
    

class SquareSet(np.uint64):
    """
    A set of squares.

    It is implemented as a bit-board of 64 bit using numpy.uint64 as parent type.
    """

    transformation_labels = np.array(
        ['trans_identity',
         'trans_rotate_90c',
         'trans_rotate_180',
         'trans_rotate_90a',
         'trans_reflection_vertical',
         'trans_reflection_diag_h1a8',
         'trans_reflection_horizontal',
         'trans_reflection_diag_a1h8',
         ])

    anti_transformation_labels = np.array(
        ['trans_identity',
         'trans_rotate_90a',
         'trans_rotate_180',
         'trans_rotate_90c',
         'trans_reflection_vertical',
         'trans_reflection_diag_h1a8',
         'trans_reflection_horizontal',
         'trans_reflection_diag_a1h8',
         ])

    @classmethod
    def new_from_signed_int(cls, i: np.int64) -> Self:
        """
        Returns a new SquareSet object from a numpy.int64 value given as argument.

        Example:
          i = numpy.int64(-1)
          s = SquareSet.new_from_signed_int(i)
        """
        if not isinstance(i, np.int64):
            raise TypeError('Argument i is not an instance of numpy.int64')
        return SquareSet(np.uint64(i))

    @classmethod
    def new_from_hex(cls, h: str) -> SquareSet:
        """
        Returns a new SquareSet object from the hexadecimal string given as argument.
        The h string must be 16 character long having values in the range [0..F].
        Characters could be uppercase or lovercase.
        Note that the argument string is not prefixed with '0x'.

        Example:
          s = SquareSet.new_from_hex('ffffffffffffffff')
        """
        if not isinstance(h, str):
            raise TypeError('Argument h is not an instance of str')
        if not len(h) == 16:
            raise ValueError('Argument h must be a string of lenght 16')
        i = int(h, 16)
        return SquareSet(i)

    def clone(self) -> SquareSet:
        """
        Clones the square set.
        """
        return SquareSet(self)

    def to_signed_int(self) -> np.int64:
        """
        Returns the signed int 64 bit long representation of the square set.
        It is useful to store and retrieve the value from PostgreSQL databases.
        """
        return np.int64(self)

    def bsr(self) -> int:
        """
        Bit Scan Reverse - returns index of MOST significant set bit (63-0) or -1 if zero.
        """
        n = int(self)
        if n == 0:
            return -1
        return n.bit_length() - 1

    def to_square_list(self) -> List[Square]:
        """
        Returns the square set represented as a list of squares.
        Squares are ordered from larger (H8) to smaller (A1)
        """
        if self == 0:
            return []

        s = self
        positions = []
        while s:
            sq = s.bsr()
            positions.append(Square(sq))
            s = SquareSet(s ^ (np.uint64(1) << sq))
    
        return positions

    def to_square_array(self) -> np.ndarray:
        """
        Returns the square set as a numpy array.
        """
        return np.array(self.to_square_list())

    def to_string_list(self) -> List[str]:
        """
        Returns the square set as a list of strings.
        """
        squares = self.to_square_list()
        return [sq.to_str() for sq in squares]
    
    def count(self) -> int:
        """
        Returns the number of squares in the set.
        """
        bit_count = np.bitwise_count(self)
        return bit_count
    
    def print(self):
        """
        Prints on stdout a 2D representation of the square set.
        """
        one = SquareSet(1)
        sq = SquareSet(1)
        print('  a b c d e f g h')
        for row in range(0,8):
            print(row + 1, end = '')
            for col in range(0,8):
                if sq & self != 0:
                    c = 'x'
                else:
                    c = '.'
                print(' ' + c, end = '')
                sq <<= one
            print()
        return

    def trans_reflection_diag_a1h8(self) -> Self:
        """
        Reflects a square set on the diagonal a1-h8.
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
        k1: SquareSet = 0x5500550055005500
        k2: SquareSet = 0x3333000033330000
        k4: SquareSet = 0x0f0f0f0f00000000
        s = self
        t =      k4 & (s ^ (s << 28))
        s = s ^       (t ^ (t >> 28))
        t =      k2 & (s ^ (s << 14))
        s = s ^       (t ^ (t >> 14))
        t =      k1 & (s ^ (s << 7))
        s = s ^       (t ^ (t >> 7))
        return SquareSet(s)

    def trans_reflection_diag_h1a8(self) -> Self:
        """
        Reflects a square set on the diagonal h1-a8.
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
        k1: SquareSet = 0xaa00aa00aa00aa00
        k2: SquareSet = 0xcccc0000cccc0000
        k4: SquareSet = 0xf0f0f0f00f0f0f0f
        s = self
        t =            s ^ (s << 36)
        s = s ^ (k4 & (t ^ (s >> 36)))
        t =      k2 & (s ^ (s << 18))
        s = s ^       (t ^ (t >> 18))
        t =      k1 & (s ^ (s << 9))
        s = s ^       (t ^ (t >> 9))
        return SquareSet(s)

    def trans_reflection_horizontal(self) -> Self:
        """
        Reflects the square set horizontally (on the horizontal axis).
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
        mask56: SquareSet = 0xFF00000000000000
        mask48: SquareSet = 0x00FF000000000000
        mask40: SquareSet = 0x0000FF0000000000
        mask32: SquareSet = 0x000000FF00000000
        mask24: SquareSet = 0x00000000FF000000
        mask16: SquareSet = 0x0000000000FF0000
        mask08: SquareSet = 0x000000000000FF00
        mask00: SquareSet = 0x00000000000000FF
        s = self
        s = (((s << 56) & mask56) |
             ((s << 40) & mask48) |
             ((s << 24) & mask40) |
             ((s <<  8) & mask32) |
             ((s >>  8) & mask24) |
             ((s >> 24) & mask16) |
             ((s >> 40) & mask08) |
             ((s >> 56) & mask00))
        return SquareSet(s)

    def trans_reflection_vertical(self) -> Self:
        """
        Reflects the square set vertically (on the vertical axis).
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
        k1: SquareSet = 0x5555555555555555
        k2: SquareSet = 0x3333333333333333
        k4: SquareSet = 0x0f0f0f0f0f0f0f0f
        s = self
        s = ((s >> 1) & k1) | ((s & k1) << 1)
        s = ((s >> 2) & k2) | ((s & k2) << 2)
        s = ((s >> 4) & k4) | ((s & k4) << 4)
        return  SquareSet(s)

    def trans_identity(self) -> Self:
        """
        Returns the square set unchanged, as it is.
        Conceptually it applies a rotation of zero degrees.
        
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
        return self

    def trans_rotate_180(self) -> Self:
        """
        Rotates the square set by 180 degrees.
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
        return self.trans_reflection_horizontal().trans_reflection_vertical()

    def trans_rotate_90c(self) -> Self:
        """
        Rotates a square set by 90 degrees clockwise.
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
        return self.trans_reflection_diag_h1a8().trans_reflection_horizontal()

    def trans_rotate_90a(self) -> Self:
        """
        Rotates a square set by 90 degrees anticlockwise.
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
        return self.trans_reflection_horizontal().trans_reflection_diag_h1a8()

    def transformations(self) -> np.ndarray:
        """
        Returns eight square set as an array by transforming the set as follow.
         - 0 -> 0 : trans_identity
         - 0 -> 1 : trans_rotate_90c
         - 0 -> 2 : trans_rotate_180
         - 0 -> 3 : trans_rotate_90a
         - 0 -> 4 : trans_reflection_vertical
         - 0 -> 5 : trans_reflection_diag_h1a8
         - 0 -> 6 : trans_reflection_horizontal
         - 0 -> 7 : trans_reflection_diag_a1h8
        """
        # ts: transformed square sets
        ts = np.zeros(8, dtype=SquareSet)

        h1a8 = self.trans_reflection_diag_h1a8()
        fh = self.trans_reflection_horizontal()

        # - 0 -> 0 : trans_identity
        ts[0] = self

        # - 0 -> 1 : trans_rotate_90c
        ts[1] = h1a8.trans_reflection_horizontal()

        # - 0 -> 2 : trans_rotate_180
        ts[2] = fh.trans_reflection_vertical()

        # - 0 -> 3 : trans_rotate_90a
        ts[3] = fh.trans_reflection_diag_h1a8()

        # - 0 -> 4 : trans_reflection_vertical
        ts[4] = self.trans_reflection_vertical()

        # - 0 -> 5 : trans_reflection_diag_h1a8
        ts[5] = h1a8

        # - 0 -> 6 : trans_reflection_horizontal
        ts[6] = fh

        # - 0 -> 7 : trans_reflection_diag_a1h8
        ts[7] = self.trans_reflection_diag_a1h8()

        return ts

    def anti_transformations(self):
        """
        Returns eight square set as an array by transforming the set as follow.
         - 0 -> 0 : trans_identity
         - 0 -> 1 : trans_rotate_90a
         - 0 -> 2 : trans_rotate_180
         - 0 -> 3 : trans_rotate_90c
         - 0 -> 4 : trans_reflection_vertical
         - 0 -> 5 : trans_reflection_diag_h1a8
         - 0 -> 6 : trans_reflection_horizontal
         - 0 -> 7 : trans_reflection_diag_a1h8
        These are the anti-transformations as defined by the transformations() method.
        """
        # ts: transformed square sets
        ts = np.zeros(8, dtype=SquareSet)

        h1a8 = self.trans_reflection_diag_h1a8()
        fh = self.trans_reflection_horizontal()

        # - 0 -> 0 : trans_identity
        ts[0] = self
        
        # - 0 -> 1 : trans_rotate_90a
        ts[1] = fh.trans_reflection_diag_h1a8()

        # - 0 -> 2 : trans_rotate_180
        ts[2] = fh.trans_reflection_vertical()

        # - 0 -> 3 : trans_rotate_90c
        ts[3] = h1a8.trans_reflection_horizontal()

        # - 0 -> 4 : trans_reflection_vertical
        ts[4] = self.trans_reflection_vertical()

        # - 0 -> 5 : trans_reflection_diag_h1a8
        ts[5] = h1a8

        # - 0 -> 6 : trans_reflection_horizontal
        ts[6] = fh

        # - 0 -> 7 : trans_reflection_diag_a1h8
        ts[7] = self.trans_reflection_diag_a1h8()
        
        return ts

    trans_fs = np.array(
        [trans_identity,
         trans_rotate_90c,
         trans_rotate_180,
         trans_rotate_90a,
         trans_reflection_vertical,
         trans_reflection_diag_h1a8,
         trans_reflection_horizontal,
         trans_reflection_diag_a1h8,
         ])

    anti_trans_fs = np.array(
        [trans_identity,
         trans_rotate_90a,
         trans_rotate_180,
         trans_rotate_90c,
         trans_reflection_vertical,
         trans_reflection_diag_h1a8,
         trans_reflection_horizontal,
         trans_reflection_diag_a1h8,
         ])


# kogge-stone functions.

_all_squares:                 SquareSet = SquareSet(0xFFFFFFFFFFFFFFFF)
_all_squares_except_column_a: SquareSet = SquareSet(0xFEFEFEFEFEFEFEFE)
_all_squares_except_column_h: SquareSet = SquareSet(0x7F7F7F7F7F7F7F7F)

_mask_l = np.array([
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares,
    _all_squares_except_column_a
], dtype=SquareSet)

_mask_r = np.array([
    _all_squares_except_column_h,
    _all_squares_except_column_a,
    _all_squares,
    _all_squares_except_column_h
], dtype=SquareSet)

_slide_1 = np.array([1, 7, 8, 9], dtype=np.uint8)
_slide_2 = _slide_1 * 2
_slide_4 = _slide_1 * 4

def _kogge_stone_lms(generator: SquareSet,
                     propagator: SquareSet,
                     blocker: SquareSet) -> SquareSet:
    
    result = SquareSet(0)

    for i in range(4):
        g = generator
        p = propagator & _mask_l[i]

        g |= p & (g << _slide_1[i])
        p &= p << _slide_1[i]

        g |= p & (g << _slide_2[i])
        p &= p << _slide_2[i]

        g |= p & (g << _slide_4[i])

        g &= ~generator
        g = blocker & _mask_l[i] & (g << _slide_1[i])

        result |= g

        g = generator
        p = propagator & _mask_r[i]

        g |= p & (g >> _slide_1[i])
        p &= p >> _slide_1[i]

        g |= p & (g >> _slide_2[i])
        p &= p >> _slide_2[i]

        g |= p & (g >> _slide_4[i])

        g &= ~generator
        g = blocker & _mask_r[i] & (g >> _slide_1[i])

        result |= g

    return SquareSet(result)

_mask = np.array([
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares,
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares_except_column_a,
    _all_squares,
    _all_squares_except_column_h,
], dtype=SquareSet)

_slide_up_1 = np.array([1,  7,  8,  9,  0,  0,  0,  0], dtype=np.uint8)
_slide_dw_1 = np.array([0,  0,  0,  0,  1,  7,  8,  9], dtype=np.uint8)
_slide_up_2 = np.array([2, 14, 16, 18,  0,  0,  0,  0], dtype=np.uint8)
_slide_dw_2 = np.array([0,  0,  0,  0,  2, 14, 16, 18], dtype=np.uint8)
_slide_up_4 = np.array([4, 28, 32, 36,  0,  0,  0,  0], dtype=np.uint8)
_slide_dw_4 = np.array([0,  0,  0,  0,  4, 28, 32, 36], dtype=np.uint8)

def _kogge_stone_mm(generator: SquareSet,
                    propagator: SquareSet,
                    blocker: SquareSet) -> SquareSet:
    g: SquareSet
    p: SquareSet
    gt: SquareSet
    accumulator: SquareSet

    accumulator = SquareSet(0)

    for i in range(8):
        g = generator
        p = propagator & _mask[i]

        g |= p & ((g << _slide_up_1[i]) >> _slide_dw_1[i])
        p &=     ((p << _slide_up_1[i]) >> _slide_dw_1[i])

        g |= p & ((g << _slide_up_2[i]) >> _slide_dw_2[i])
        p &=     ((p << _slide_up_2[i]) >> _slide_dw_2[i])

        # Flipped disks plus move, but only if this direction really flips.
        g |= p & ((g << _slide_up_4[i]) >> _slide_dw_4[i])

        # Flip blocker. It is the bracketing square belonging to mover.
        gt = blocker & _mask[i] & ((g << _slide_up_1[i]) >> _slide_dw_1[i])

        # Accumulates flipped disks only if there is a blocker (true flipping direction).
        # In C: (g & -(gt > 0))
        if gt > 0:
            accumulator |= g

    return SquareSet(accumulator & ~generator)


# End of kpgge-stone algos.


class Board:

    transformation_labels = SquareSet.transformation_labels
    anti_transformation_labels = SquareSet.anti_transformation_labels

    def __init__(self, mover: SquareSet, opponent: SquareSet):
        if not isinstance(mover, (SquareSet, np.uint64)):
            raise TypeError('Argument mover is not an instance of SquareSet')
        if not isinstance(opponent, (SquareSet, np.uint64)):
            raise TypeError('Argument opponent is not an instance of SquareSet')
        if mover & opponent != SquareSet(0):
            raise ValueError('Arguments mover and opponent have overlapping squares')
        self.mover = mover
        self.opponent = opponent
        self._lms_ = None # legal moves set
        self._lmc_ = None # legal move count

    def __eq__(self, other):
        """
        Two boards are equals when mover and opponent fields are equal.
        """
        if isinstance(other, Board):
            return self.mover == other.mover and self.opponent == other.opponent
        return False

    def clone(self) -> Board:
        """
        Clones the board.
        """
        c = Board(self.mover, self.opponent)
        c._lms_ = self._lms_
        c._lmc_ = self._lmc_
        return c

    def print(self):
        """
        Prints on stdout a 2D representation of the board.
        Symbol @ identifies the mover, symbol O the opponent.
        """
        one = SquareSet(1)
        sq = SquareSet(1)
        print('  a b c d e f g h')
        for row in range(0,8):
            print(row + 1, end = '')
            for col in range(0,8):
                if sq & self.mover != 0:
                    c = '@'
                elif sq & self.opponent != 0:
                    c = 'O'
                else:
                    c = '.'
                print(' ' + c, end = '')
                sq <<= one
            print()
        return

    def empties(self) -> SquareSet:
        """
        Returns the set of empty squares in the game position.
        """
        return ~(self.mover | self.opponent)

    def get_mover(self) -> SquareSet:
        """
        Returns the set of squares belonging to the mover player.
        """
        return self.mover

    def get_opponent(self) -> SquareSet:
        """
        Returns the set of squares belonging to the opponent player.
        """
        return self.opponent

    def legal_moves(self) -> SquareSet:
        """
        Returns the set of squares that represents the legal moves for the game position.
        """
        if self._lms_ is None:
            self._lms_ = _kogge_stone_lms(self.mover, self.opponent, self.empties())
            self._lmc_ = self._lms_.count()
        return self._lms_

    def legal_moves_count(self) -> int:
        """
        Returns the count of legal moves for the game position.
        """
        if self._lmc_ is None:
            lms = self.legal_moves()
            self._lmc_ = lms.count()
        return self._lmc_

    def flips(self, move: SquareSet) -> (SquareSet, Board):
        """
        Computes the flip set.
        Return flips as well as the updated board.
        The moving player places a disc in the square identified by the move parameter.
        The flipped squares are returned.
        When the move is not legal the empy set is returned.
        When move is 0ULL, 0ULL is returned.
        The updated board is also returned as second element of the tuple when the move is legal,
        None otherwise.
        """
        if not isinstance(move, SquareSet):
            raise TypeError('Argument move is not an instance of SquareSet')
        n = move.count()
        if n != 1:
            return SquareSet(0x0000000000000000), None
        flips = _kogge_stone_mm(move, self.opponent, self.mover)
        if flips != 0x0000000000000000:
            updated = Board(self.opponent & ~flips, self.mover | flips | move)
        else:
            updated = None
        return flips, updated

    def make_move(self, move: Move) -> Board:
        """
        Executes a game move.
        The move must be in the range A1, ... H8, PA. [0..64].
        When 64 (PA) is passed the player and opponent bitboard are swapped, it is not verified
        that pass is the only legal move.
        When an illegal move is passed the same board is returned.
        """
        if not isinstance(move, Move):
            raise TypeError('Argument move is not an instance of Move')
        if move == 64: # PASS
            return Board(self.opponent, self.mover)
        if move > 64:
            raise ValueError('Argument move {} is out of range [0..64]'.format(move))
        flips :SquareSet
        updated :Board
        m: SquareSet = SquareSet(0x0000000000000001 << move)
        _, updated = self.flips(m)
        return updated

    def count_difference(self) -> int:
        """
        Returns the disk difference between the player and her opponent.
        """
        mc = self.mover.count()
        oc = self.opponent.count()
        return mc - oc

    def final_value(self) -> int:
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
        mc = int(self.mover.count())
        oc = int(self.opponent.count())
        diff = mc - oc
        if diff == 0: return 0
        ec = 64 - (mc + oc)
        if diff > 0:
            delta = diff + ec
        else:
            delta = diff - ec
        return delta

    def has_to_pass(self) -> bool:
        """
        Returns true if the mover player does't have any legal move.
        """
        lmc = self.legal_moves_count()
        if lmc == 0:
            return True
        else:
            return False

    def is_game_over(self) -> bool:
        """
        Returns true if the game is over.
        """
        lmc = self.legal_moves_count()
        if lmc > 0: return False
        next_board = self.make_move(Move.new_from_str('PA'))
        next_lmc = next_board.legal_moves_count()
        if next_lmc > 0: return False
        return True

    def is_move_legal(self, move: Move) -> bool:
        """
        Returns true if the move is legal.
        """
        if not isinstance(move, Move):
            raise TypeError('Argument move is not an instance of Move')
        if move > 64: return False
        lms = self.legal_moves()
        mss = move.as_square_set()
        if mss == 0x0000000000000000 and mss == lms: # Pass move
            return True
        if (mss & lms) == mss:
            return True
        return False

    def trans_reflection_diag_a1h8(self) -> Board:
        """
        Reflects the board on the diagonal a1-h8.
        """
        return Board(self.mover.trans_reflection_diag_a1h8(),
                     self.opponent.trans_reflection_diag_a1h8())

    def trans_reflection_diag_h1a8(self) -> Board:
        """
        Reflects the board on the diagonal h1-a8.
        """
        return Board(self.mover.trans_reflection_diag_h1a8(),
                     self.opponent.trans_reflection_diag_h1a8())

    def trans_reflection_horizontal(self) -> Board:
        """
        Reflects the board horizontally (on the horizontal axis).
        """
        return Board(self.mover.trans_reflection_horizontal(),
                     self.opponent.trans_reflection_horizontal())

    def trans_reflection_vertical(self) -> Board:
        """
        Reflects the board vertically (on the vertical axis).
        """
        return Board(self.mover.trans_reflection_vertical(),
                     self.opponent.trans_reflection_vertical())

    def trans_identity(self) -> Board:
        """
        Returns the board unchanged, as it is.
        """
        return self.clone()

    def trans_rotate_180(self) -> Board:
        """
        Rotates the board by 180 degrees.
        """
        return Board(self.mover.trans_rotate_180(),
                     self.opponent.trans_rotate_180())

    def trans_rotate_90c(self) -> Board:
        """
        Rotates the board by 90 degrees clockwise.
        """
        return Board(self.mover.trans_rotate_90c(),
                     self.opponent.trans_rotate_90c())

    def trans_rotate_90a(self) -> Board:
        """
        Rotates the board by 90 degrees anticlockwise.
        """
        return Board(self.mover.trans_rotate_90a(),
                     self.opponent.trans_rotate_90a())

    def transformations(self) -> np.ndarray:
        """
        Returns an array of transformed boards.
        See the transformation() method defined by SquareSet.
        """
        tm = self.mover.transformations()
        to = self.opponent.transformations()
        return np.array([Board(SquareSet(m), SquareSet(o)) for m, o in zip(tm, to)])
    
    def anti_transformations(self) -> np.ndarray:
        """
        Returns an array of anti-transformed boards.
        See the anti_transformation() method defined by SquareSet.
        """
        tm = self.mover.anti_transformations()
        to = self.opponent.anti_transformations()
        return np.array([Board(SquareSet(m), SquareSet(o)) for m, o in zip(tm, to)])


class Pattern:
    """
    The Pattern class represents a specific spatial configuration of squares (a "pattern") on a Reversi bitboard.
    It encapsulates the logic for bit manipulation, symmetry detection, and canonical form validation,
    which are essential for building Pattern Databases (PDB) or training evaluation functions.

    Attributes
    
    1. Basic Metadata
    name (str): A human-readable label for the pattern (e.g., "EDG8", "DIAG8", "CORNER").
    mask (SquareSet): The primary 64-bit mask defining the squares belonging to this pattern.

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
    unique_mask_indexes (list): The indices of the transformations that produce unique masks.
    n_instances (int): The number of unique physical locations/orientations this pattern occupies (cardinality of the orbit |Orb|).
    n_stabilizer (int): The number of transformations that leave the mask bitwise identical (order of the stabilizer |Stab|).

    4. Transformation Functions
    trans_fs (list): The set of functions used to transform the pattern squares.
    anti_trans_fs (list): The inverse functions used to map transformed squares back to the original configuration.

    5. Symmetry & Automorphism Logic
    unique_symmetric_instance_indexes (list): Indices of transformations that leave the mask bitwise identical but might permute the internal order of the squares.
                                              These represent the Automorphism Group of the pattern.
    symmetry_fs (list): The specific transformation functions that result in a bitwise identical mask, used to reduce parameters in the regression model or neural network.

    6. Optimization Plans
    pack_plan (list): A precomputed sequence of bitwise operations (masks and shifts) used to "pack" the scattered bits of a 64-bit board into a contiguous integer index.
                      This is critical for high-speed evaluation.
                      Detailed breakdown of each tuple:
                      np.uint64: The first element of the tuple is the block_mask.
                      int: The second element is the shift_amount.
                           It is the result of the subtraction of (source_pos - dest_pos), it defaults to the standard Python int type.
    """

    def __init__(self, name: str, mask: SquareSet):
        if not isinstance(mask, (SquareSet, np.uint64)):
            raise TypeError('Argument mask is not an instance of SquareSet')
        if not isinstance(name, str):
            raise TypeError('Argument name is not an instance of str')

        self.name = name
        self.mask = mask
        self.n_squares = mask.count()
        self.n_configurations = 3 ** int(self.n_squares)
        self.tmasks = mask.transformations()
        self.squares = mask.to_square_list()[::-1]
        self.snames = mask.to_string_list()[::-1]
        self.unique_masks = list(dict.fromkeys(self.tmasks))
        self.mask_to_index = {m: i for i, m in reversed(list(enumerate(self.tmasks)))}
        self.mask_indexes = np.zeros(8, dtype=np.uint8)
        for i, tmask in enumerate(self.tmasks):
            self.mask_indexes[i] = self.mask_to_index[tmask]
        self.unique_mask_indexes = list(dict.fromkeys(self.mask_indexes))
        # n_instances corresponds to the cardinality of the orbit |Orb(P)|
        self.n_instances = len(self.unique_masks)
        # n_stabilizer corresponds to the order of the stabilizer |Stab(P)|
        self.n_stabilizer = 8 // self.n_instances
        self.trans_fs = [SquareSet.trans_fs[i] for i in self.unique_mask_indexes]
        self.anti_trans_fs = [SquareSet.anti_trans_fs[i] for i in self.unique_mask_indexes]

        def _check_mask_is_principal() -> int:
            instance: int = 0
            highest_cell_instance_0 = SquareSet(self.tmasks[0]).bsr()
            for i in range(1, 8):
                highest_cell = SquareSet(self.tmasks[i]).bsr()
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
        
        def _precompute_pack_plan() -> list[tuple[np.uint64, int]]:
            empty = np.uint64(0)
            plan = []
            mask = np.uint64(self.mask)
            dest_pos = 0
            source_pos = 0
            
            while source_pos < 64:
                # 1. Skip zeros to find the start of the next block
                # We use a bit selector to stay in the 64-bit domain
                while source_pos < 64:
                    selector = np.uint64(1) << source_pos
                    if (mask & selector) != empty:
                        break
                    source_pos += 1
                if source_pos >= 64:
                    break
                
                # 2. Found the start of a block
                start_source = source_pos

                # 3. Find the end of this contiguous block
                while source_pos < 64:
                    selector = np.uint64(1) << source_pos
                    if (mask & selector) == empty:
                        break
                    source_pos += 1
                block_len = source_pos - start_source
        
                # 4. Create the mask for this block
                block_mask = (np.uint64(0xFFFFFFFFFFFFFFFF) >> (64 - block_len)) << start_source
        
                # 5. Calculate the shift required to pack this block
                shift_amount = start_source - dest_pos
        
                # Store in the plan: (mask, shift)
                plan.append((block_mask, shift_amount))
        
                # 6. Increment destination position by the number of bits packed
                dest_pos += block_len

            return plan

        self.pack_plan = _precompute_pack_plan()

        # - Symmetries ...

        def _get_position_mapping(symms) -> list[int]:
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
            tfn = SquareSet.trans_fs[instance_idx]
            atfn = SquareSet.anti_trans_fs[instance_idx]
            exchanges = np.zeros(self.n_squares, dtype=np.uint8)
            for j in range(self.n_squares):
                sq = Square(j)
                sqm = SquareSet(SquareSet(1) << sq)
                sqm_up = SquareSet(unpack_ss(sqm, self))
                m = SquareSet(tfn(sqm_up))
                e = SquareSet(pack_ss(m, self))
                idx = e.bsr()
                exchanges[j] = idx
            symms.append(exchanges)
        pos_map = _get_position_mapping(symms)
        unique_pos_map = set(pos_map)
        unique_pos_map.discard(0)
        self.unique_symmetric_instance_indexes = [int(x) for i, x in enumerate(symmetric_instance_indexes) if i in unique_pos_map]
        self.symmetry_fs = [SquareSet.trans_fs[idx] for idx in self.unique_symmetric_instance_indexes]

        # - Symmetries.

        # Invariance check
        self._check_invariances()

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
        if not set(self.symmetry_fs).issubset(set(SquareSet.trans_fs)):
             raise AssertionError("symmetry_fs contains unknown transformation functions.")

        
    def print(self) -> None:
        trans_fs_labels = [SquareSet.transformation_labels[i] for i in self.unique_mask_indexes]
        a_trans_fs_labels = [SquareSet.anti_transformation_labels[i] for i in self.unique_mask_indexes]
        symmetry_fs_labels = [SquareSet.transformation_labels[i] for i in self.unique_symmetric_instance_indexes]
        print("[Pattern: name = {}, mask = 0x{:016x}]".format(self.name, self.mask))
        print(f"  [n_squares = {self.n_squares}, n_configurations = {self.n_configurations}, n_instances = {self.n_instances}, n_stabilizer = {self.n_stabilizer}]")
        print(f"  Cells:               [{', '.join(f'{cn}' for cn in self.snames)}]")
        print(f"  Transformed masks:   [{', '.join(f'0x{x:016X}' for x in self.tmasks)}]")
        print(f"  Mask indexes:        [{', '.join(f'{x}' for x in self.mask_indexes)}]")
        print(f"  Unique masks:        [{', '.join(f'0x{x:016X}' for x in self.unique_masks)}]")
        print(f"  Unique mask indexes: [{', '.join(f'{x}' for x in self.unique_mask_indexes)}]")
        print(f"  Transf. functions:   [{', '.join(f'{fn}' for fn in trans_fs_labels)}]")
        print(f"  Anti-transf. f.:     [{', '.join(f'{fn}' for fn in a_trans_fs_labels)}]")
        print(f"  Symmetry functions:  [{', '.join(f'{fn}' for fn in symmetry_fs_labels)}]")

def pack_ss(s: np.uint64, p: Pattern) -> np.uint64:
    """
    Packs a square set according to the mask defined by pattern p.
    """
    s = np.uint64(s)
    res = np.uint64(0)
    
    for block_mask, shift in p.pack_plan:
        res |= (s & block_mask) >> shift
        
    return res

def unpack_ss(packed_val: np.uint64, p: Pattern) -> np.uint64:
    """
    Executes the inverse of pack_ss (PDEP simulation).
    Takes packed bits and distributes them back to their original 
    positions on the bitboard using the precomputed pack_plan.
    
    Args:
        packed_val: The compressed bits (result of a pack operation).
        p: The Pattern object containing the pack_plan.
    """
    res = np.uint64(0)
    packed_val = np.uint64(packed_val)
    all_ones = np.uint64(0xFFFFFFFFFFFFFFFF)
    
    # We need to track how many bits we have already 'unpacked' 
    # to know where to extract them from the packed_val.
    current_packed_pos = 0
    
    for block_mask, shift in p.pack_plan:
        # 1. Calculate the length of the block from its mask
        # (Technically, the plan could store block_len, but we can derive it)
        # Using bit_count is efficient on Zen 3 (native popcount)
        block_len = int(np.bitwise_count(block_mask))
        
        # 2. Create a mask for the bits in their CURRENT packed position
        extract_mask = (all_ones >> np.uint64(64 - block_len)) << np.uint64(current_packed_pos)
        
        # 3. Extract the bits from the packed value and shift them BACK to the source
        # Since 'shift' was (source - dest), we shift LEFT to go back.
        res |= (packed_val & extract_mask) << np.uint64(shift)
        
        # 4. Advance the pointer in the packed value
        current_packed_pos += block_len
        
    return res
