#
# domain.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
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
# >>> from twolm.domain import *
# >>> ar = SquareSet(0x22120a0e1222221e)
# >>> ar
# SquareSet(2455035802420388382)
# >>> ar.log()
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

from enum import Enum

from typing import Self, Callable, Any, Union, TypeVar, List

import hashlib
import logging
import sys
import io

__all__ = ['Square', 'Move', 'SquareSet', 'Board', 'Pattern', 'PatternSet',
           'pack_ss', 'unpack_ss', 'sample_patterns',
           'convert_to_principal_index']

logging.basicConfig(
    stream=sys.stdout,
    level=logging.INFO,
    format='%(message)s'
)

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
        """
        Creates a new Square instance from a string representation.

        Args:
            name (str): The string representation of the square (e.g., 'A1').

        Returns:
            Square: A new Square instance.

        Raises:
            ValueError: If the name is not a valid square name.
        """
        try:
            index = cls.__names__.index(name)
            return cls(index)
        except ValueError:
            raise ValueError(f"Invalid name: '{name}'. Must be one of {cls.__names__}")

    def __new__(cls: type[Self], value: np.uint8) -> Self:
        """
        Creates a new Square instance from a numeric value.

        Args:
            value (np.uint8): The numeric value of the square (0-63).

        Returns:
            Square: A new Square instance.

        Raises:
            AssertionError: If the value is not in the range [0..63].
        """
        assert 0 <= value < cls.__max_value__, f"Value must be in the range [0..{cls.__max_value__}]"
        return super().__new__(cls, value)

    def to_str(self) -> str:
        """
        Returns the string representation of the square.

        Returns:
            str: The string representation of the square (e.g., 'A1').
        """
        return self.__names__[self]

    def as_square_set(self) -> SquareSet:
        """
        Converts the square to a SquareSet containing only this square.

        Returns:
            SquareSet: A SquareSet containing only this square.
        """
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
        ['ro000',
         'ro090',
         'ro180',
         'ro270',
         'fvert',
         'fh1a8',
         'fhori',
         'fa1h8',
         ])

    anti_transformation_labels = np.array(
        ['ro000',
         'ro270',
         'ro180',
         'ro090',
         'fvert',
         'fh1a8',
         'fhori',
         'fa1h8',
         ])

    @classmethod
    def new_from_signed_int(cls, i: np.int64) -> SquareSet:
        """
        Returns a new SquareSet object from a numpy.int64 value given as argument.

        Args:
            i (np.int64): The signed integer value.

        Returns:
            SquareSet: A new SquareSet instance.

        Raises:
            TypeError: If the argument is not an instance of numpy.int64.

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

        Args:
            h (str): The hexadecimal string.

        Returns:
            SquareSet: A new SquareSet instance.

        Raises:
            TypeError: If the argument is not an instance of str.
            ValueError: If the argument string is not 16 characters long.

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

        Returns:
            SquareSet: A new SquareSet instance with the same value.
        """
        return SquareSet(self)

    def to_signed_int(self) -> np.int64:
        """
        Returns the signed int 64 bit long representation of the square set.
        It is useful to store and retrieve the value from PostgreSQL databases.

        Returns:
            np.int64: The signed integer representation of the square set.
        """
        return np.int64(self)

    def bsr(self) -> int:
        """
        Bit Scan Reverse - returns index of MOST significant set bit (63-0) or -1 if zero.

        Returns:
            int: The index of the most significant set bit or -1 if the square set is zero.
        """
        n = int(self)
        if n == 0:
            return -1
        return n.bit_length() - 1

    def to_square_list(self) -> List[Square]:
        """
        Returns the square set represented as a list of squares.
        Squares are ordered from larger (H8) to smaller (A1).

        Returns:
            List[Square]: A list of Square instances.
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

    def to_square_array(self) -> npt.NDArray[Square]:
        """
        Returns the square set as a numpy array.

        Returns:
            npt.NDArray[Square]: A numpy array of Square instances.
        """
        return np.array(self.to_square_list())

    def to_string_list(self) -> List[str]:
        """
        Returns the square set as a list of strings.

        Returns:
            List[str]: A list of string representations of the squares.
        """
        squares = self.to_square_list()
        return [sq.to_str() for sq in squares]
    
    def count(self) -> int:
        """
        Returns the number of squares in the set.

        Returns:
            int: The number of squares in the set.
        """
        bit_count = np.bitwise_count(self)
        return bit_count
    
    def log(self) -> None:
        """
        Logs (INFO level) a 2D representation of the square set.
        """
        buffer = io.StringIO()
        one = SquareSet(1)
        sq = SquareSet(1)
        print('  a b c d e f g h', file=buffer)
        for row in range(0,8):
            print(row + 1, end = '', file=buffer)
            for col in range(0,8):
                if sq & self != 0:
                    c = 'x'
                else:
                    c = '.'
                print(' ' + c, end = '', file=buffer)
                sq <<= one
            print(file=buffer)
        logging.info(buffer.getvalue())
        buffer.close()
        return

    @staticmethod
    def _fa1h8(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        k1 = 0x5500550055005500
        k2 = 0x3333000033330000
        k4 = 0x0f0f0f0f00000000
        s = ss
        t =      k4 & (s ^ (s << 28))
        s = s ^       (t ^ (t >> 28))
        t =      k2 & (s ^ (s << 14))
        s = s ^       (t ^ (t >> 14))
        t =      k1 & (s ^ (s << 7))
        s = s ^       (t ^ (t >> 7))
        return s

    def fa1h8(self) -> Self:
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
        

        Returns:
            SquareSet: The reflected square set.
        """
        return SquareSet(self._fa1h8(self))

    @staticmethod
    def _fh1a8(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        k1 = 0xaa00aa00aa00aa00
        k2 = 0xcccc0000cccc0000
        k4 = 0xf0f0f0f00f0f0f0f
        s = ss
        t =            s ^ (s << 36)
        s = s ^ (k4 & (t ^ (s >> 36)))
        t =      k2 & (s ^ (s << 18))
        s = s ^       (t ^ (t >> 18))
        t =      k1 & (s ^ (s << 9))
        s = s ^       (t ^ (t >> 9))
        return s
    
    def fh1a8(self) -> Self:
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

        Returns:
            SquareSet: The reflected square set.
        """
        return SquareSet(self._fh1a8(self))

    @staticmethod
    def _fhori(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        mask56 = 0xFF00000000000000
        mask48 = 0x00FF000000000000
        mask40 = 0x0000FF0000000000
        mask32 = 0x000000FF00000000
        mask24 = 0x00000000FF000000
        mask16 = 0x0000000000FF0000
        mask08 = 0x000000000000FF00
        mask00 = 0x00000000000000FF
        s = ss
        s = (((s << 56) & mask56) |
             ((s << 40) & mask48) |
             ((s << 24) & mask40) |
             ((s <<  8) & mask32) |
             ((s >>  8) & mask24) |
             ((s >> 24) & mask16) |
             ((s >> 40) & mask08) |
             ((s >> 56) & mask00))
        return s

    def fhori(self) -> Self:
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

        Returns:
            SquareSet: The reflected square set.        
        """
        return SquareSet(self._fhori(self))

    @staticmethod
    def _fvert(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        k1 = 0x5555555555555555
        k2 = 0x3333333333333333
        k4 = 0x0f0f0f0f0f0f0f0f
        s = ss
        s = ((s >> 1) & k1) | ((s & k1) << 1)
        s = ((s >> 2) & k2) | ((s & k2) << 2)
        s = ((s >> 4) & k4) | ((s & k4) << 4)
        return s

    def fvert(self) -> Self:
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

        Returns:
            SquareSet: The reflected square set.
        """
        return SquareSet(self._fvert(self))

    @staticmethod
    def _ro000(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        return ss

    def ro000(self) -> Self:
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
        
        Returns:
            SquareSet: The unchanged square set.
        """
        return SquareSet(self._ro000(self))

    @staticmethod
    def _ro180(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        return SquareSet._fvert(SquareSet._fhori(ss))

    def ro180(self) -> Self:
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
        
        Returns:
            SquareSet: The rotated square set.
        """
        return SquareSet(self._ro180(self))

    @staticmethod
    def _ro090(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        return SquareSet._fhori(SquareSet._fh1a8(ss))

    def ro090(self) -> Self:
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

        Returns:
            SquareSet: The rotated square set.
        """
        return SquareSet(self._ro090(self))

    @staticmethod
    def _ro270(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        return SquareSet._fh1a8(SquareSet._fhori(ss))

    def ro270(self) -> Self:
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

        Returns:
            SquareSet: The rotated square set.
        """
        return SquareSet(self._ro270(self))

    @staticmethod
    def trxs(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        """
        Vectorized transformation of input array into 8 derived uint64 columns.
        It is the vectorized version of method transformations().
        Input ss: Shape (N,)
        Output: Shape (N, 8)
        """
        ss = np.atleast_1d(ss)
        n = ss.shape[0]
        ts = np.empty((n, 8), dtype=np.uint64)

        # Pre-calculate common intermediate transformation
        fh = SquareSet._fhori(ss)

        # Each function call is expected to return an array of shape (N,)
        ts[:, 0] = ss
        ts[:, 1] = SquareSet._fa1h8(fh)
        ts[:, 2] = SquareSet._fvert(fh)
        ts[:, 3] = SquareSet._fh1a8(fh)
        ts[:, 4] = SquareSet._fvert(ss)
        ts[:, 5] = SquareSet._fh1a8(ss)
        ts[:, 6] = fh
        ts[:, 7] = SquareSet._fa1h8(ss)

        return ts

    @staticmethod
    def atrxs(ss: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint64]:
        """
        Vectorized anti-transformation of input array into 8 derived uint64 columns.
        It is the vectorized version of method anti_transformations().
        Input ss: Shape (N,)
        Output: Shape (N, 8)
        """
        ss = np.atleast_1d(ss)
        n = ss.shape[0]
        ats = np.empty((n, 8), dtype=np.uint64)

        # Pre-calculate common intermediate transformation
        fh = SquareSet._fhori(ss)

        # Each function call is expected to return an array of shape (N,)
        ats[:, 0] = ss
        ats[:, 1] = SquareSet._fh1a8(fh)
        ats[:, 2] = SquareSet._fvert(fh)
        ats[:, 3] = SquareSet._fa1h8(fh)
        ats[:, 4] = SquareSet._fvert(ss)
        ats[:, 5] = SquareSet._fh1a8(ss)
        ats[:, 6] = fh
        ats[:, 7] = SquareSet._fa1h8(ss)

        return ats
    
    def transformations(self) -> npt.NDArray[SquareSet]:
        """
        Returns eight square set as an array by transforming the set as follow.
         - 0 -> 0 : ro000
         - 0 -> 1 : ro090
         - 0 -> 2 : ro180
         - 0 -> 3 : ro270
         - 0 -> 4 : fvert
         - 0 -> 5 : fh1a8
         - 0 -> 6 : fhori
         - 0 -> 7 : fa1h8
        
        Returns:
            npt.NDArray[SquareSet]: An array of transformed square sets.
        """
        return SquareSet.trxs(self).view(SquareSet).flatten()
    
    def anti_transformations(self) -> npt.NDArray[SquareSet]:
        """
        Returns eight square set as an array by transforming the set as follow.
         - 0 -> 0 : ro000
         - 0 -> 1 : ro270
         - 0 -> 2 : ro180
         - 0 -> 3 : ro090
         - 0 -> 4 : fvert
         - 0 -> 5 : fh1a8
         - 0 -> 6 : fhori
         - 0 -> 7 : fa1h8
        These are the anti-transformations as defined by the transformations() method.
        
        Returns:
            npt.NDArray[SquareSet]: An array of anti-transformed square sets.
        """
        return SquareSet.atrxs(self).view(SquareSet).flatten()

    trans_fs = np.array(
        [ro000,
         ro090,
         ro180,
         ro270,
         fvert,
         fh1a8,
         fhori,
         fa1h8,
         ])

    anti_trans_fs = np.array(
        [ro000,
         ro270,
         ro180,
         ro090,
         fvert,
         fh1a8,
         fhori,
         fa1h8,
         ])

    _trans_fs = np.array(
        [_ro000,
         _ro090,
         _ro180,
         _ro270,
         _fvert,
         _fh1a8,
         _fhori,
         _fa1h8,
         ])

    _anti_trans_fs = np.array(
        [_ro000,
         _ro270,
         _ro180,
         _ro090,
         _fvert,
         _fh1a8,
         _fhori,
         _fa1h8,
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

    def __eq__(self, other) -> bool:
        """
        Two boards are equals when mover and opponent fields are equal.

        Args:
            other: The object to compare with.
        
        Returns:
            bool: True if the boards are equal, False otherwise.
        """
        if isinstance(other, Board):
            return self.mover == other.mover and self.opponent == other.opponent
        return False

    def clone(self) -> Board:
        """
        Clones the board.

        Returns:
            Board: A new Board instance with the same state.
        """
        c = Board(self.mover, self.opponent)
        c._lms_ = self._lms_
        c._lmc_ = self._lmc_
        return c

    def log(self) -> None:
        """
        Logs (INFO level) a 2D representation of the board.
        Symbol @ identifies the mover, symbol O the opponent.
        """
        buffer = io.StringIO()
        one = SquareSet(1)
        sq = SquareSet(1)
        print('  a b c d e f g h', file=buffer)
        for row in range(0,8):
            print(row + 1, end = '', file=buffer)
            for col in range(0,8):
                if sq & self.mover != 0:
                    c = '@'
                elif sq & self.opponent != 0:
                    c = 'O'
                else:
                    c = '.'
                print(' ' + c, end = '', file=buffer)
                sq <<= one
            print(file=buffer)
        logging.info(buffer.getvalue())
        buffer.close()
        return

    def empties(self) -> SquareSet:
        """
        Returns the set of empty squares in the game position.

        Returns:
            SquareSet: A SquareSet containing the empty squares.
        """
        return ~(self.mover | self.opponent)

    def get_mover(self) -> SquareSet:
        """
        Returns the set of squares belonging to the mover player.

        Returns:
            SquareSet: A SquareSet containing the mover's squares.
        """
        return self.mover

    def get_opponent(self) -> SquareSet:
        """
        Returns the set of squares belonging to the opponent player.

        Returns:
            SquareSet: A SquareSet containing the opponent's squares.
        """
        return self.opponent

    def legal_moves(self) -> SquareSet:
        """
        Returns the set of squares that represents the legal moves for the game position.

        Returns:
            SquareSet: A SquareSet containing the legal moves.
        """
        if self._lms_ is None:
            self._lms_ = _kogge_stone_lms(self.mover, self.opponent, self.empties())
            self._lmc_ = self._lms_.count()
        return self._lms_

    def legal_moves_count(self) -> int:
        """
        Returns the count of legal moves for the game position.

        Returns:
            int: The number of legal moves.
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

        Args:
            move (SquareSet): The move to be made.

        Returns:
            (SquareSet, Board): A tuple containing the flipped squares and the updated board.
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

        Args:
            move (Move): The move to be made.

        Returns:
            Board: The updated board after the move.
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

        Returns:
            int: The disk difference.
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

        Returns:
            int: The final score.
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

        Returns:
            bool: True if the mover has to pass, False otherwise.
        """
        lmc = self.legal_moves_count()
        if lmc == 0:
            return True
        else:
            return False

    def is_game_over(self) -> bool:
        """
        Returns true if the game is over.

        Returns:
            bool: True if the game is over, False otherwise.
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

        Args:
            move (Move): The move to be checked.

        Returns:
            bool: True if the move is legal, False otherwise.
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

    def fa1h8(self) -> Board:
        """
        Reflects the board on the diagonal a1-h8.

        Returns:
            Board: The reflected board.
        """
        return Board(self.mover.fa1h8(),
                     self.opponent.fa1h8())

    def fh1a8(self) -> Board:
        """
        Reflects the board on the diagonal h1-a8.

        Returns:
            Board: The reflected board.
        """
        return Board(self.mover.fh1a8(),
                     self.opponent.fh1a8())

    def fhori(self) -> Board:
        """
        Reflects the board horizontally (on the horizontal axis).

        Returns:
            Board: The reflected board.
        """
        return Board(self.mover.fhori(),
                     self.opponent.fhori())

    def fvert(self) -> Board:
        """
        Reflects the board vertically (on the vertical axis).

        Returns:
            Board: The reflected board.
        """
        return Board(self.mover.fvert(),
                     self.opponent.fvert())

    def ro000(self) -> Board:
        """
        Returns the board unchanged, as it is.

        Returns:
            Board: The unchanged board.
        """
        return self.clone()

    def ro180(self) -> Board:
        """
        Rotates the board by 180 degrees.

        Returns:
            Board: The rotated board.
        """
        return Board(self.mover.ro180(),
                     self.opponent.ro180())

    def ro090(self) -> Board:
        """
        Rotates the board by 90 degrees clockwise.

        Returns:
            Board: The rotated board.
        """
        return Board(self.mover.ro090(),
                     self.opponent.ro090())

    def ro270(self) -> Board:
        """
        Rotates the board by 90 degrees anticlockwise.

        Returns:
            Board: The rotated board.
        """
        return Board(self.mover.ro270(),
                     self.opponent.ro270())

    def transformations(self) -> npt.NDArray[Board]:
        """
        Returns an array of transformed boards.
        See the transformation() method defined by SquareSet.
        """
        tm = self.mover.transformations()
        to = self.opponent.transformations()
        return np.array([Board(SquareSet(m), SquareSet(o)) for m, o in zip(tm, to)])
    
    def anti_transformations(self) -> npt.NDArray[Board]:
        """
        Returns an array of anti-transformed boards.
        See the anti_transformation() method defined by SquareSet.
        """
        tm = self.mover.anti_transformations()
        to = self.opponent.anti_transformations()
        return np.array([Board(SquareSet(m), SquareSet(o)) for m, o in zip(tm, to)])


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

    def __init__(self, name: str, mask: SquareSet):
        """
        Initializes a new Pattern instance with the given name and mask.
        
        Args:
            name (str): The human-readable label for the pattern (e.g., "EDG8", "DIAG8", "CORNER").
            mask (SquareSet): The primary 64-bit mask defining the squares belonging to this pattern.
        
        Raises:
            TypeError: If the mask is not an instance of SquareSet.
            TypeError: If the name is not an instance of str.
            ValueError: If the mask is not the principal mask among its transformations.
        """
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
        self.unique_mask_indexes = np.array(list(dict.fromkeys(self.mask_indexes)))
        # n_instances corresponds to the cardinality of the orbit |Orb(P)|
        self.n_instances = len(self.unique_masks)
        # n_stabilizers corresponds to the order of the stabilizer |Stab(P)|
        self.n_stabilizers = 8 // self.n_instances
        self.trans_fs = [SquareSet.trans_fs[i] for i in self.unique_mask_indexes]
        self.anti_trans_fs = [SquareSet.anti_trans_fs[i] for i in self.unique_mask_indexes]

        def _check_mask_is_principal() -> int:
            """
            Checks if the mask is the principal mask among its transformations.
            The principal mask is the one having the lower value of the highest bit set.
        
            Returns:
                int: The index of the offending transformation if the mask is not principal, otherwise 0.
            """
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
        
        def _precompute_pack_unpack_plans() -> tuple[npt.NDArray[np.uint64],
                                                     npt.NDArray[np.uint64],
                                                     npt.NDArray[np.uint64]]:
            """
            Precomputes the pack and unpack plans for the pattern.
            These plans are used to compress and decompress the square set according to the pattern's mask.
        
            Returns:
                tuple[npt.NDArray[np.uint64], npt.NDArray[np.uint64], npt.NDArray[np.uint64]]:
                    A tuple containing three numpy arrays:
                    - p_masks (npt.NDArray[np.uint64]): The block masks used for packing.
                    - u_masks (npt.NDArray[np.uint64]): The block masks used for unpacking.
                    - p_shifts (npt.NDArray[np.uint64]): The shift amounts used for packing.
            """
            mask = np.uint64(self.mask)
            one = np.uint64(1)
            all_ones = np.uint64(0xFFFFFFFFFFFFFFFF)
            dest_pos = 0
            source_pos = 0
    
            p_masks, p_shifts, u_masks = [], [], []

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

            return (np.array(p_masks, dtype=np.uint64),
                    np.array(u_masks, dtype=np.uint64),
                    np.array(p_shifts, dtype=np.uint64))

        self.pack_plan = _precompute_pack_unpack_plans()

        # - Symmetries ...

        def _get_position_mapping(symms: list[list[int]]) -> list[int]:
            """
            Generates a mapping of unique symmetry positions.
        
            Args:
                symms (list[list[int]]): A list of lists, where each inner list represents a permutation of cell positions
                                         resulting from a symmetry transformation.
        
            Returns:
                list[int]: A list of integers representing the unique position mapping for each symmetry.
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
        self._symmetry_fs = [SquareSet._trans_fs[idx] for idx in self.unique_symmetric_instance_indexes]
        self._anti_symmetry_fs = [SquareSet._anti_trans_fs[idx] for idx in self.unique_symmetric_instance_indexes]

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
        transformed_masks = self.mask.transformations()
        
        # For each original cell, determine where it maps to in each transformed pattern
        for i, cell_name in enumerate(original_cells):
            # Convert cell name to square index
            sq = Square.new_from_str(cell_name)
            # Create a square set with just this square
            sq_set = SquareSet(SquareSet(1) << sq)
            
            # For each transformation, find where this square maps to
            for j in range(8):
                # Apply transformation to the square
                transformed_sq = SquareSet.trans_fs[j](sq_set)
                # Get the square index from the transformed mask
                if transformed_sq != 0:
                    # Find the bit position
                    pos = SquareSet(transformed_sq).bsr()
                    transformed_cell = Square(pos)
                    # Convert back to square name
                    transformed_cell_name = Square(pos).to_str().lower()
                else:
                    transformed_cell = None
                    transformed_cell_name = ""
                if transformed_cell is not None:
                    self.squares_t[j].append(int(transformed_cell))
                self.snames_t[j].append(transformed_cell_name)
                
    def mdp_record(self) -> str:
        """
        Returns a string representation of the pattern in CSV format.
        Fields are separated by commas.
        
        Returns:
            str: A string representing the pattern in CSV format.
        """
        trans_fs_labels = [SquareSet.transformation_labels[i] for i in self.unique_mask_indexes]
        anti_trans_fs_labels = [SquareSet.anti_transformation_labels[i] for i in self.unique_mask_indexes]
        symmetry_fs_labels = [SquareSet.transformation_labels[i] for i in self.unique_symmetric_instance_indexes]
        ptype = self.type_info.data["type"]
        return (
            f"{self.name},{self.mask:016X},{self.n_squares},{self.n_configurations}"
            f",{self.n_instances},{self.n_stabilizers},{':'.join(self.snames)}"
            f",{':'.join(f'{m:016X}' for m in self.tmasks)}"
            f",{':'.join(str(i) for i in self.mask_indexes)}"
            f",{':'.join(f'{m:016X}' for m in self.unique_masks)}"
            f",{':'.join(str(i) for i in self.unique_mask_indexes)}"
            f",{':'.join(f'{fn}' for fn in trans_fs_labels)}"
            f",{':'.join(f'{fn}' for fn in anti_trans_fs_labels)}"
            f",{':'.join(f'{fn}' for fn in symmetry_fs_labels)}"
            f",{','.join(':'.join(self.snames_t[i]) for i in range(8))}"
            f",{':'.join(f'{fp}' for fp in self.fingerprint)}"
            f",{ptype}"
        )

    def log(self) -> None:
        """
        Logs a detailed representation of the pattern to stdout.
        Includes information about the pattern's name, mask, number of squares, number of configurations,
        number of instances, number of stabilizers, type, cells, transformed masks, mask indexes,
        unique masks, unique mask indexes, transformation functions, anti-transformation functions,
        symmetry functions, transformed cells, transformed sorted cells, and fingerprint.
        """
        trans_fs_labels = [SquareSet.transformation_labels[i] for i in self.unique_mask_indexes]
        anti_trans_fs_labels = [SquareSet.anti_transformation_labels[i] for i in self.unique_mask_indexes]
        symmetry_fs_labels = [SquareSet.transformation_labels[i] for i in self.unique_symmetric_instance_indexes]
        ptype = self.type_info.data["type"]
        logging.info(f"[Pattern: name = {self.name}, mask = 0x{self.mask:016x}]")
        logging.info(f"  [n_squares = {self.n_squares}, n_configurations = {self.n_configurations}, n_instances = {self.n_instances}, n_stabilizers = {self.n_stabilizers}, type = {ptype}]")
        logging.info(f"  Cells:                [{', '.join(f'{cn}' for cn in self.snames)}]")
        logging.info(f"  Transformed masks:    [{', '.join(f'0x{x:016X}' for x in self.tmasks)}]")
        logging.info(f"  Mask indexes:         [{', '.join(f'{x}' for x in self.mask_indexes)}]")
        logging.info(f"  Unique masks:         [{', '.join(f'0x{x:016X}' for x in self.unique_masks)}]")
        logging.info(f"  Unique mask indexes:  [{', '.join(f'{x}' for x in self.unique_mask_indexes)}]")
        logging.info(f"  Transf. functions:    [{', '.join(f'{fn}' for fn in trans_fs_labels)}]")
        logging.info(f"  Anti-transf. f.:      [{', '.join(f'{fn}' for fn in anti_trans_fs_labels)}]")
        logging.info(f"  Symmetry functions:   [{', '.join(f'{fn}' for fn in symmetry_fs_labels)}]")
        logging.info(f"  Transformed cells:    [{', '.join(f'{l}' for l in self.squares_t)}]")
        logging.info(f"  Transf. sorted cells: [{', '.join(f'{l}' for l in self.squares_ts)}]")
        logging.info(f"  Fingerprint:          [{', '.join(f'{fp}' for fp in self.fingerprint)}]")

    def compute_indexes_on_board(self, b: Board) -> npt.NDArray[np.int32]:
        """
        Computes the indexes of the pattern on the given board for both mover and opponent.
        
        Args:
            b (Board): The board on which to compute the pattern indexes.
            
        Returns:
            npt.NDArray[np.int32]: An array of indexes representing the pattern configurations on the board.
        """
        instances_mover = b.mover.anti_transformations()[self.unique_mask_indexes]
        instances_opponent = b.opponent.anti_transformations()[self.unique_mask_indexes]
        combined = np.stack([instances_mover, instances_opponent])
        p_combined = pack_ss(combined, self)
        bits = (p_combined[..., np.newaxis] >> self.bit_shifts) & 1
        idxs = bits @ self.powers_3
        return (idxs[0] + 2 * idxs[1]).astype(np.int32)

    def compute_indexes_on_ss_packed_tensor(self, 
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
        bits = (combined[..., np.newaxis] >> self.bit_shifts) & 1
        idxs = bits @ self.powers_3
        indexes = (idxs[0] + 2 * idxs[1]).astype(np.uint32)
        return indexes
        
    def compute_principal_index_dict(self) -> None:
        """
        Computes the dictionary of principal indexes for the pattern.
        This dictionary maps each configuration index to its principal index,
        which is the smallest index among all its symmetric configurations.
        The method also computes the unique principal indexes and their count.
        """
        def _symmetry_transformations(ss_array: npt.NDArray[np.uint64], 
                                     symmetry_functions: list[Callable[[npt.NDArray[np.uint64]], npt.NDArray[np.uint64]]]) -> npt.NDArray[np.uint64]:
            # Apply each function to the entire array and collect the results in a list
            symmetries = [f(ss_array) for f in symmetry_functions]
            # Stack the columns into a single matrix N x X
            return np.column_stack(symmetries)

        # -1- Compute the packed configurations of mover and opponent.
        #     This code is executed the same for each pattern, so cuould be factored and memoized, doing it for the larges one.
        m, o = _compute_mover_opponent_by_index_value(int(self.n_squares))

        # -2- Unpack the two square sets to get the instance 0 of the pattern with the given index.
        m_unpacked, o_unpacked = unpack_ss(m[:self.n_configurations], self), unpack_ss(o[:self.n_configurations], self)
        
        # -3- Apply the symmetry operations [O,1,3,7]
        symm_tr_fs = [SquareSet._ro000] + self._anti_symmetry_fs
        m_syms = _symmetry_transformations(m_unpacked, symm_tr_fs)
        o_syms = _symmetry_transformations(o_unpacked, symm_tr_fs)

        # -4- Pack the results
        m_syms_packed = pack_ss(m_syms, self)
        o_syms_packed = pack_ss(o_syms, self)
        
        # -5- Calculate the indexes of the symmetric instances
        sym_indexes = self.compute_indexes_on_ss_packed_tensor(m_syms_packed, o_syms_packed)

        # -6- Take the lowest available value and insert it into principal_index_dict[index]
        self.principal_index_dict = sym_indexes.min(axis=1)
        self.principal_indexes = np.unique(self.principal_index_dict)
        self.principal_index_count = len(self.principal_indexes)
        return

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

def convert_to_principal_index(index: npt.NDArray[np.uint32],
                               p: Pattern) -> npt.NDArray[np.uint32]:
    """
    Converts a given configuration index to its principal index using the pattern's principal index dictionary.
    If the dictionary is not yet computed, it computes it first.
    
    Args:
        index (npt.NDArray[np.uint32]): The configuration index to be converted.
        p (Pattern): The Pattern object containing the principal index dictionary.
    
    Returns:
        npt.NDArray[np.uint32]: The principal index corresponding to the given configuration index.
    """
    if not p.principal_index_dict:
        p.compute_principal_index_dict()
    principal_index = p.principal_index_dict[index]
    return principal_index

def _compute_mover_opponent_by_index_value(n_squares: int) -> tuple[npt.NDArray[np.uint32], npt.NDArray[np.uint32]]:
    if not isinstance(n_squares, int):
        raise TypeError('Argument n_squares is not an instance of int')
    if n_squares < 0:
        raise ValueError(f"Argumnet n_square = {n_squares}. It must be positive.")
    # Pattern having more than 18 n_squares are not fitting the uint32 space.
    MAX_NUMBER_OF_SQUARE_FOR_PATTERN = 18
    if n_squares > MAX_NUMBER_OF_SQUARE_FOR_PATTERN:
        raise ValueError(f"More than 18 squares is not supported! n_square = {n_squares}. Aborting!")
    N = 3 ** n_squares
    indices = np.arange(N, dtype=np.uint32)
    powers = 3 ** np.arange(n_squares, dtype=np.uint32)
    digits = (indices[:, None] // powers) % 3    
    bit_powers = np.uint64(1) << np.arange(n_squares, dtype=np.uint64)
    m = np.sum((digits == 1) * bit_powers, axis=1, dtype=np.uint64)
    o = np.sum((digits == 2) * bit_powers, axis=1, dtype=np.uint64)
    return m, o



#
# A list of Patterns.
#
# Use it to generate the mdp.csv file used by the LaTeX document:
#
# $ cd $(REVERSI_HOME)/c
# $ source py/.reversi_venv/bin/activate
# $ PYTHONPATH="./py" python3
# >>> from twolm.domain import *
# >>> Pattern.mdp_csv_file(sample_patterns, 'build/tmp/mdp_test.csv')
#
sample_patterns = [
    Pattern('ELLE',   SquareSet(0x0000000000000107)),
    Pattern('SNAKE',  SquareSet(0x0000000C30000000)),
    Pattern('EDGE',   SquareSet(0x00000000000000FF)),
    Pattern('R2',     SquareSet(0x000000000000FF00)),
    Pattern('R3',     SquareSet(0x0000000000FF0000)),
    Pattern('R4',     SquareSet(0x00000000FF000000)),
    Pattern('XEDGE',  SquareSet(0x00000000000042FF)),
    Pattern('DIAG3',  SquareSet(0x0000000000010204)),
    Pattern('DIAG4',  SquareSet(0x0000000001020408)),
    Pattern('DIAG5',  SquareSet(0x0000000102040810)),
    Pattern('DIAG6',  SquareSet(0x0000010204081020)),
    Pattern('DIAG7',  SquareSet(0x0001020408102040)),
    Pattern('DIAG8',  SquareSet(0x0102040810204080)),
    Pattern('CORNER', SquareSet(0x0000000000070707)),
    Pattern('2X5COR', SquareSet(0x0000000000001F1F)),
    Pattern('2X6COR', SquareSet(0x0000000000003F3F)),
    Pattern('RCT2X4', SquareSet(0x0000003C3C000000)),
    Pattern('CASTLE', SquareSet(0x000000000000C3FF)),
    Pattern('BARBEL', SquareSet(0x030304081020C0C0)),
    Pattern('MACE',   SquareSet(0x010204081020C0C0)),
    Pattern('FOURC',  SquareSet(0x8100000000000081)),
    Pattern('CORE',   SquareSet(0x0000001818000000)),
    Pattern('CORED',  SquareSet(0x0000241818240000)),
    Pattern('COREA',  SquareSet(0x000008381C100000)),
    Pattern('WHIRL',  SquareSet(0x83800000000001C1)),
    Pattern('TAU',    SquareSet(0x010100C1C1000101)),
    Pattern('DOTA1',  SquareSet(0x0000000000000001)),
    Pattern('DOTB1',  SquareSet(0x0000000000000002)),
    Pattern('TWOND',  SquareSet(0x0000000000000201)),
]


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
        if not isinstance(name, str):
            raise TypeError('Argument name is not an instance of str')
        if not isinstance(patterns, list):
            raise TypeError('Argument patterns is not a list')
        if not all(isinstance(p, Pattern) for p in patterns):
            raise TypeError('All elements in patterns must be Pattern instances')
        
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

    def log_summary(self) -> None:
        """
        Logs (INFO level) a summary of the set including the name, hash, and basic pattern information.
        """
        logging.info(f"PatternSet: name = {self.name}, lenght = {len(self.patterns)}, hash = {self.hash}")
        for pattern in self.patterns:
            logging.info(f"  Pattern: name = {pattern.name}, mask = 0x{pattern.mask:016x}")

    def log(self) -> None:
        """
        Logs (INFO level) a detailed summary of the set including the name, hash, and full pattern information.
        """
        logging.info(f"PatternSet: name = {self.name}, lenght = {len(self.patterns)}, hash = {self.hash}")
        for pattern in self.patterns:
            pattern.log()
