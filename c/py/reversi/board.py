#
# board.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022 Roberto Corradini. All rights reserved.
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
 
from reversi import libreversi as libreversi

import numpy as np

import ctypes as ct

from enum import Enum

# Defines SquareSet as an alias for numpy.uint64
# SquareSet = np.uint64
# Or better as a sub-class of numpy.uint64
class SquareSet(np.uint64):

    @classmethod
    def new_from_hex(cls, h: str) -> 'SquareSet':
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

    @classmethod
    def new_from_bin(cls, b: str) -> 'SquareSet':
        """
        Returns a new SquareSet object from the binary string given as argument.
        The b string must be 64 character long having values in the range [0..1].
        Note that the argument string is not prefixed with '0b'.

        Example:
          s = SquareSet.new_from_bin('0000000000000000000000000000000000000000000000000000000000000100')
        """
        if not isinstance(b, str):
            raise TypeError('Argument b is not an instance of str')
        if not len(b) == 64:
            raise ValueError('Argument b must be a string of lenght 64')
        i = int(b, 2)
        return SquareSet(i)    

    def print(self):
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

    def to_hex(self) -> str:
        return format(self, '016x')

    def to_bin(self) -> str:
        return format(self, '064b')

    def trans_flip_horizontal(self) -> 'SquareSet':
        f = libreversi.board_trans_flip_horizontal
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_flip_vertical(self) -> 'SquareSet':
        f = libreversi.board_trans_flip_vertical
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_flip_diag_h1a8(self) -> 'SquareSet':
        f = libreversi.board_trans_flip_diag_h1a8
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_flip_diag_a1h8(self) -> 'SquareSet':
        f = libreversi.board_trans_flip_diag_a1h8
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_rotate_180(self) -> 'SquareSet':
        f = libreversi.board_trans_rotate_180
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_rotate_90c(self) -> 'SquareSet':
        f = libreversi.board_trans_rotate_90c
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_rotate_90a(self) -> 'SquareSet':
        f = libreversi.board_trans_rotate_90a
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(self))

    def trans_identity(self) -> 'SquareSet':
        return SquareSet(self)


class Player(Enum):
    BLACK = 0
    WHITE = 1
    
    def opponent(self):
        if self == Player.BLACK:
            return Player.WHITE
        else:
            return Player.BLACK


class Color(Enum):
    BLACK = 0
    WHITE = 1
    EMPTY = 2
    
    def symbol(self):
        match self:
            case Color.BLACK:
                return '@'
            case Color.WHITE:
                return 'O'
            case Color.EMPTY:
                return '.'
            case _:
                return 'X'


Square = Enum("Square",
              [
               'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
               'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
               'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
               'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
               'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5',
               'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6',
               'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7',
               'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'H8',
              ], start = 0)


Move = Enum("Move",
            [
             'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1',
             'A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2',
             'A3', 'B3', 'C3', 'D3', 'E3', 'F3', 'G3', 'H3',
             'A4', 'B4', 'C4', 'D4', 'E4', 'F4', 'G4', 'H4',
             'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5',
             'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6',
             'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7',
             'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'H8',
             'PA', 'NA', 'UN'
            ], start = 0)


class BoardHelper(ct.Structure):
    _fields_ = [("m", ct.c_ulonglong),
                ("o", ct.c_ulonglong)]


class GamePositionHelper(ct.Structure):
    _fields_ = [("blacks", ct.c_ulonglong),
                ("whites", ct.c_ulonglong),
                ("player", ct.c_ushort)]


class Board:
    def __init__(self, mover: SquareSet, opponent: SquareSet):
        if not isinstance(mover, SquareSet):
            raise TypeError('Argument mover is not an instance of SquareSet')
        if not isinstance(opponent, SquareSet):
            raise TypeError('Argument opponent is not an instance of SquareSet')
        if mover & opponent != SquareSet(0):
            raise ValueError('Arguments mover and opponent have overlapping squares')
        self.mover = mover
        self.opponent = opponent

    @classmethod
    def new_from_hexes(cls, mover: str, opponent: str) -> 'Board':
        """
        Returns a new Board object created from two hexadecimal strings.
        Mover and opponent must be 16 character strings having values in the range [0..F].
        Characters could be uppercase or lovercase.
        The two hex strings must not have overlapping bits.
        Note that the argument strings are not prefixed with '0x'

        Example:
          b = Board.new_from_hexes('ffffffffffffffff', '0000000000000000')
        """
        m = SquareSet.new_from_hex(mover)
        o = SquareSet.new_from_hex(opponent)
        return Board(m, o)
        
    def empties(self) -> SquareSet:
        return SquareSet(~(self.mover | self.opponent))

    def legal_moves(self) -> SquareSet:
        f = libreversi.game_position_x_legal_moves
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_void_p]
        gph = GamePositionHelper(self.mover, self.opponent, Player.BLACK.value)
        gph_p = ct.pointer(gph)
        lms = SquareSet(f(gph_p))
        return lms

    def print(self):
        one = SquareSet(1)
        sq = SquareSet(1)
        print('  a b c d e f g h')
        for row in range(0,8):
            print(row + 1, end = '')
            for col in range(0,8):
                if sq & self.mover != 0:
                    c = Color.BLACK
                elif sq & self.opponent != 0:
                    c = Color.WHITE
                else:
                    c = Color.EMPTY
                print(' ' + c.symbol(), end = '')
                sq <<= one
            print()
        return

        
