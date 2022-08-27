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

import ctypes as ct
import numpy as np
import re

import operator, functools

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

    @classmethod
    def new_from_signed_int(cls, i: np.int64) -> 'SquareSet':
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
    def new_from_list(cls, l: list) -> 'SquareSet':
        """
        Returns a new SquareSet object from a list of Square given as argument.

        Example:
          l = [Square.G3, Square.A1, Square.F2]
          s = SquareSet.new_from_list(l)
        """
        if not isinstance(l, list):
            raise TypeError('Argument l is not an instance of list')
        if not all([isinstance(e, Square) for e in l]):
            raise TypeError('Argument l must have all elements belonging to Square type')
        lu = np.uint64([square.value for square in l])
        return SquareSet(functools.reduce(lambda a, b: a | (np.uint(1) << b), lu, np.uint64(0)))

    def fill_square_at_position(self, pos: int) -> 'SquareSet':
        return SquareSet(self | np.uint64(1) << np.uint64(pos))

    def remove_square_at_position(self, pos: int) -> 'SquareSet':
        return SquareSet(self & ~(np.uint64(1) << np.uint64(pos)))

    def count(self) -> int:
        f = libreversi.square_set_count
        f.restype = ct.c_ubyte
        f.argtypes = [ct.c_ulonglong]
        return f(self)

    def list(self):
        is_filled = [bool(int(element)) for element in '{:064b}'.format(self)[::-1]]
        return [square for (square, included) in list(zip(list(Square), is_filled)) if included]
    
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

    def to_signed_int(self) -> np.int64:
        return np.int64(self)

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

def move(self):
    return Move[self.name]

Square.move = move


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


class BoardCTHelper(ct.Structure):
    _fields_ = [("m", ct.c_ulonglong),
                ("o", ct.c_ulonglong)]


class GamePositionCTHelper(ct.Structure):
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
        self.lms = None

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

    def game_position(self) -> 'GamePosition':
        gp = GamePosition(self.mover, self.opponent, Player.BLACK)
        if self.lms:
            gp.lms = self.lms
        return gp
        
    def empties(self) -> SquareSet:
        return SquareSet(~(self.mover | self.opponent))

    def legal_moves(self) -> SquareSet:
        if not self.lms:
            self.lms = self.game_position().legal_moves()
        return self.lms

    def count_difference(self) -> int:
        return self.mover.count() - self.opponent.count()

    def final_value(self) -> int:
        mc = self.mover.count()
        oc = self.opponent.count()
        diff = mc - oc
        empties = 64 - (mc + oc)
        return diff + empties if diff > 0 else diff - empties

    def has_any_legal_move(self) -> bool:
        return True if self.legal_moves() else False
    
    # TODO: missing board methods:
    #
    # - All the board transformations defined on SquareSet
    # - Methods equivalent to GamePosition when applicable
    #
    # - Pattern index computation ... see board_trans.h and board_patterns.h
    #
    
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


class GamePosition:
    def __init__(self, blacks: SquareSet, whites: SquareSet, player: Player):
        if not isinstance(blacks, SquareSet):
            raise TypeError('Argument blacks is not an instance of SquareSet')
        if not isinstance(whites, SquareSet):
            raise TypeError('Argument whites is not an instance of SquareSet')
        if blacks & whites != SquareSet(0):
            raise ValueError('Arguments mover and opponent have overlapping squares')
        if not isinstance(player, Player):
            raise TypeError('Argument player is not an instance of Player')
        self.blacks = blacks
        self.whites = whites
        self.player = player
        self.lms = None

    @classmethod
    def new_from_hexes(cls, blacks: str, whites: str, player: Player) -> 'GamePosition':
        """
        Returns a new GamePosition object created from two hexadecimal strings.
        Blacks and whites must be 16 character strings having values in the range [0..F].
        Characters could be uppercase or lovercase.
        The two hex strings must not have overlapping bits.
        Note that the argument strings are not prefixed with '0x'

        Example:
          gp = GamePosition.new_from_hexes('ffffffffffffffff', '0000000000000000', Player.BLACK)
        """
        b = SquareSet.new_from_hex(blacks)
        w = SquareSet.new_from_hex(whites)
        return GamePosition(b, w, player)

    @classmethod
    def new_from_string(cls, s: str) -> 'GamePosition':
        """
        Returns a new GamePosition object created from a formatted string.
        The s argument must be a 66 characters long string.
        Allowed characters are: 'b', 'w', '.' for positions from 0 to 63, position 64 must be ';',
        position 65 mus be either 'b' or 'w'.
        Character 'b' means a black disc, 'w' a white one and '.' an empty square.
        The last character identifies the player that has the next move.

        Example:
          gp = GamePosition.new_from_string('..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b')
        """
        if not isinstance(s, str):
            raise TypeError('Argument s is not an instance of str')
        if not len(s) == 66:
            raise ValueError('Argument s must be a string of lenght 66')
        if not all([ True if x in ['b', 'w', '.'] else False for x in s[0:64] ]):
            raise ValueError('Argument s contains wrong character in the board definition')
        if not s[64] == ';':
            raise ValueError('Argument s must have charater ; at position 64')
        if not s[65] in ['b', 'w']:
            raise ValueError('Argument s must have charater b or w at position 66')
        b = s[-3::-1].replace('b', '1').replace('w', '0').replace('.','0')
        w = s[-3::-1].replace('b', '0').replace('w', '1').replace('.','0')
        return GamePosition(SquareSet.new_from_bin(b), SquareSet.new_from_bin(w), Player.BLACK  if s[65] == 'b' else Player.WHITE)

    @classmethod
    def read_database_file(cls, filepath: str, verbose : bool = False) -> dict:
        verboseprint = print if verbose else lambda *a, **k: None
        game_positions = {}
        count = 0
        with open(filepath) as fp:
            while True:
                count += 1
                line = fp.readline()
                if not line:
                    break
                if not line[0] == '#':
                    line = line.strip()
                    chunks = re.split(";", line)
                    if len(chunks) < 5:
                        raise SyntaxError('File {}, line {} has a sintax error. Line=\"{}\"'.format(filepath, count, line))
                    name = chunks[0]
                    board = chunks[1]
                    player = chunks[2]
                    comment = chunks[3]
                    gp = GamePosition.new_from_string(board + ';' + player)
                    game_positions[name] = (name, gp, comment)
                    verboseprint("Line {:04d}: {}".format(count, line))

        return game_positions

    def board(self) -> Board:
        if self.player is Player.BLACK:
            m = self.blacks
            o = self.whites
        else:
            m = self.whites
            o = self.blacks
        b = Board(m, o)
        if self.lms:
            b.lms = self.lms
        return b
    
    def legal_moves(self) -> SquareSet:
        if not self.lms:
            f = libreversi.game_position_x_legal_moves
            f.restype = ct.c_ulonglong
            f.argtypes = [ct.c_void_p]
            gph = GamePositionCTHelper(self.blacks, self.whites, self.player.value)
            gph_p = ct.pointer(gph)
            lms = SquareSet(f(gph_p))
            self.lms = lms
        return self.lms

    def is_move_legal(self, m: Move) -> bool:
        if not isinstance(m, Move):
            raise TypeError('Argument m is not an instance of Move')
        if m.value > Move.PA.value:
            return False
        if not self.lms:
            self.legal_moves()
        if m == Move.PA:
            return self.lms == 0
        return Square(m.value) in self.lms.list()
    
    def make_move(self, m: Move) -> 'GamePosition':
        if not isinstance(m, Move):
            raise TypeError('Argument m is not an instance of Move')
        if not self.is_move_legal(m):
            raise ValueError('Argument m is not legal')
        f = libreversi.game_position_x_make_move
        f.argtypes = [ct.c_void_p, ct.c_int, ct.c_void_p]
        current = GamePositionCTHelper(self.blacks, self.whites, self.player.value)
        move = m.value
        updated = GamePositionCTHelper(self.blacks, self.whites, self.player.value)
        current_p = ct.pointer(current)
        updated_p = ct.pointer(updated)
        f(current_p, move, updated_p)
        return GamePosition(SquareSet(updated.blacks), SquareSet(updated.whites), Player(updated.player))

    def count_difference(self) -> int:
        return self.board().count_difference()

    def final_value(self) -> int:
        return self.board().final_value()
    
    def has_any_legal_move(self) -> bool:
        return True if self.legal_moves() else False

    # TO DO GamePosition methods:
    #  - hash()
    #  - has_any_player_any_legal_move()
    #  - flips()
    #
    #  - Solvers ?
    #
    
    def print(self):
        one = SquareSet(1)
        sq = SquareSet(1)
        print('  a b c d e f g h')
        for row in range(0,8):
            print(row + 1, end = '')
            for col in range(0,8):
                if sq & self.blacks != 0:
                    c = Color.BLACK
                elif sq & self.whites != 0:
                    c = Color.WHITE
                else:
                    c = Color.EMPTY
                print(' ' + c.symbol(), end = '')
                sq <<= one
            print()
        print('  To move:', self.player)
        return
