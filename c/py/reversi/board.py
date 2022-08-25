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
        b = s[0:64].replace('b', '1').replace('w', '0').replace('.','0')
        w = s[0:64].replace('b', '0').replace('w', '1').replace('.','0')
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
    

    
ffo_positions = {
    'ffo-01': GamePosition.new_from_string('..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b')
}

#
# FFO positions from 1 to 19.
#
#ffo-01;..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b; G8:+18. H1:+12. H7:+6. A2:+6. A3:+4. B1:-4. A4:-22. G2:-24.;
#ffo-02;.bbbbbb...bwwww..bwbbwwb.wwwwwwwwwwwbbwwwwwbbwwb..bbww....bbbbb.;b; A4:+10. B2:+0. A3:-6. G7:-8. A7:-12. H7:-14. B7:-14. H2:-24.;
#ffo-03;....wb....wwbb...wwwbb.bwwbbwwwwwbbwbbwwwbbbwwwwwbbbbwbw..wwwwwb;b; D1:+2. G3:+0. B8:-2. B1:-4. C1:-4. A2:-4. A3:-6. B2:-12.;
#ffo-04;.bbbbbb.b.bbbww.bwbbbwwbbbwbwwwb.wbwwbbb..wwwbbb..wwbb....bwbbw.;b; H8:+0. A5:+0. B6:-4. B7:-4. A6:-8. B2:-12. H2:-26.;
#ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
#ffo-06;..wbbb..wwwbbb..wwwbwbw.wwbwwwb.wwbbbbbbbwwbbwb..wwwwb...bbbbbb.;b; A1:+14. H3:+14. A8:+12. H2:+8. G2:+8. H4:+4. G7:+4. A7:-22. B1:-24.;
#ffo-07;..wbbw..bwbbbb..bwwwbbbbbwwbbbbbbwwwwbbb.bbbbbbb..bbwww....bbww.;b; A6:+8. G1:+0. A1:-2. H8:-6. H7:-14. B1:-30.;
#ffo-08;...b.b..b.bbbb..bbbbwbbbbbbwwwwwbbwbbbw.bwbbbbw.bwwbbb..bwwbbw..;w; E1:+8. H2:+4. G2:+4. B2:+4. G7:+4. B1:+2. G1:-6. C1:-8.;
#ffo-09;..bwbb..w.wwbbbb.wwwbbbb.bwbbbwbbbwbwwwbwbbwbwbb..wbww....wwww..;w; G7:-8. A4:-8. B1:-16. A7:-16. B7:-26. A3:-30. G1:-38. H7:-40.;
#ffo-10;.bbbb.....wbbb..bwbwbwbbwbwbbwbbwbbwbwwwbbbwbwwb..wbbw...wwwww..;w; B2:+10. B7:+4. F1:+0. A7:-4. A2:-6. G2:-12. H2:-16. H7:-20.;
#ffo-11;...w.bwb....bbwb...bbwwbw.bbwbwbbbbwwbwb.bwwbbbbbwwwbb.bwwwwwww.;w; B3:+30. C2:+26. A6:+24. G7:+20. C3:+18. D2:+16. B4:+10. E1:+6.;
#ffo-12;..w..w..b.wwwwb.bbwwwbwwbbwbwbwwbbwbbwwwbbbbwwww..wbbb...bbbbb..;w; B7:-8. A7:-10. G7:-14. G8:-14. H2:-16. G1:-16. H1:-20.;
#ffo-13;..bbbbb..wwwbb...wwwbbbb.wbwbwbbwbbbwbbb..bwbwbb..wbwww..wwwww..;b; B7:+14. A4:+0. A3:-8. B1:-18. G8:-20. H7:-20. A2:-24.;
#ffo-14;..bbbbb...wwwb...bwwbbbb.wwwwwwwwwwbbbwwwwwbbwwb..bbww....bbbbb.;b; A3:+18. A4:+12. B1:+8. G7:-4. H7:-14. A7:-24. B7:-24. B2:-28.;
#ffo-15;....w......wwb...wwwbb.bwwwbwwwwwbbwbbwwwbbbwwwwwbbbwwbw..wwwwwb;b; G3:+4. B8:+4. F1:+0. C1:+0. C2:-2. D1:-4. B2:-8. A3:-8.;
#ffo-16;.bbbbbb.b.bbbww.bwbbbwwbbwwbbbwb.wwwbbbb..wwbbbb...www....bwb.w.;b; F8:+24. C7:+20. A5:+6. H1:+6. B6:+0. B7:-2. A6:-6. H2:-26.;
#ffo-17;.wwwww....wbbw.bbbwwwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bww....bbbb...;b; F8:+8. G2:+6. G6:-24. G1:-32. F7:-32. G7:-34. B2:-38.;
#ffo-18;.bbb......wwwb..bwwwwwbbwbwbwwbbwbbwwwwwbbbwbwwb..wbbw...wwwww..;b; G2:-2. B7:-6. F1:-8. E1:-10. H7:-12. G8:-14. G7:-14. A2:-18. B2:-18.;
#ffo-19;..wbbw..bwbbbb..bwwwwbbbbwwwbbbbb.wwwbbb..wwwwbb..bbwww....bbww.;b; B6:+8. H8:+4. B7:+0. G1:-6. B5:-16. H7:-16. B1:-24.;
