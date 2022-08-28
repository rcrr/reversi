#
# test_board.py
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

import unittest

import reversi
import reversi.board

from reversi.board import *

import numpy as np

import io
from contextlib import redirect_stdout


class TestPlayer(unittest.TestCase):

    def test_basics(self):
        self.assertEqual(Player(0), Player.BLACK)
        self.assertEqual(Player(1), Player.WHITE)

        self.assertEqual(Player.BLACK.name, 'BLACK')
        self.assertEqual(Player.WHITE.name, 'WHITE')

        self.assertEqual(Player.BLACK.value, 0)
        self.assertEqual(Player.WHITE.value, 1)

        with self.assertRaises(ValueError) as context:
            p = Player(2)
        self.assertIsInstance(context.exception, ValueError)

        self.assertListEqual(list(Player), [Player.BLACK, Player.WHITE])

        self.assertEqual(Player.__members__['BLACK'], Player.BLACK)

    def test_opponent(self):
        p = Player.BLACK
        o = p.opponent()
        self.assertEqual(Player.WHITE, o)
        
        p = Player.WHITE
        o = p.opponent()
        self.assertEqual(Player.BLACK, o)


class TestColor(unittest.TestCase):

    def test_basics(self):
        self.assertEqual(Color(0), Color.BLACK)
        self.assertEqual(Color(1), Color.WHITE)
        self.assertEqual(Color(2), Color.EMPTY)

        self.assertEqual(Color.BLACK.name, 'BLACK')
        self.assertEqual(Color.WHITE.name, 'WHITE')
        self.assertEqual(Color.EMPTY.name, 'EMPTY')

        self.assertEqual(Color.BLACK.value, 0)
        self.assertEqual(Color.WHITE.value, 1)
        self.assertEqual(Color.EMPTY.value, 2)

        with self.assertRaises(ValueError) as context:
            c = Color(3)
        self.assertIsInstance(context.exception, ValueError)

        self.assertListEqual(list(Color), [Color.BLACK, Color.WHITE, Color.EMPTY])

        self.assertEqual(Color.__members__['EMPTY'], Color.EMPTY)

    def test_symbol(self):
        self.assertEqual(Color.BLACK.symbol(), '@')
        self.assertEqual(Color.WHITE.symbol(), 'O')
        self.assertEqual(Color.EMPTY.symbol(), '.')


class TestSquare(unittest.TestCase):

    def test_basics(self):
        self.assertEqual(Square(0), Square.A1)
        self.assertEqual(Square(63), Square.H8)

        self.assertEqual(Square.A1.name, 'A1')
        self.assertEqual(Square.H8.name, 'H8')

        self.assertEqual(Square.A1.value, 0)
        self.assertEqual(Square.H8.value, 63)

        with self.assertRaises(ValueError) as context:
            s = Square(64)
        self.assertIsInstance(context.exception, ValueError)

        sl = list(Square)
        self.assertEqual(len(sl), 64)

        self.assertEqual(Square.__members__['C7'], Square.C7)

    def test_move(self):
        self.assertEqual(Square.B2.move(), Move.B2)


class TestMove(unittest.TestCase):

    def test_basics(self):
        self.assertEqual(Move(0), Move.A1)

        self.assertEqual(Move.A1.name, 'A1')
        self.assertEqual(Move.PA.name, 'PA')

        self.assertEqual(Move.A1.value, 0)
        self.assertEqual(Move.H8.value, 63)
        self.assertEqual(Move.PA.value, 64)
        self.assertEqual(Move.NA.value, 65)
        self.assertEqual(Move.UN.value, 66)

        with self.assertRaises(ValueError) as context:
            s = Move(67)
        self.assertIsInstance(context.exception, ValueError)

        ml = list(Move)
        self.assertEqual(len(ml), 67)

        self.assertEqual(Move.__members__['UN'], Move.UN)

    def test_move(self):
        self.assertEqual(Square.B2.move(), Move.B2)


class TestSquareSet(unittest.TestCase):

    def test_sanity_check(self):
        self.assertEqual(1, 1)

    def test_constructor(self):
        self.assertIsInstance(SquareSet(0), SquareSet)

    def test_new_from_hex(self):
        self.assertIsInstance(SquareSet.new_from_hex('ffffffffffffffff'), SquareSet)

        with self.assertRaises(TypeError) as context:
            SquareSet.new_from_hex(None)
        self.assertIsInstance(context.exception, TypeError)

        with self.assertRaises(ValueError) as context:
            SquareSet.new_from_hex('fffffffffffffff')
        self.assertIsInstance(context.exception, ValueError)

        with self.assertRaises(ValueError) as context:
            SquareSet.new_from_hex('ffffffffffffffg')
        self.assertIsInstance(context.exception, ValueError)

    def test_new_from_bin(self):
        b = '0000000000000000000000000000000000000000000000000000000000000100'
        self.assertIsInstance(SquareSet.new_from_bin(b), SquareSet)

        with self.assertRaises(TypeError) as context:
            SquareSet.new_from_bin(None)
        self.assertIsInstance(context.exception, TypeError)

        with self.assertRaises(ValueError) as context:
            SquareSet.new_from_bin('00')
        self.assertIsInstance(context.exception, ValueError)

        with self.assertRaises(ValueError) as context:
            b = '000000000000000000000000000000000000000000000000000000000000010'
            SquareSet.new_from_hex(b)
        self.assertIsInstance(context.exception, ValueError)

    def test_new_from_signed_int(self):
        i = np.int64(-1)
        self.assertIsInstance(SquareSet.new_from_signed_int(i), SquareSet)

        with self.assertRaises(TypeError) as context:
            SquareSet.new_from_signed_int(-1)
        self.assertIsInstance(context.exception, TypeError)

    def test_new_from_list(self): 
        l = [Square.G3, Square.A1, Square.F2]
        self.assertIsInstance(SquareSet.new_from_list(l), SquareSet)
        self.assertIsInstance(SquareSet.new_from_list([]), SquareSet)

        with self.assertRaises(TypeError) as context:
            SquareSet.new_from_list(None)
        self.assertIsInstance(context.exception, TypeError)

        with self.assertRaises(TypeError) as context:
            l = [Square.G3, Square.A1, None, Square.F2]
            SquareSet.new_from_list(l)
        self.assertIsInstance(context.exception, TypeError)

    def test_fill_square_at_position(self):
        with self.assertRaises(TypeError) as context:
            s = SquareSet(1)
            s.fill_square_at_position(None)
        self.assertIsInstance(context.exception, TypeError)

        sb = SquareSet(1)
        sa = sb.fill_square_at_position(0)
        self.assertEqual(sb, sa)

        sb = SquareSet(1)
        sa = sb.fill_square_at_position(1)
        se = SquareSet(3)
        self.assertEqual(se, sa)

        sb = SquareSet(1)
        sa = sb.fill_square_at_position(-1)
        self.assertEqual(sb, sa)

        sb = SquareSet(1)
        sa = sb.fill_square_at_position(64)
        self.assertEqual(sb, sa)

    def test_remove_square_at_position(self):
        with self.assertRaises(TypeError) as context:
            s = SquareSet(1)
            s.remove_square_at_position(None)
        self.assertIsInstance(context.exception, TypeError)

        sb = SquareSet(3)
        sa = sb.remove_square_at_position(0)
        se = SquareSet(2)
        self.assertEqual(se, sa)

        sb = SquareSet(3)
        sa = sb.remove_square_at_position(2)
        se = SquareSet(3)
        self.assertEqual(se, sa)

        sb = SquareSet(3)
        sa = sb.remove_square_at_position(-1)
        se = SquareSet(3)
        self.assertEqual(se, sa)

        sb = SquareSet(3)
        sa = sb.remove_square_at_position(64)
        se = SquareSet(3)
        self.assertEqual(se, sa)

    def test_count(self):
        s = SquareSet.new_from_hex('0000000000000000');
        self.assertEqual(0, s.count())

        s = SquareSet.new_from_hex('ffffffffffffffff');
        self.assertEqual(64, s.count())

        s = SquareSet.new_from_hex('0000000000000001');
        self.assertEqual(1, s.count())

        s = SquareSet.new_from_hex('000000000000ff00');
        self.assertEqual(8, s.count())

    def test_list(self):
        s = SquareSet.new_from_hex('0000000000000000');
        self.assertListEqual([], s.list())
    
        s = SquareSet.new_from_hex('0000000000000001');
        self.assertListEqual([Square.A1], s.list())
    
        s = SquareSet.new_from_hex('ffffffffffffffff');
        self.assertListEqual(list(Square), s.list())
    
    def test_print(self):
        with io.StringIO() as buf, redirect_stdout(buf):
            SquareSet(4).print()
            output = buf.getvalue()
        
        expected = ("  a b c d e f g h\n"
                    "1 . . x . . . . .\n"
                    "2 . . . . . . . .\n"
                    "3 . . . . . . . .\n"
                    "4 . . . . . . . .\n"
                    "5 . . . . . . . .\n"
                    "6 . . . . . . . .\n"
                    "7 . . . . . . . .\n"
                    "8 . . . . . . . .\n")

        self.assertEqual(output, expected)
    
    def test_to_hex(self):
        h = '163726a6f80c3d17'
        self.assertEqual(h, SquareSet.new_from_hex(h).to_hex())

    def test_to_bin(self):
        b = '0001011000110111001001101010011011111000000011000011110100010111'
        self.assertEqual(b, SquareSet.new_from_bin(b).to_bin())

    def test_to_signed_int(self):
        i = np.int64(-8775502850340143849)
        self.assertEqual(i, SquareSet.new_from_signed_int(i).to_signed_int())

    def test_trans_flip_horizontal(self):
        pass

    def test_trans_flip_vertical(self):
        pass

    def test_trans_flip_diag_h1a8(self):
        pass

    def test_trans_flip_diag_a1h8(self):
        pass

    def test_trans_rotate_180(self):
        pass

    def test_trans_rotate_90c(self):
        pass

    def test_trans_rotate_90a(self):
        pass

    def test_trans_identity(self):
        pass


class TestBoard(unittest.TestCase):

    def test_sanity_check(self):
        self.assertEqual(1, 1)


class TestGamePosition(unittest.TestCase):

    def test_sanity_check(self):
        self.assertEqual(1, 1)


