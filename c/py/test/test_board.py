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

    def test_opponent(self):
        p = Player.BLACK
        o = p.opponent()
        self.assertEqual(Player.WHITE, o)
        
        p = Player.WHITE
        o = p.opponent()
        self.assertEqual(Player.BLACK, o)


class TestColor(unittest.TestCase):

    def test_sanity_check(self):
        self.assertEqual(1, 1)


class TestSquare(unittest.TestCase):

    def test_sanity_check(self):
        self.assertEqual(1, 1)


class TestMove(unittest.TestCase):

    def test_sanity_check(self):
        self.assertEqual(1, 1)


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
        sa = s.fill_square_at_position(0)
        self.assertEqual(sb, sa)

        sb = SquareSet(1)
        sa = s.fill_square_at_position(1)
        se = SquareSet(3)
        self.assertEqual(se, sa)

        sb = SquareSet(1)
        sa = s.fill_square_at_position(-1)
        self.assertEqual(sb, sa)

        sb = SquareSet(1)
        sa = s.fill_square_at_position(64)
        self.assertEqual(sb, sa)

    def test_remove_square_at_position(self):
        pass

    def test_count(self):
        pass

    def test_list(self):
        pass
    
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
        pass

    def test_to_bin(self):
        pass

    def test_to_signed_int(self):
        pass

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


