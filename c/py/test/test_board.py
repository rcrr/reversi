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
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_flip_horizontal()
        ar_flipped_v = SquareSet.new_from_hex('1e2222120e0a1222')
        self.assertEqual(ar_flipped_v, ar_transformed)

    def test_trans_flip_vertical(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_flip_vertical()
        ar_mirror_h = SquareSet.new_from_hex('4448507048444478')
        self.assertEqual(ar_mirror_h, ar_transformed)

    def test_trans_flip_diag_h1a8(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_flip_diag_h1a8()
        ar_flip_h1a8 = SquareSet.new_from_hex('00ff888c92610000')
        self.assertEqual(ar_flip_h1a8, ar_transformed)

    def test_trans_flip_diag_a1h8(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_flip_diag_a1h8()
        ar_flip_a1h8 = SquareSet.new_from_hex('000086493111ff00')
        self.assertEqual(ar_flip_a1h8, ar_transformed)

    def test_trans_rotate_180(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_rotate_180()
        ar_rotate_180 = SquareSet.new_from_hex('7844444870504844')
        self.assertEqual(ar_rotate_180, ar_transformed)

    def test_trans_rotate_90c(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_rotate_90c()
        ar_rotate_90c = SquareSet.new_from_hex('000061928c88ff00')
        self.assertEqual(ar_rotate_90c, ar_transformed)

    def test_trans_rotate_90a(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        ar_transformed = ar.trans_rotate_90a()
        ar_rotate_90a = SquareSet.new_from_hex('00ff113149860000')
        self.assertEqual(ar_rotate_90a, ar_transformed)

    def test_trans_identity(self):
        ar = SquareSet.new_from_hex('22120a0e1222221e')
        self.assertEqual(ar, ar.trans_identity())


class TestBoard(unittest.TestCase):

    def test_constructor(self):
        b = Board(SquareSet(1), SquareSet(2))
        self.assertIsNotNone(b)
        self.assertEqual(1, b.mover)
        self.assertEqual(2, b.opponent)
        self.assertEqual(None, b.lms)
        
        with self.assertRaises(TypeError) as context:
            Board(SquareSet(0), None)
        self.assertIsInstance(context.exception, TypeError)
        with self.assertRaises(TypeError) as context:
            Board(None, SquareSet(0))
        self.assertIsInstance(context.exception, TypeError)

        with self.assertRaises(ValueError) as context:
            Board(SquareSet(1), SquareSet(1))
        self.assertIsInstance(context.exception, ValueError)

    def test_new_from_hexes(self):
        b = Board.new_from_hexes('ffffffffffffffff', '0000000000000000')
        self.assertIsNotNone(b)

        with self.assertRaises(TypeError) as context:
            Board.new_from_hexes(None, '0000000000000000')
        self.assertIsInstance(context.exception, TypeError)
        with self.assertRaises(TypeError) as context:
            Board.new_from_hexes('0000000000000000', None)
        self.assertIsInstance(context.exception, TypeError)

        with self.assertRaises(ValueError) as context:
            Board.new_from_hexes('000000000000000g', '0000000000000000')
        self.assertIsInstance(context.exception, ValueError)
        with self.assertRaises(ValueError) as context:
            Board.new_from_hexes('0000000000000000', '000000000000000g')
        self.assertIsInstance(context.exception, ValueError)

        with self.assertRaises(ValueError) as context:
            Board.new_from_hexes('0000000000000001', '0000000000000001')
        self.assertIsInstance(context.exception, ValueError)

    def test_game_position(self):
        mover = SquareSet(1)
        opponent = SquareSet(2)
        b = Board(mover, opponent)
        gp = b.game_position()
        self.assertEqual(mover, gp.blacks)
        self.assertEqual(opponent, gp.whites)
        self.assertEqual(Player.BLACK, gp.player)

    def test_empties(self):
        b = Board.new_from_hexes('0000000000000001', '0000000000000002')
        empties = b.empties()
        expected = SquareSet.new_from_hex('fffffffffffffffc')
        self.assertEqual(expected, empties)

    def test_legal_moves(self):
        b = Board.new_from_hexes('0000000000000001', '0000000000000002')
        lms = b.legal_moves()
        expected = SquareSet.new_from_hex('0000000000000004')
        self.assertEqual(expected, lms)

    def test_count_difference(self):
        b = Board.new_from_hexes('0000000000000001', '0000000000000002')
        diff = b.count_difference()
        expected = 0
        self.assertEqual(expected, diff)

        b = Board.new_from_hexes('0000000000000001', '0000000000000000')
        diff = b.count_difference()
        expected = 1
        self.assertEqual(expected, diff)

        b = Board.new_from_hexes('0000000000000000', '0000000000000002')
        diff = b.count_difference()
        expected = -1
        self.assertEqual(expected, diff)

    def test_final_value(self):
        #
        #     seq    | mover |      opponent       | player | game_value
        # -----------+-------+---------------------+--------+------------
        #  158303776 |    62 | 4611633241869241472 |      0 |        -54
        #
        mover = SquareSet.new_from_signed_int(np.int64(62))
        opponent = SquareSet.new_from_signed_int(np.int64(4611633241869241472))
        b = Board(mover, opponent)
        expected = -54
        fv = b.final_value()
        self.assertEqual(expected, fv)

    def test_has_any_legal_move(self):
        b = Board(SquareSet(1), SquareSet(2))
        can_move = b.has_any_legal_move()
        expected = True
        self.assertEqual(expected, can_move)

        b = Board(SquareSet(2), SquareSet(1))
        can_move = b.has_any_legal_move()
        expected = False
        self.assertEqual(expected, can_move)

        b = Board.new_from_hexes('ffffffffffffffff', '0000000000000000')
        can_move = b.has_any_legal_move()
        expected = False
        self.assertEqual(expected, can_move)

        b = Board.new_from_hexes('0000000000000000', 'ffffffffffffffff')
        can_move = b.has_any_legal_move()
        expected = False
        self.assertEqual(expected, can_move)
        
    def test_print(self):
        pass
        
class TestGamePosition(unittest.TestCase):

    def test_constructor(self):
        pass

    def test_new_from_hexes(self):
        pass

    def test_new_from_string(self):
        pass

    def test_read_database_file(self):
        pass

    def test_board(self):
        pass

    def test_legal_moves(self):
        pass

    def test_is_move_legal(self):
        pass

    def test_make_move(self):
        pass

    def test_count_difference(self):
        pass

    def test_final_value(self):
        pass

    def test_has_any_legal_move(self):
        pass

    def test_print(self):
        pass

