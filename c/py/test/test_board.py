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

    def test_eq(self):
        a = SquareSet(0)
        b = SquareSet(0)
        self.assertEqual(a == b, True)
        
        a = SquareSet(0)
        b = SquareSet(1)
        self.assertEqual(a == b, False)

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

    def test_eq(self):
        a = Board(SquareSet(0), SquareSet(1))
        b = Board(SquareSet(0), SquareSet(1))
        self.assertEqual(a == b, True)
        
        a = Board(SquareSet(0), SquareSet(1))
        b = Board(SquareSet(0), SquareSet(2))
        self.assertEqual(a == b, False)
        
        a = Board(SquareSet(0), SquareSet(2))
        b = Board(SquareSet(1), SquareSet(2))
        self.assertEqual(a == b, False)
        
        a = Board(SquareSet(0), SquareSet(1))
        b = Board(SquareSet(0), SquareSet(1))
        a.legal_moves()
        self.assertEqual(a == b, True)

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

    def test_has_any_player_any_legal_move(self):
        b = Board.new_from_hexes('0000000000000001', '0000000000000002')
        is_game_over = not b.has_any_player_any_legal_move()
        self.assertEqual(False, is_game_over)
        
        b = Board.new_from_hexes('0000000000000002', '0000000000000001')
        is_game_over = not b.has_any_player_any_legal_move()
        self.assertEqual(False, is_game_over)
        
        b = Board.new_from_hexes('0000000000000000', '0000000000000007')
        is_game_over = not b.has_any_player_any_legal_move()
        self.assertEqual(True, is_game_over)

    def test_hash(self):
        b = Board.new_from_hexes('0000000000000002', '0000000000000004')
        expected = np.uint64(int('0x4689879C5E2B6C8D', 16)) ^ np.uint64(int('0x1C10E0B05C7B3C49', 16))
        self.assertEqual(expected, b.hash())
        
    def test_flips(self):
        mover = SquareSet.new_from_hex('0000000000000002')
        opponent = SquareSet.new_from_hex('0000000000000004')
        move = SquareSet.new_from_hex('0000000000000001')
        b = Board(mover, opponent)
        (flipped_squares, updated_b) = b.flips(move)
        self.assertEqual(SquareSet.new_from_hex('0000000000000000'), flipped_squares)
        self.assertEqual(mover, updated_b.mover)
        self.assertEqual(opponent, updated_b.opponent)

        mover = SquareSet.new_from_hex('0000000000000002')
        opponent = SquareSet.new_from_hex('0000000000000004')
        move = SquareSet.new_from_hex('0000000000000008')
        b = Board(mover, opponent)
        expected_mover = SquareSet.new_from_hex('0000000000000000')
        expected_opponent = SquareSet.new_from_hex('000000000000000e')
        (flipped_squares, updated_b) = b.flips(move)
        self.assertEqual(SquareSet.new_from_hex('0000000000000004'), flipped_squares)
        self.assertEqual(expected_mover, updated_b.mover)
        self.assertEqual(expected_opponent, updated_b.opponent)

    def test_trans_flip_horizontal(self):
        mover = SquareSet.new_from_hex('00000000000000ff')
        opponent = SquareSet.new_from_hex('000000000000ff00')
        b = Board(mover, opponent)
        t = b.trans_flip_horizontal()
        expected_mover = SquareSet.new_from_hex('ff00000000000000')
        expected_opponent = SquareSet.new_from_hex('00ff000000000000')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_flip_vertical(self):
        mover = SquareSet.new_from_hex('0101010101010101')
        opponent = SquareSet.new_from_hex('0202020202020202')
        b = Board(mover, opponent)
        t = b.trans_flip_vertical()
        expected_mover = SquareSet.new_from_hex('8080808080808080')
        expected_opponent = SquareSet.new_from_hex('4040404040404040')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_flip_diag_h1a8(self):
        mover = SquareSet.new_from_hex('00000000000000ff')
        opponent = SquareSet.new_from_hex('000000000000ff00')
        b = Board(mover, opponent)
        t = b.trans_flip_diag_h1a8()
        expected_mover = SquareSet.new_from_hex('8080808080808080')
        expected_opponent = SquareSet.new_from_hex('4040404040404040')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_flip_diag_a1h8(self):
        mover = SquareSet.new_from_hex('00000000000000ff')
        opponent = SquareSet.new_from_hex('000000000000ff00')
        b = Board(mover, opponent)
        t = b.trans_flip_diag_a1h8()
        expected_mover = SquareSet.new_from_hex('0101010101010101')
        expected_opponent = SquareSet.new_from_hex('0202020202020202')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_rotate_180(self):
        mover = SquareSet.new_from_hex('00000000000000ef')
        opponent = SquareSet.new_from_hex('000000000000ef00')
        b = Board(mover, opponent)
        t = b.trans_rotate_180()
        expected_mover = SquareSet.new_from_hex('f700000000000000')
        expected_opponent = SquareSet.new_from_hex('00f7000000000000')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_rotate_90c(self):
        mover = SquareSet.new_from_hex('000000000000007f')
        opponent = SquareSet.new_from_hex('0000000000007f00')
        b = Board(mover, opponent)
        t = b.trans_rotate_90c()
        expected_mover = SquareSet.new_from_hex('0080808080808080')
        expected_opponent = SquareSet.new_from_hex('0040404040404040')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_rotate_90a(self):
        mover = SquareSet.new_from_hex('000000000000007f')
        opponent = SquareSet.new_from_hex('0000000000007f00')
        b = Board(mover, opponent)
        t = b.trans_rotate_90a()
        expected_mover = SquareSet.new_from_hex('0101010101010100')
        expected_opponent = SquareSet.new_from_hex('0202020202020200')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)

    def test_trans_identity(self):
        mover = SquareSet.new_from_hex('00000000000000ef')
        opponent = SquareSet.new_from_hex('000000000000ef00')
        b = Board(mover, opponent)
        t = b.trans_identity()
        expected_mover = SquareSet.new_from_hex('00000000000000ef')
        expected_opponent = SquareSet.new_from_hex('000000000000ef00')
        self.assertEqual(expected_mover, t.mover)
        self.assertEqual(expected_opponent, t.opponent)
        
    def test_print(self):
        with io.StringIO() as buf, redirect_stdout(buf):
            b = Board(SquareSet(1), SquareSet(2))
            b.print()
            output = buf.getvalue()
        
        expected = ("  a b c d e f g h\n"
                    "1 @ O . . . . . .\n"
                    "2 . . . . . . . .\n"
                    "3 . . . . . . . .\n"
                    "4 . . . . . . . .\n"
                    "5 . . . . . . . .\n"
                    "6 . . . . . . . .\n"
                    "7 . . . . . . . .\n"
                    "8 . . . . . . . .\n")

        self.assertEqual(output, expected)
        
class TestGamePosition(unittest.TestCase):

    def test_constructor(self):
        blacks = SquareSet(1)
        whites = SquareSet(2)
        player = Player.BLACK
        gp = GamePosition(blacks, whites, player)
        self.assertIsNotNone(gp)
        self.assertEqual(blacks, gp.blacks)
        self.assertEqual(whites, gp.whites)
        self.assertEqual(player, gp.player)

        with self.assertRaises(TypeError) as context:
            GamePosition(None, SquareSet(0), Player.BLACK)
        self.assertIsInstance(context.exception, TypeError)
        with self.assertRaises(TypeError) as context:
            GamePosition(SquareSet(0), None, Player.BLACK)
        self.assertIsInstance(context.exception, TypeError)
        with self.assertRaises(TypeError) as context:
            GamePosition(SquareSet(0), SquareSet(1), None)
        self.assertIsInstance(context.exception, TypeError)
        with self.assertRaises(ValueError) as context:
            GamePosition(SquareSet(1), SquareSet(1), Player.BLACK)
        self.assertIsInstance(context.exception, ValueError)

    def test_eq(self):
        a = GamePosition(SquareSet(0), SquareSet(1), Player.BLACK)
        b = GamePosition(SquareSet(0), SquareSet(1), Player.BLACK)
        self.assertEqual(a == b, True)
        
        a = GamePosition(SquareSet(0), SquareSet(1), Player.BLACK)
        b = GamePosition(SquareSet(0), SquareSet(2), Player.BLACK)
        self.assertEqual(a == b, False)
        
        a = GamePosition(SquareSet(0), SquareSet(2), Player.BLACK)
        b = GamePosition(SquareSet(1), SquareSet(2), Player.BLACK)
        self.assertEqual(a == b, False)
        
        a = GamePosition(SquareSet(0), SquareSet(2), Player.BLACK)
        b = GamePosition(SquareSet(0), SquareSet(2), Player.WHITE)
        self.assertEqual(a == b, False)
        
        a = GamePosition(SquareSet(0), SquareSet(2), Player.BLACK)
        b = object()
        self.assertEqual(a == b, False)
        
        a = GamePosition(SquareSet(0), SquareSet(1), Player.BLACK)
        b = GamePosition(SquareSet(0), SquareSet(1), Player.BLACK)
        a.legal_moves()
        self.assertEqual(a == b, True)

    def test_new_from_hexes(self):
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.BLACK)
        self.assertIsNotNone(gp)
        self.assertEqual(SquareSet(1), gp.blacks)
        self.assertEqual(SquareSet(2), gp.whites)
        self.assertEqual(Player.BLACK, gp.player)

    def test_new_from_string(self):
        gp = GamePosition.new_from_string('..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b')
        self.assertIsNotNone(gp)
        self.assertEqual(SquareSet.new_from_hex('000ed4eed4b0307c'), gp.blacks)
        self.assertEqual(SquareSet.new_from_hex('3e7028112a4e8e00'), gp.whites)
        self.assertEqual(Player.BLACK, gp.player)
        
        with self.assertRaises(TypeError) as context:
            GamePosition.new_from_string(None)
        self.assertIsInstance(context.exception, TypeError)

        with self.assertRaises(ValueError) as context:
            GamePosition.new_from_string('..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww;b')
        self.assertIsInstance(context.exception, ValueError)

        with self.assertRaises(ValueError) as context:
            GamePosition.new_from_string('..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwwwq.wwwww..;b')
        self.assertIsInstance(context.exception, ValueError)

        with self.assertRaises(ValueError) as context:
            GamePosition.new_from_string('..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;.')
        self.assertIsInstance(context.exception, ValueError)

    def test_read_database_file(self):
        
        from os.path import exists
        filepath = 'db/gpdb-ffo.txt'
        file_exists = exists(filepath)
        if not file_exists:
            print('\nFile = \'{:s}\', does not exist.'.format(filepath))
        self.assertTrue(file_exists)

        gpdb = GamePosition.read_database_file(filepath)

        self.assertIsNotNone(gpdb)
        self.assertIsInstance(gpdb, dict)

        # ffo-22;..wwww..b.wwwww.bbwwbwbbbwbwbbbbbbbwbbbb.bbwbwbb..wbbb.b....b...;w; G8:+2. A6:+0. F8:-4. A7:-4. H2:-4. B2:-6. D8:-8. B7:-14. G7:-26;
        (name, gp, desc) = gpdb['ffo-22']

        self.assertIsNotNone(name)
        self.assertIsInstance(name, str)

        self.assertIsNotNone(gp)
        self.assertIsInstance(gp, GamePosition)
        self.assertEqual('10b8d6f7f5d30100', gp.blacks.to_hex())
        self.assertEqual('000428080a2c7c3c', gp.whites.to_hex())
        self.assertEqual(Player.WHITE, gp.player)

        self.assertIsNotNone(desc)
        self.assertIsInstance(desc, str)
        
    def test_board(self):
        blacks = SquareSet(1)
        whites = SquareSet(2)
        player = Player.BLACK
        gp = GamePosition(blacks, whites, player)
        b = gp.board()
        self.assertEqual(blacks, b.mover)
        self.assertEqual(whites, b.opponent)

        blacks = SquareSet(1)
        whites = SquareSet(2)
        player = Player.WHITE
        gp = GamePosition(blacks, whites, player)
        b = gp.board()
        self.assertEqual(whites, b.mover)
        self.assertEqual(blacks, b.opponent)

    def test_legal_moves(self):
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.BLACK)
        lms = gp.legal_moves()
        expected = SquareSet(4)
        self.assertEqual(expected, lms)

    def test_is_move_legal(self):
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.BLACK)
        self.assertEqual(True, gp.is_move_legal(Move.C1))
        self.assertEqual(False, gp.is_move_legal(Move.A2))
        self.assertEqual(False, gp.is_move_legal(Move.A1))
        self.assertEqual(False, gp.is_move_legal(Move.B1))
        
        with self.assertRaises(TypeError) as context:
            gp.is_move_legal(None)
        self.assertIsInstance(context.exception, TypeError)
        
        gp = GamePosition.new_from_hexes('ffffffffffffffff', '0000000000000000', Player.BLACK)
        self.assertEqual(False, gp.is_move_legal(Move.C1))
        self.assertEqual(True, gp.is_move_legal(Move.PA))
        self.assertEqual(False, gp.is_move_legal(Move.NA))
        self.assertEqual(False, gp.is_move_legal(Move.UN))

    def test_make_move(self):
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000100', Player.BLACK)
        gp_next = gp.make_move(Move.A3)
        self.assertEqual('0000000000010101', gp_next.blacks.to_hex())
        self.assertEqual('0000000000000000', gp_next.whites.to_hex())
        self.assertEqual(Player.WHITE, gp_next.player)
        
        gp = GamePosition.new_from_hexes('ffffffffffffffff', '0000000000000000', Player.BLACK)
        gp_next = gp.make_move(Move.PA)
        self.assertEqual('ffffffffffffffff', gp_next.blacks.to_hex())
        self.assertEqual('0000000000000000', gp_next.whites.to_hex())
        self.assertEqual(Player.WHITE, gp_next.player)

        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.BLACK)
        with self.assertRaises(TypeError) as context:
            gp.make_move(None)
        self.assertIsInstance(context.exception, TypeError)
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.BLACK)
        with self.assertRaises(ValueError) as context:
            gp.make_move(Move.H8)
        self.assertIsInstance(context.exception, ValueError)
        
    def test_count_difference(self):
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000000', Player.BLACK)
        diff = gp.count_difference()
        expected = +1
        self.assertEqual(expected, diff)
        
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000000', Player.WHITE)
        diff = gp.count_difference()
        expected = -1
        self.assertEqual(expected, diff)

    def test_final_value(self):
        blacks = SquareSet.new_from_signed_int(np.int64(62))
        whites = SquareSet.new_from_signed_int(np.int64(4611633241869241472))
        gp = GamePosition(blacks, whites, Player.WHITE)
        expected = +54
        fv = gp.final_value()
        self.assertEqual(expected, fv)

    def test_has_any_legal_move(self):
        gp = GamePosition(SquareSet(1), SquareSet(2), Player.BLACK)
        can_move = gp.has_any_legal_move()
        expected = True
        self.assertEqual(expected, can_move)

    def test_has_any_player_any_legal_move(self):
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.BLACK)
        is_game_over = not gp.has_any_player_any_legal_move()
        self.assertEqual(False, is_game_over)
        
        gp = GamePosition.new_from_hexes('0000000000000001', '0000000000000002', Player.WHITE)
        is_game_over = not gp.has_any_player_any_legal_move()
        self.assertEqual(False, is_game_over)
        
        gp = GamePosition.new_from_hexes('0000000000000007', '0000000000000000', Player.WHITE)
        is_game_over = not gp.has_any_player_any_legal_move()
        self.assertEqual(True, is_game_over)
        
    def test_hash(self):
        gp = GamePosition.new_from_hexes('0000000000000002', '0000000000000004', Player.BLACK)
        expected = np.uint64(int('0x4689879C5E2B6C8D', 16)) ^ np.uint64(int('0x1C10E0B05C7B3C49', 16))
        self.assertEqual(expected, gp.hash())

    def test_flips(self):
        blacks = SquareSet.new_from_hex('0000000000000002')
        whites = SquareSet.new_from_hex('0000000000000004')
        player = Player.BLACK
        move = SquareSet.new_from_hex('0000000000000001')
        gp = GamePosition(blacks, whites, player)
        (flipped_squares, updated_gp) = gp.flips(move)
        self.assertEqual(SquareSet.new_from_hex('0000000000000000'), flipped_squares)
        self.assertEqual(blacks, updated_gp.blacks)
        self.assertEqual(whites, updated_gp.whites)
        self.assertEqual(player, updated_gp.player)

        blacks = SquareSet.new_from_hex('0000000000000002')
        whites = SquareSet.new_from_hex('0000000000000004')
        player = Player.BLACK
        move = SquareSet.new_from_hex('0000000000000008')
        gp = GamePosition(blacks, whites, player)
        (flipped_squares, updated_gp) = gp.flips(move)
        self.assertEqual(SquareSet.new_from_hex('0000000000000004'), flipped_squares)
        self.assertEqual(SquareSet.new_from_hex('000000000000000e'), updated_gp.blacks)
        self.assertEqual(SquareSet.new_from_hex('0000000000000000'), updated_gp.whites)
        self.assertEqual(Player.WHITE, updated_gp.player)

        blacks = SquareSet.new_from_hex('0000000000000002')
        whites = SquareSet.new_from_hex('0000000000000004')
        player = Player.BLACK
        move = None
        gp = GamePosition(blacks, whites, player)
        with self.assertRaises(TypeError) as context:
            gp.flips(move)
        self.assertIsInstance(context.exception, TypeError)

        blacks = SquareSet.new_from_hex('0000000000000002')
        whites = SquareSet.new_from_hex('0000000000000004')
        player = Player.BLACK
        move = SquareSet.new_from_hex('0300000000000000')
        gp = GamePosition(blacks, whites, player)
        with self.assertRaises(ValueError) as context:
            gp.flips(move)
        self.assertIsInstance(context.exception, ValueError)

        blacks = SquareSet.new_from_hex('0000000000000002')
        whites = SquareSet.new_from_hex('0000000000000004')
        player = Player.BLACK
        move = SquareSet.new_from_hex('0000000000000002')
        gp = GamePosition(blacks, whites, player)
        with self.assertRaises(ValueError) as context:
            gp.flips(move)
        self.assertIsInstance(context.exception, ValueError)

        blacks = SquareSet.new_from_hex('0000000000000002')
        whites = SquareSet.new_from_hex('0000000000000004')
        player = Player.BLACK
        move = SquareSet.new_from_hex('0000000000000000')
        gp = GamePosition(blacks, whites, player)
        (flipped_squares, updated_gp) = gp.flips(move)
        self.assertEqual(SquareSet.new_from_hex('0000000000000000'), flipped_squares)
        self.assertEqual(blacks, updated_gp.blacks)
        self.assertEqual(whites, updated_gp.whites)
        self.assertEqual(player, updated_gp.player)

        blacks = SquareSet.new_from_hex('ffffffffffffffff')
        whites = SquareSet.new_from_hex('0000000000000000')
        player = Player.BLACK
        move = SquareSet.new_from_hex('0000000000000000')
        gp = GamePosition(blacks, whites, player)
        (flipped_squares, updated_gp) = gp.flips(move)
        self.assertEqual(SquareSet.new_from_hex('0000000000000000'), flipped_squares)
        self.assertEqual(blacks, updated_gp.blacks)
        self.assertEqual(whites, updated_gp.whites)
        self.assertEqual(player, updated_gp.player)

    def test_print(self):
        with io.StringIO() as buf, redirect_stdout(buf):
            gp = GamePosition(SquareSet(1), SquareSet(2), Player.BLACK)
            gp.print()
            output = buf.getvalue()
        
        expected = ("  a b c d e f g h\n"
                    "1 @ O . . . . . .\n"
                    "2 . . . . . . . .\n"
                    "3 . . . . . . . .\n"
                    "4 . . . . . . . .\n"
                    "5 . . . . . . . .\n"
                    "6 . . . . . . . .\n"
                    "7 . . . . . . . .\n"
                    "8 . . . . . . . .\n"
                    "  To move: Player.BLACK\n")

        self.assertEqual(output, expected)

