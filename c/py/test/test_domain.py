#
# test_domain.py
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
#
# How to use the domain module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest test.test_domain
#

import unittest
import numpy.testing as npt

import twolm
import twolm.domain

from twolm.domain import *

import numpy as np

from collections import namedtuple


ar:            SquareSet = SquareSet(0x22120a0e1222221e)
ar_reflection_h:     SquareSet = SquareSet(0x1e2222120e0a1222)
ar_reflection_v:     SquareSet = SquareSet(0x4448507048444478)
ar_reflection_h1a8:  SquareSet = SquareSet(0x00ff888c92610000)
ar_reflection_a1h8:  SquareSet = SquareSet(0x000086493111ff00)
ar_rotate_180: SquareSet = SquareSet(0x7844444870504844)
ar_rotate_90c: SquareSet = SquareSet(0x000061928c88ff00)
ar_rotate_90a: SquareSet = SquareSet(0x00ff113149860000)

full:          SquareSet = SquareSet(0xffffffffffffffff)
empty:         SquareSet = SquareSet(0x0000000000000000)
sqa1:          SquareSet = SquareSet(0x0000000000000001)
sqa8:          SquareSet = SquareSet(0x0000000000000080)
sqh1:          SquareSet = SquareSet(0x0100000000000000)
sqh8:          SquareSet = SquareSet(0x8000000000000000)

row_1:         SquareSet = SquareSet(0x00000000000000ff)
row_8:         SquareSet = SquareSet(0xff00000000000000)
column_a:      SquareSet = SquareSet(0x0101010101010101)
column_h:      SquareSet = SquareSet(0x8080808080808080)

half_left:     SquareSet = SquareSet(0x0f0f0f0f0f0f0f0f)
half_right:    SquareSet = SquareSet(0xf0f0f0f0f0f0f0f0)
half_top:      SquareSet = SquareSet(0x00000000ffffffff)
half_bottom:   SquareSet = SquareSet(0xffffffff00000000)


class TestSquare(unittest.TestCase):

    def test_new_square(self):
        self.assertEqual(Square(0), Square(0))

    def test_new_from_string(self):
        self.assertEqual(Square(0), Square.new_from_str('A1'))

    def test_to_str(self):
        self.assertEqual(Square(00).to_str(), 'A1')
        self.assertEqual(Square(63).to_str(), 'H8')

    def test_as_square_set(self):
        move = Move(0)
        expected = SquareSet(0x0000000000000001)
        self.assertEqual(move.as_square_set(), expected)
        
        move = Move(63)
        expected = SquareSet(0x8000000000000000)
        self.assertEqual(move.as_square_set(), expected)
        
        move = Move(64)
        expected = SquareSet(0x0000000000000000)
        self.assertEqual(move.as_square_set(), expected)

    def test_invalid_value_raises_assertion(self):
        with self.assertRaises(AssertionError):
            Square(64)
    
    def test_invalid_name_raises_value_error(self):
        with self.assertRaises(ValueError):
            Square.new_from_str("UN")


class TestMove(unittest.TestCase):

    def test_new_move(self):
        self.assertEqual(Move(0), Move(0))

    def test_new_from_string(self):
        self.assertEqual(Move(0), Move.new_from_str('A1'))

    def test_to_str(self):
        self.assertEqual(Move(00).to_str(), 'A1')
        self.assertEqual(Move(63).to_str(), 'H8')
        self.assertEqual(Move(64).to_str(), 'PA')
        self.assertEqual(Move(65).to_str(), 'NA')
        self.assertEqual(Move(66).to_str(), 'UN')

    def test_invalid_value_raises_assertion(self):
        with self.assertRaises(AssertionError):
            Move(67)

    def test_invalid_name_raises_value_error(self):
        with self.assertRaises(ValueError):
            Move.new_from_str("ZZ")


class TestSquareSet(unittest.TestCase):

    def test_dummy(self):
        self.assertEqual(True, True)
        self.assertEqual(ar, ar)
        if False:
            ar.print()

    def test_clone(self):
        s = SquareSet(1)
        t = s.clone()
        self.assertEqual(s, t)

    def test_new_squareset(self):
        zero = np.int64(0)

        e = SquareSet(zero)
        isss = isinstance(e, SquareSet)
        self.assertEqual(isss, True)

        e = SquareSet.new_from_signed_int(zero)
        isss = isinstance(e, SquareSet)
        self.assertEqual(isss, True)
        
        e = SquareSet.new_from_hex('0000000000000000')
        isss = isinstance(e, SquareSet)
        self.assertEqual(isss, True)

        e = SquareSet(0x000000000000000)
        isss = isinstance(e, SquareSet)
        self.assertEqual(isss, True)

    def test_to_signed_int(self):
        e = SquareSet.new_from_hex('0000000000000000')
        res = e.to_signed_int()
        self.assertEqual(res, 0)

        f = SquareSet.new_from_hex('ffffffffffffffff')
        res = f.to_signed_int()
        self.assertEqual(res, -1)

    def test_count(self):
        self.assertEqual(empty.count(), 0)
        self.assertEqual(full.count(), 64)
        self.assertEqual(ar.count(), 19)

    def test_trans_reflection_diag_a1h8(self):
        self.assertEqual(empty.trans_reflection_diag_a1h8(), empty)
        self.assertEqual(full.trans_reflection_diag_a1h8(), full)
        self.assertEqual(sqa1.trans_reflection_diag_a1h8(), sqa1)
        self.assertEqual(sqh1.trans_reflection_diag_a1h8(), sqa8)
        self.assertEqual(sqa8.trans_reflection_diag_a1h8(), sqh1)
        self.assertEqual(sqh8.trans_reflection_diag_a1h8(), sqh8)
        self.assertEqual(row_1.trans_reflection_diag_a1h8(), column_a)
        self.assertEqual(row_8.trans_reflection_diag_a1h8(), column_h)
        self.assertEqual(column_a.trans_reflection_diag_a1h8(), row_1)
        self.assertEqual(column_h.trans_reflection_diag_a1h8(), row_8)
        self.assertEqual(half_left.trans_reflection_diag_a1h8(), half_top)
        self.assertEqual(half_right.trans_reflection_diag_a1h8(), half_bottom)
        self.assertEqual(half_top.trans_reflection_diag_a1h8(), half_left)
        self.assertEqual(half_bottom.trans_reflection_diag_a1h8(), half_right)

    def test_trans_reflection_diag_h1a8(self):
        self.assertEqual(empty.trans_reflection_diag_h1a8(), empty)
        self.assertEqual(full.trans_reflection_diag_h1a8(), full)
        self.assertEqual(sqa1.trans_reflection_diag_h1a8(), sqh8)
        self.assertEqual(sqh1.trans_reflection_diag_h1a8(), sqh1)
        self.assertEqual(sqa8.trans_reflection_diag_h1a8(), sqa8)
        self.assertEqual(sqh8.trans_reflection_diag_h1a8(), sqa1)

    def test_trans_reflection_horizontal(self):
        self.assertEqual(empty.trans_reflection_horizontal(), empty)
        self.assertEqual(full.trans_reflection_horizontal(), full)
        self.assertEqual(sqa1.trans_reflection_horizontal(), sqh1)
        self.assertEqual(sqh1.trans_reflection_horizontal(), sqa1)
        self.assertEqual(sqa8.trans_reflection_horizontal(), sqh8)
        self.assertEqual(sqh8.trans_reflection_horizontal(), sqa8)

    def test_trans_reflection_vertical(self):
        self.assertEqual(empty.trans_reflection_vertical(), empty)
        self.assertEqual(full.trans_reflection_vertical(), full)
        self.assertEqual(sqa1.trans_reflection_vertical(), sqa8)
        self.assertEqual(sqh1.trans_reflection_vertical(), sqh8)
        self.assertEqual(sqa8.trans_reflection_vertical(), sqa1)
        self.assertEqual(sqh8.trans_reflection_vertical(), sqh1)

    def test_trans_identity(self):
        self.assertEqual(empty.trans_identity(), empty)
        self.assertEqual(full.trans_identity(), full)
        self.assertEqual(sqa1.trans_identity(), sqa1)
        self.assertEqual(sqh1.trans_identity(), sqh1)
        self.assertEqual(sqa8.trans_identity(), sqa8)
        self.assertEqual(sqh8.trans_identity(), sqh8)

    def test_trans_rotate_180(self):
        self.assertEqual(empty.trans_rotate_180(), empty)
        self.assertEqual(full.trans_rotate_180(), full)
        self.assertEqual(sqa1.trans_rotate_180(), sqh8)
        self.assertEqual(sqh1.trans_rotate_180(), sqa8)
        self.assertEqual(sqa8.trans_rotate_180(), sqh1)
        self.assertEqual(sqh8.trans_rotate_180(), sqa1)

    def test_trans_rotate_90c(self):
        self.assertEqual(empty.trans_rotate_90c(), empty)
        self.assertEqual(full.trans_rotate_90c(), full)
        self.assertEqual(sqa1.trans_rotate_90c(), sqa8)
        self.assertEqual(sqh1.trans_rotate_90c(), sqa1)
        self.assertEqual(sqa8.trans_rotate_90c(), sqh8)
        self.assertEqual(sqh8.trans_rotate_90c(), sqh1)

    def test_trans_rotate_90a(self):
        self.assertEqual(empty.trans_rotate_90a(), empty)
        self.assertEqual(full.trans_rotate_90a(), full)
        self.assertEqual(sqa1.trans_rotate_90a(), sqh1)
        self.assertEqual(sqh1.trans_rotate_90a(), sqh8)
        self.assertEqual(sqa8.trans_rotate_90a(), sqa1)
        self.assertEqual(sqh8.trans_rotate_90a(), sqa8)

    def test_ar_transformations(self):
        self.assertEqual(ar.trans_reflection_diag_a1h8(), ar_reflection_a1h8)
        self.assertEqual(ar.trans_reflection_diag_h1a8(), ar_reflection_h1a8)
        self.assertEqual(ar.trans_reflection_horizontal(), ar_reflection_h)
        self.assertEqual(ar.trans_reflection_vertical(), ar_reflection_v)
        self.assertEqual(ar.trans_identity(), ar)
        self.assertEqual(ar.trans_rotate_180(), ar_rotate_180)
        self.assertEqual(ar.trans_rotate_90c(), ar_rotate_90c)
        self.assertEqual(ar.trans_rotate_90a(), ar_rotate_90a)

    def test_bsr(self):
        s = SquareSet(0xFFFFFFFFFFFFFFFF)
        self.assertTrue(s.bsr() == 63)

        s = SquareSet(0x0000000000000000)
        self.assertTrue(s.bsr() == -1)

        s = SquareSet(0x0000000000000001)
        self.assertTrue(s.bsr() == 0)

        s = SquareSet(0x0000000000000002)
        self.assertTrue(s.bsr() == 1)

        s = SquareSet(0x0000000000000003)
        self.assertTrue(s.bsr() == 1)

        s = SquareSet(0x0000000000000004)
        self.assertTrue(s.bsr() == 2)

        s = SquareSet(0x8000000000000000)
        self.assertTrue(s.bsr() == 63)

        s = SquareSet(0x8000000000000001)
        self.assertTrue(s.bsr() == 63)

    def test_to_square_list(self):
        s = SquareSet(0x0000000000000000)
        self.assertTrue(s.to_square_list() == [])

        s = SquareSet(0x0000000000000001)
        self.assertTrue(s.to_square_list() == [Square(0)])

        s = SquareSet(0x0000000000000003)
        self.assertTrue(s.to_square_list() == [Square(1), Square(0)])

        s = SquareSet(0x8000000000000003)
        self.assertTrue(s.to_square_list() == [Square(63), Square(1), Square(0)])

    def test_to_square_array(self):
        s = SquareSet(0x0000000000000000)
        self.assertTrue(s.to_square_array().size == 0)

        s = SquareSet(0x8000000000000003)
        self.assertTrue(np.array_equal(s.to_square_array(),  np.array([63, 1, 0])))

    def test_to_string_list(self):
        s = SquareSet(0x8000000000000003)
        squares_as_str = s.to_string_list()
        self.assertTrue(squares_as_str == ['H8', 'B1', 'A1'])

    def test_transformations(self):
        s = SquareSet(0x0000000000000107)
        expected = np.array([0x0000000000000107, # 0
                             0x00000000008080C0, # 1
                             0xE080000000000000, # 2
                             0x0301010000000000, # 3
                             0x00000000000080E0, # 4
                             0xC080800000000000, # 5
                             0x0701000000000000, # 6
                             0x0000000000010103, # 7
                             ])
        t = s.transformations()
        for i, ts in np.ndenumerate(t):
            e = SquareSet(expected[i])
            c = SquareSet(ts)
            if e != c:
                print("\n")
                print("The two square set differ at transformation index {}".format(i))
                print("Computed:")
                c.print()
                print("Expected:")
                e.print()
                self.assertTrue(False)
        self.assertTrue(True)

    def test_anti_transformations(self):
        s = SquareSet(0x0000000000000107)
        expected = np.array([0x0000000000000107, # 0
                             0x0301010000000000, # 1
                             0xE080000000000000, # 2
                             0x00000000008080C0, # 3
                             0x00000000000080E0, # 4
                             0xC080800000000000, # 5
                             0x0701000000000000, # 6
                             0x0000000000010103, # 7
                             ])
        t = s.anti_transformations()
        for i, ts in np.ndenumerate(t):
            e = SquareSet(expected[i])
            c = SquareSet(ts)
            if e != c:
                print("\n")
                print("The two square set differ at transformation index {}".format(i))
                print("Computed:")
                c.print()
                print("Expected:")
                e.print()
                self.assertTrue(False)
        self.assertTrue(True)


class TestBoard(unittest.TestCase):

    def test_new_board(self):
        mover = SquareSet(0)
        opponent = SquareSet(1)
        board = Board(mover, opponent)
        self.assertEqual(mover, board.mover)
        self.assertEqual(opponent, board.opponent)

    def test_invalid_mover_raises_type_error(self):
        with self.assertRaises(TypeError):
            Board("ERROR", SquareSet(0))

    def test_invalid_opponent_raises_type_error(self):
        with self.assertRaises(TypeError):
            Board(SquareSet(0), "ERROR")

    def test_invalid_data_raises_value_error(self):
        with self.assertRaises(ValueError):
            Board(SquareSet(1), SquareSet(1))

    def test_eq(self):
        b1 = Board(SquareSet(1), SquareSet(0))
        b2 = Board(SquareSet(1), SquareSet(0))
        b3 = Board(SquareSet(0), SquareSet(0))
        self.assertTrue(b1 == b2)
        self.assertFalse(b1 == b3)

    def test_clone(self):
        b = Board(SquareSet(0x0000000000000000), SquareSet(0xFFFFFFFFFFFFFFFF))
        c = b.clone()
        self.assertEqual(b, c)
        
    def test_empties(self):
        b = Board(SquareSet(0), SquareSet(0))
        self.assertEqual(b.empties(), SquareSet(0xFFFFFFFFFFFFFFFF))

        b = Board(SquareSet(0x0000000000000000), SquareSet(0x0000000000000001))
        self.assertEqual(b.empties(), SquareSet(0xFFFFFFFFFFFFFFFE))

        b = Board(SquareSet(0xFF00000000000001), SquareSet(0x00FF000000000002))
        self.assertEqual(b.empties(), SquareSet(0x0000FFFFFFFFFFFC))

    def test_get_mover(self):
        b = Board(SquareSet(0xFF00000000000001), SquareSet(0x00FF000000000002))
        self.assertEqual(b.get_mover(), SquareSet(0xFF00000000000001))

    def test_get_opponent(self):
        b = Board(SquareSet(0xFF00000000000001), SquareSet(0x00FF000000000002))
        self.assertEqual(b.get_opponent(), SquareSet(0x00FF000000000002))

    def test_legal_moves(self):
        b = Board(SquareSet(0x0000000000000001), SquareSet(0x0040201008040200))
        self.assertEqual(b.legal_moves(), SquareSet(0x8000000000000000))

    def test_legal_moves_bulk(self):
        data = [
            (0x0000000000000000, 0x0000000000000000, 0x0000000000000000), # 000
            (0x0000000000000001, 0x0000000000000000, 0x0000000000000000), # 001
            (0x0000000000000002, 0x0000000000000000, 0x0000000000000000), # 002
            (0x0000000000000004, 0x0000000000000000, 0x0000000000000000), # 003
            (0x0000000000000008, 0x0000000000000000, 0x0000000000000000), # 004
            (0x0000000000000000, 0x0000000000000001, 0x0000000000000000), # 005
            (0x0000000000000000, 0x0000000000000002, 0x0000000000000000), # 006
            (0x0000000000000000, 0x0000000000000004, 0x0000000000000000), # 007
            (0x0000000000000008, 0x0000000000000000, 0x0000000000000000), # 008
            (0x0000000000000000, 0x00000000000000ff, 0x0000000000000000), # 009
            (0x0000000000000001, 0x0000000000000002, 0x0000000000000004), # 010
            (0x0000000000000001, 0x0000000000000006, 0x0000000000000008), # 011
            (0x0000000000000001, 0x000000000000000e, 0x0000000000000010), # 012
            (0x0000000000000001, 0x000000000000001e, 0x0000000000000020), # 013
            (0x0000000000000001, 0x000000000000003e, 0x0000000000000040), # 014
            (0x0000000000000001, 0x000000000000007e, 0x0000000000000080), # 015
            (0x0000000000000001, 0x00000000000000fe, 0x0000000000000000), # 016
            (0x0000000000000004, 0x000000000000007a, 0x0000000000000081), # 017
            (0x0000000000000081, 0x000000000000007a, 0x0000000000000004), # 018
            (0x0000000000000002, 0x00000000000000fc, 0x0000000000000000), # 019
            (0x0000000000000001, 0x0000000000000100, 0x0000000000010000), # 020
            (0x0000000000000002, 0x0000000000000200, 0x0000000000020000), # 021
            (0x0000000000000004, 0x0000000000000400, 0x0000000000040000), # 022
            (0x0000000000000008, 0x0000000000000800, 0x0000000000080000), # 023
            (0x0000000000000001, 0x0000000000010100, 0x0000000001000000), # 024
            (0x0000000000000001, 0x0001010101010100, 0x0100000000000000), # 025
            (0x0000000000000011, 0x0011111111111100, 0x1100000000000000), # 026
            (0x0000000000000055, 0x0055555555555500, 0x5500000000000000), # 027
            (0xaa00000000000000, 0x00aaaaaaaaaaaa00, 0x00000000000000aa), # 028
            (0x0055000000000000, 0x0000555555550000, 0x0000000000005500), # 029
            (0x0000000000000001, 0x0000000000000200, 0x0000000000040000), # 030
            (0x0000000000040000, 0x0000000000000200, 0x0000000000000001), # 031
            (0x8000000000000000, 0x0040000000000000, 0x0000200000000000), # 032
            (0x0000200000000000, 0x0040000000000000, 0x8000000000000000), # 033
            (0x0000000000000001, 0x0040201008040200, 0x8000000000000000), # 034
            (0x8000000000000000, 0x0040201008040200, 0x0000000000000001), # 035
            (0x8000000000000001, 0x0040200008040200, 0x0000001000000000), # 036
            (0x0000000000000020, 0x0000000000004000, 0x0000000000800000), # 037
            (0x0000000000800000, 0x0000000000004000, 0x0000000000000020), # 038
            (0x0000000000080000, 0x0080002010000400, 0x0000400000000002), # 039
            (0x0000000000000080, 0x0000000000004000, 0x0000000000200000), # 040
            (0x0000000000200000, 0x0000000000004000, 0x0000000000000080), # 041
            (0x0000000000000080, 0x0002040810204000, 0x0100000000000000), # 042
            (0x0100000000000000, 0x0002040810204000, 0x0000000000000080), # 043
            (0x0000000010000000, 0x0002040800204000, 0x0100000000000080), # 044
            (0x0000000000000004, 0x0000000000000200, 0x0000000000010000), # 045
            (0x0000000000010000, 0x0000000000000200, 0x0000000000000004), # 046
            (0x2000000000000000, 0x0040000000000000, 0x0000800000000000), # 047
            (0x0000800000000000, 0x0040000000000000, 0x2000000000000000), # 048
            (0x0000000001000408, 0x0000000000000000, 0x0000000000000000), # 049
            (0x0000000810000000, 0x0000001008000000, 0x0000102004080000), # 050
            (0x0000000008000000, 0x0000003810000000, 0x0000280020000000), # 051
            (0x0000003010000000, 0x0000080808000000, 0x0004040404040000), # 052
            (0x0000080800000000, 0x0000003018040000, 0x0000004020280000), # 053
            (0x0000003010040000, 0x0000080808080000, 0x0004040404100400), # 054
            (0x0000080800080000, 0x000000301c040000, 0x0000404220220000), # 055
            (0x000000201c040000, 0x0000081820080000, 0x0008340440301c00), # 056
            (0x0000080820080000, 0x000020301c040000, 0x0020404202220000), # 057
            (0x000020300c040000, 0x0000080830280000, 0x000c100440503c00), # 058
            (0x0000080800280000, 0x000020307c040000, 0x002040c200420000), # 059
            (0x000020205c040000, 0x0000081820680000, 0x0008140400107c00), # 060
            (0x0000080020680000, 0x000030385c040000, 0x002044c282020000), # 061
            (0x000030385c000000, 0x00000800206e0000, 0x000c04000000ff00), # 062
            (0x00000800004e0000, 0x000030387c202000, 0x0060404600100010), # 063
            (0x000030387c002000, 0x00000800007e0000, 0x000c04008000df00), # 064
            (0x0000080000460000, 0x000030387c383000, 0x0060404400000810), # 065
            (0x0000303878383000, 0x0000080404460000, 0x000c06028281c300), # 066
            (0x0000000404460000, 0x00003c3878383000, 0x0074004080000070), # 067
            (0x0000343878383000, 0x0010080404460000, 0x180c02028281c700), # 068
            (0x0010000404460000, 0x00043c3878383000, 0x0660004080000070), # 069
            (0x00043c3800383000, 0x00100004fc460000, 0x380002c202818700), # 070
            (0x00100004dc460000, 0x00043c7820383000, 0x066a408000004c78), # 071
            (0x00043c0020383000, 0x001000fcdc460000, 0x3800c20202818700), # 072
            (0x001000dccc460000, 0x00047c2030383000, 0x066a000000004878), # 073
            (0x00047c2010203000, 0x001000dcec5e0800, 0x380002020281c708), # 074
            (0x001000d8e44e0800, 0x00047e2418303000, 0x06ea000202004038), # 075
            (0x0004660418303000, 0x001818f8e44e0800, 0x382080020281c60c), # 076
            (0x001818f8e04e0800, 0x000466041e303000, 0x06e2810301014038), # 077
            (0x000466040e200000, 0x001818f8f05e3810, 0x382000000081c768), # 078
            (0x001018f8f05e3810, 0x100c66040e200000, 0x0ee3810301014000), # 079
            (0x100c660400200000, 0x001018f8ff5e3810, 0x282000000081c668), # 080
            (0x000018f8ff5e3810, 0x181c660400200000, 0x26e3810200004000), # 081
            (0x181c660000200000, 0x000018feff5e3810, 0x000000000081c668), # 082
            (0x000018daeb523810, 0x181c6624142c0400, 0x2663810000004208), # 083
            (0x180c6624142c0400, 0x201018daeb523810, 0x402001010081c368), # 084
            (0x001018daeb523810, 0x780c6624142c0400, 0x0663810000004208), # 085
            (0x780c0604142c0400, 0x003078faeb523810, 0x00c081010081c368), # 086
            (0x003078f8eb523810, 0x780c0706142c0400, 0x060300010000420a), # 087
            (0x780c0700142c0400, 0x003078ffeb523810, 0x00c080000081c368), # 088
            (0x002070fae8503810, 0x781c0f05172f0400, 0x0603000000004302), # 089
            (0x78180705172f0400, 0x022478fae8503810, 0x04c2800000804068), # 090
            (0x020458eae0503810, 0x787827151f2f0400, 0x0483000000004306), # 091
            (0x78782511172f0400, 0x02055aeee8503810, 0x058280000080c028), # 092
            (0x02054acea8103810, 0x7878353157ef0400, 0x808200000000c306), # 093
            (0x7878351147e70000, 0x02054aeeb8183c12, 0x050280000000002c), # 094
            (0x020542e6b0102012, 0x78783d194fef1c08, 0x048200000000c324), # 095
            (0x78783c184eee1c08, 0x020543e7b1112112, 0x0482800000004060), # 096
            (0x020543e7b1110112, 0x78783c184eee7c08, 0x04820000000082e4), # 097
            (0x78783c184cec7c08, 0x020543e7b3130312, 0x0482800000000021), # 098
            (0x020543e7b3130112, 0x78783c184cec7e09, 0x04820000000080e4), # 099
            (0x78783c1848e87001, 0x020543e7b7170f1e, 0x0582800000000020), # 100
            (0x020503c7a7170f1e, 0x78f87c3858e87001, 0x8000800000008060), # 101
            (0x78f87c3858c04001, 0x020503c7a73f3f3e, 0x0502800000000040), # 102
            (0x020503c7a72f1f00, 0x78f87c3858d0607f, 0x8002800000008080), # 103
            (0x78b85c2850d0607f, 0x824523d7af2f1f00, 0x0502800000000000), # 104
            (0x824422d6ae2e1e00, 0x79b95d2951d1617f, 0x0402800000008080), # 105
            (0x79391d2951d1617f, 0x82c4e2d6ae2e1e00, 0x0402000000000000), # 106
            (0x82c0e0d4ac2c1c00, 0x793f1f2b53d3637f, 0x0400000000008080), # 107
            (0xfec4e4d4ac2c1c00, 0x013b1b2b53d3637f, 0x0000000000008080), # 108
            (0x013b1b2b53d3237f, 0xfec4e4d4ac2c5c80, 0x0000000000008000), # 109
            (0xfec4e4d4ac2c1c80, 0x013b1b2b53d3e37f, 0x0000000000000000), # 110
            (0x3e0059e9150b050e, 0x007c26162a341800, 0x4002800040406030), # 111
            (0x0001073f0e060000, 0x3e3cb84030393c3e, 0x4040408040404040), # 112
            (0xa4bc9c8c0a042c0c, 0x00406373751b1100, 0x0003000080e00210), # 113
            (0x00a0cac0d8c804fe, 0x3c08303e26343800, 0x0050050101034200), # 114
            (0x785c2e4620000000, 0x84a0d0b9dffea000, 0x0200000000015f60), # 115
            (0x00108e86bff38000, 0x04687078400c3c1c, 0xf884000000000262), # 116
            (0x00783c3a66002020, 0x000183c599ff9c08, 0x0002400000004316), # 117
            (0x0c1c3c6c160e0000, 0x2001031329713d3c, 0x000200004080c042), # 118
            (0x0014f9c0849a0000, 0x0000063e7b643c3c, 0x000300010001c242), # 119
            (0x000002160e0e123d, 0x000d1d2931712d00, 0x1f306040c080c042), # 120
        ]
        
        for i, (mover, opponent, expected_legal_moves) in enumerate(data):
            b = Board(SquareSet(mover), SquareSet(opponent))
            lm = b.legal_moves()
            elm = SquareSet(expected_legal_moves)
            if lm != elm:
                print("\nTest fails, computed and expected legal moves differ.".format())
                print("Error on data line n. {}".format(i))
                print("Board:")
                b.print()
                print("Computed legal_moves square set:")
                lm.print()
                print("Expected legal_moves square set:")
                elm.print()
            self.assertTrue(lm == elm)

    def test_flips_no_move(self):
        mover = SquareSet(0x000428080A2C7C3C)
        opponent = SquareSet(0x10B8D6F7F5D30100)
        move = SquareSet(0x0000000000000000)
        expected_flips = SquareSet(0x0000000000000000)
        b = Board(mover, opponent)
        flips, updated = b.flips(move)
        self.assertTrue(flips == expected_flips)
        self.assertIsNone(updated)

    def test_flips_inconsistent_move(self):
        mover = SquareSet(0x000428080A2C7C3C)
        opponent = SquareSet(0x10B8D6F7F5D30100)
        move = SquareSet(0x0000000000000003)
        expected_flips = SquareSet(0x0000000000000000)
        b = Board(mover, opponent)
        flips, updated = b.flips(move)
        self.assertTrue(flips == expected_flips)
        self.assertIsNone(updated)

    def test_flips_wrong_move_type(self):
        with self.assertRaises(TypeError):
            mover = SquareSet(0x000428080A2C7C3C)
            opponent = SquareSet(0x10B8D6F7F5D30100)
            move = 'WRONG TYPE'
            b = Board(mover, opponent)
            flips, updated = b.flips(move)

    def test_flips(self):
        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        move = SquareSet(0x0000000000000004)
        expected_flips = SquareSet(0x0000000000000002)
        expected_updated = Board(SquareSet(0x0000000000000000), SquareSet(0x0000000000000007))
        b = Board(mover, opponent)
        flips, updated = b.flips(move)
        self.assertTrue(flips == expected_flips)
        self.assertTrue(updated == expected_updated)
        
    def test_flips_bulk(self):
        data = [
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000000, 0x0000000000000000), # 000
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000001, 0x0000000000000000), # 001
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000002, 0x0000000000000000), # 002
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000040, 0x0000000000000000), # 003
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000080, 0x0000000000000000), # 004
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000200, 0x0000000000020000), # 005
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000008000, 0x0000001020400000), # 006
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000010000000000, 0x0000060204000000), # 007
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0001000000000000, 0x0000020400000000), # 008
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0002000000000000, 0x0000060200000000), # 009
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0040000000000000, 0x0038404040400000), # 010
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0100000000000000, 0x0000000000000000), # 011
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0200000000000000, 0x0000000000000000), # 012
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0400000000000000, 0x0000000000000000), # 013
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0800000000000000, 0x0018000000000000), # 014
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x2000000000000000, 0x0030000000000000), # 015
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x4000000000000000, 0x0020100000000000), # 016
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x8000000000000000, 0x0000000000000000), # 017
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000001, 0x0000000000000000), # 018
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000002, 0x0000000000080400), # 019
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000040, 0x0000000000002020), # 020
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000080, 0x0000000000000000), # 021
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000100, 0x0000000000000000), # 022
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000200, 0x0000000000043C00), # 023
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000008000, 0x0000000000000000), # 024
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000010000, 0x0000000002000000), # 025
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000020000, 0x00000002020C0400), # 026
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000800000, 0x0000000000000000), # 027
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000001000000, 0x0000040200000000), # 028
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000004000000, 0x0000000000040400), # 029
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000100000000, 0x0000010202040800), # 030
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000008000000000, 0x0000004000000000), # 031
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000400000000000, 0x0000004000000000), # 032
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0010000000000000, 0x0000080000000000), # 033
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0040000000000000, 0x0000000000000000), # 034
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0080000000000000, 0x0000000000000000), # 035
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0100000000000000, 0x0602040000000000), # 036
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x1000000000000000, 0x0000000000000000), # 037
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x2000000000000000, 0x0000000000000000), # 038
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x4000000000000000, 0x0000000000000000), # 039
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x8000000000000000, 0x0000000000000000), # 040
            (0xFF0315CBE5713900, 0x00FC6A341A0E067F, 0x0000800000000000, 0x00C0600000000000), # 041
        ]

        for i, (mover, opponent, move, expected_flips) in enumerate(data):
            b = Board(SquareSet(mover), SquareSet(opponent))
            m = SquareSet(move)
            ef = SquareSet(expected_flips)
            flips, updated = b.flips(m)
            if flips != ef:
                print("\nTest fails, computed and expected flips differ.".format())
                print("Error on data line n. {}".format(i))
                print("Board:")
                b.print()
                print("Move:")
                m.print()
                print("Computed flips:")
                flips.print()
                print("Expected flips:")
                ef.print()
            self.assertTrue(flips == ef)

    def test_make_move(self):
        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        move = Move(2)
        expected_updated = Board(SquareSet(0x0000000000000000), SquareSet(0x0000000000000007))
        b = Board(mover, opponent)
        updated = b.make_move(move)
        self.assertTrue(updated == expected_updated)

    def test_make_move_wrong_type(self):
        with self.assertRaises(TypeError):
            mover = SquareSet(0x0000000000000001)
            opponent = SquareSet(0x0000000000000002)
            move = 'WRONG TYPE'
            b = Board(mover, opponent)
            updated = b.make_move(move)

    def test_make_move_wrong_value(self):
        with self.assertRaises(ValueError):
            mover = SquareSet(0x0000000000000001)
            opponent = SquareSet(0x0000000000000002)
            move = Move(65)
            b = Board(mover, opponent)
            updated = b.make_move(move)

    def test_make_move_pass(self):
        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        move = Move(64)
        expected_updated = Board(SquareSet(0x0000000000000002), SquareSet(0x0000000000000001))
        b = Board(mover, opponent)
        updated = b.make_move(move)
        self.assertTrue(updated == expected_updated)

    def test_count_difference(self):
        mover = SquareSet(0xFF00000000000000)
        opponent = SquareSet(0x0000000000000002)
        expected_count = 7
        b = Board(mover, opponent)
        self.assertTrue(b.count_difference() == expected_count)

    def test_final_value(self):
        mover = SquareSet(0xFF00000000000000)
        opponent = SquareSet(0x0000000000000002)
        expected_value = 62
        b = Board(mover, opponent)
        self.assertTrue(b.final_value() == expected_value)

    def test_final_value_bulk(self):
        data = [
            (0xFFFFFFFFFFFFFFFF, 0x0000000000000000,  64), # 000
            (0x0000000000000001, 0x0000000000000002,   0), # 001
            (0x0000000000000002, 0x0000000000000001,   0), # 002
            (0x0000000000000001, 0x0000000000000000,  64), # 003
            (0x0000000000000000, 0x0000000000000001, -64), # 004
            (0x00FF000000000001, 0xFF00000000000000,  48), # 005
        ]

        for i, (mover, opponent, expected_final_value) in enumerate(data):
            b = Board(SquareSet(mover), SquareSet(opponent))
            final_value = b.final_value()
            if final_value != expected_final_value:
                print("\nTest fails, computed and expected final value differ.".format())
                print("Error on data line n. {}".format(i))
                print("Board:")
                b.print()
                print("Expected final value {} / final value {}".format(expected_final_value, final_value))
            self.assertTrue(final_value == expected_final_value)

    def test_has_to_pass_false(self):
        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        b = Board(mover, opponent)
        self.assertFalse(b.has_to_pass())

    def test_has_to_pass_true(self):
        mover = SquareSet(0x0000000000000000)
        opponent = SquareSet(0x0000000000000007)
        b = Board(mover, opponent)
        self.assertTrue(b.has_to_pass())

    def test_is_game_over(self):
        mover = SquareSet(0xFFFFFFFFFFFFFFFF)
        opponent = SquareSet(0x0000000000000000)
        b = Board(mover, opponent)
        self.assertTrue(b.is_game_over())

    def test_legal_moves_count(self):
        mover = SquareSet(0xFFFFFFFFFFFFFFFF)
        opponent = SquareSet(0x0000000000000000)
        b = Board(mover, opponent)
        self.assertTrue(b.legal_moves_count() == 0)

    def test_is_move_legal(self):
        mover = SquareSet(0xFFFFFFFFFFFFFFFF)
        opponent = SquareSet(0x0000000000000000)
        b = Board(mover, opponent)
        move = Move.new_from_str('PA')
        self.assertTrue(b.is_move_legal(move))

        mover = SquareSet(0x0000000000000000)
        opponent = SquareSet(0xFFFFFFFFFFFFFFFF)
        b = Board(mover, opponent)
        move = Move.new_from_str('PA')
        self.assertTrue(b.is_move_legal(move))

        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        b = Board(mover, opponent)
        move = Move.new_from_str('C1')
        self.assertTrue(b.is_move_legal(move))

        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        b = Board(mover, opponent)
        move = Move.new_from_str('A2')
        self.assertFalse(b.is_move_legal(move))

        mover = SquareSet(0x0000000000000001)
        opponent = SquareSet(0x0000000000000002)
        b = Board(mover, opponent)
        move = Move.new_from_str('UN')
        self.assertFalse(b.is_move_legal(move))

    def test_trans_reflection_diag_a1h8(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_reflection_diag_a1h8()
        expected = Board(SquareSet(0x0000000000000100),
                         SquareSet(0x0000000000010000))
        self.assertEqual(t, expected)

    def test_trans_reflection_diag_h1a8(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_reflection_diag_h1a8()
        expected = Board(SquareSet(0x0080000000000000),
                         SquareSet(0x0000800000000000))
        self.assertEqual(t, expected)

    def test_trans_reflection_horizontal(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_reflection_horizontal()
        expected = Board(SquareSet(0x0200000000000000),
                         SquareSet(0x0400000000000000))
        self.assertEqual(t, expected)

    def test_trans_reflection_vertical(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_reflection_vertical()
        expected = Board(SquareSet(0x0000000000000040),
                         SquareSet(0x0000000000000020))
        self.assertEqual(t, expected)

    def test_trans_identity(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_identity()
        expected = Board(SquareSet(0x0000000000000002),
                         SquareSet(0x0000000000000004))
        self.assertEqual(t, expected)

    def test_trans_rotate_180(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_rotate_180()
        expected = Board(SquareSet(0x4000000000000000),
                         SquareSet(0x2000000000000000))
        self.assertEqual(t, expected)

    def test_trans_rotate_90c(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_rotate_90c()
        expected = Board(SquareSet(0x0000000000008000),
                         SquareSet(0x0000000000800000))
        self.assertEqual(t, expected)

    def test_trans_rotate_90a(self):
        mover = SquareSet(0x0000000000000002)
        opponent = SquareSet(0x0000000000000004)
        b = Board(mover, opponent)
        t = b.trans_rotate_90a()
        expected = Board(SquareSet(0x0001000000000000),
                         SquareSet(0x0000010000000000))
        self.assertEqual(t, expected)

    def test_transformations(self):
        mover = SquareSet(0x0000000000000102)
        opponent = SquareSet(0x0000000000000005)
        b = Board(mover, opponent)
        t = b.transformations()
        em = [0x0000000000000102, # 0
              0x0000000000008040, # 1
              0x4080000000000000, # 2
              0x0201000000000000, # 3
              0x0000000000008040, # 4
              0x4080000000000000, # 5
              0x0201000000000000, # 6
              0x0000000000000102, # 7
              ]
        eo = [0x0000000000000005, # 0
              0x0000000000800080, # 1
              0xA000000000000000, # 2
              0x0100010000000000, # 3
              0x00000000000000A0, # 4
              0x8000800000000000, # 5
              0x0500000000000000, # 6
              0x0000000000010001, # 7
              ]
        e = np.array([Board(SquareSet(m), SquareSet(o)) for m, o in zip(em, eo)])
        for tb, eb in zip(t, e):
            if tb != eb:
                print("\n")
                print("Transformed:")
                tb.print()
                print("Expected:")
                eb.print()
                self.assertTrue(False)
        self.assertTrue(True)

    def test_anti_transformations(self):
        mover = SquareSet(0x0000000000000102)
        opponent = SquareSet(0x0000000000000005)
        b = Board(mover, opponent)
        t = b.anti_transformations()
        em = [0x0000000000000102, # 0
              0x0201000000000000, # 1
              0x4080000000000000, # 2
              0x0000000000008040, # 3
              0x0000000000008040, # 4
              0x4080000000000000, # 5
              0x0201000000000000, # 6
              0x0000000000000102, # 7
              ]
        eo = [0x0000000000000005, # 0
              0x0100010000000000, # 1
              0xA000000000000000, # 2
              0x0000000000800080, # 3
              0x00000000000000A0, # 4
              0x8000800000000000, # 5
              0x0500000000000000, # 6
              0x0000000000010001, # 7
              ]
        e = np.array([Board(SquareSet(m), SquareSet(o)) for m, o in zip(em, eo)])
        for tb, eb in zip(t, e):
            if tb != eb:
                print("\n")
                print("Anti-transformed:")
                tb.print()
                print("Expected:")
                eb.print()
                self.assertTrue(False)
        self.assertTrue(True)


class TestPattern(unittest.TestCase):

    def test_dummy(self):
        self.assertEqual(True, True)

    def test_init(self):
        p = Pattern('ELLE', SquareSet(0x0000000000000107))
        self.assertEqual(p, p)

    def test_init_invalid_type_arg_1_raises_assertion(self):
        with self.assertRaises(TypeError):
            p = Pattern(1, SquareSet(0x0000000000000107))

    def test_init_invalid_type_arg_2_raises_assertion(self):
        with self.assertRaises(TypeError):
            p = Pattern('ELLE', 107)

    def test_init_invalid_value_arg_2_raises_assertion(self):
        with self.assertRaises(ValueError):
            p = Pattern('ELLE', SquareSet(0x00000000008080C0))

    def test_init_attributes(self):
        mask = SquareSet(0x0000000000000107)
        all_masks = np.full(8, mask, dtype=np.uint64)
        expected_tmasks = np.array([0x0000000000000107, # 0
                                    0x00000000008080C0, # 1
                                    0xE080000000000000, # 2
                                    0x0301010000000000, # 3
                                    0x00000000000080E0, # 4
                                    0xC080800000000000, # 5
                                    0x0701000000000000, # 6
                                    0x0000000000010103, # 7
                                    ])
        p = Pattern('ELLE', mask)
        self.assertEqual(p.name, 'ELLE')
        self.assertEqual(p.mask, mask)
        npt.assert_array_equal(p.tmasks, expected_tmasks)
        self.assertEqual(p.squares, [Square(0), Square(1), Square(2), Square(8)])
        self.assertEqual(p.snames, ['A1', 'B1', 'C1', 'A2'])
        self.assertEqual(p.n_instances, 8)
        self.assertEqual(p.n_squares, 4)
        self.assertEqual(p.n_configurations, 81)
        
        computed_tmasks = [f(SquareSet(s)) for f, s in zip(p.trans_fs, all_masks)]
        npt.assert_array_equal(computed_tmasks, expected_tmasks)
        
        computed_all_masks = [f(SquareSet(s)) for f, s in zip(p.anti_trans_fs, expected_tmasks)]
        npt.assert_array_equal(computed_all_masks, all_masks)


class TestPatternPack(unittest.TestCase):

    def setUp(self):
        empty = SquareSet(0x0000000000000000)
        full  = SquareSet(0xFFFFFFFFFFFFFFFF)
        r1    = SquareSet(0x00000000000000FF)
        r2    = SquareSet(0x000000000000FF00)
        r3    = SquareSet(0x0000000000FF0000)
        r4    = SquareSet(0x00000000FF000000)
        r5    = SquareSet(0x000000FF00000000)
        r6    = SquareSet(0x0000FF0000000000)
        r7    = SquareSet(0x00FF000000000000)
        r8    = SquareSet(0xFF00000000000000)
        c1    = SquareSet(0x0101010101010101)
        c2    = SquareSet(0x0202020202020202)
        c3    = SquareSet(0x0404040404040404)
        c4    = SquareSet(0x0808080808080808)
        c5    = SquareSet(0x1010101010101010)
        c6    = SquareSet(0x2020202020202020)
        c7    = SquareSet(0x4040404040404040)
        c8    = SquareSet(0x8080808080808080)

        self.square_set_list = [ empty, full,
                                 r1, r2, r3, r4, r5, r6, r7, r8,
                                 c1, c2, c3, c4, c5, c6, c7, c8,
                                ]
        
    def tearDown(self):
        pass

    def test_elle(self):
        p = Pattern('ELLE', SquareSet(0x0000000000000107))
        pack_plan = p.pack_plan
        expected_pack_plan = np.array([
            (0x0000000000000007, 0),
            (0x0000000000000100, 5)
        ])
        npt.assert_array_equal(expected_pack_plan, pack_plan)

        packed = [pack_ss(s, p) for s in self.square_set_list]
        expected = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x000000000000000F), # full
            SquareSet(0x0000000000000007), # r1
            SquareSet(0x0000000000000008), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000009), # c1
            SquareSet(0x0000000000000002), # c2
            SquareSet(0x0000000000000004), # c3
            SquareSet(0x0000000000000000), # c4
            SquareSet(0x0000000000000000), # c5
            SquareSet(0x0000000000000000), # c6
            SquareSet(0x0000000000000000), # c7
            SquareSet(0x0000000000000000), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [unpack_ss(s, p) for s in packed]
        expected_unpacked = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x0000000000000107), # full
            SquareSet(0x0000000000000007), # r1
            SquareSet(0x0000000000000100), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000101), # c1
            SquareSet(0x0000000000000002), # c2
            SquareSet(0x0000000000000004), # c3
            SquareSet(0x0000000000000000), # c4
            SquareSet(0x0000000000000000), # c5
            SquareSet(0x0000000000000000), # c6
            SquareSet(0x0000000000000000), # c7
            SquareSet(0x0000000000000000), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    def test_edge(self):
        p = Pattern('EDGE', SquareSet(0x00000000000000FF))
        pack_plan = p.pack_plan
        expected_pack_plan = np.array([
            (0x00000000000000FF, 0)
        ])
        npt.assert_array_equal(expected_pack_plan, pack_plan)

        packed = [pack_ss(s, p) for s in self.square_set_list]
        expected = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x00000000000000FF), # full
            SquareSet(0x00000000000000FF), # r1
            SquareSet(0x0000000000000000), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000001), # c1
            SquareSet(0x0000000000000002), # c2
            SquareSet(0x0000000000000004), # c3
            SquareSet(0x0000000000000008), # c4
            SquareSet(0x0000000000000010), # c5
            SquareSet(0x0000000000000020), # c6
            SquareSet(0x0000000000000040), # c7
            SquareSet(0x0000000000000080), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [unpack_ss(s, p) for s in packed]
        expected_unpacked = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x00000000000000FF), # full
            SquareSet(0x00000000000000FF), # r1
            SquareSet(0x0000000000000000), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000001), # c1
            SquareSet(0x0000000000000002), # c2
            SquareSet(0x0000000000000004), # c3
            SquareSet(0x0000000000000008), # c4
            SquareSet(0x0000000000000010), # c5
            SquareSet(0x0000000000000020), # c6
            SquareSet(0x0000000000000040), # c7
            SquareSet(0x0000000000000080), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    def test_r2(self):
        p = Pattern('R2', SquareSet(0x000000000000FF00))
        pack_plan = p.pack_plan
        expected_pack_plan = np.array([
            (0x000000000000FF00, 8)
        ])
        npt.assert_array_equal(expected_pack_plan, pack_plan)

        packed = [pack_ss(s, p) for s in self.square_set_list]
        expected = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x00000000000000FF), # full
            SquareSet(0x0000000000000000), # r1
            SquareSet(0x00000000000000FF), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000001), # c1
            SquareSet(0x0000000000000002), # c2
            SquareSet(0x0000000000000004), # c3
            SquareSet(0x0000000000000008), # c4
            SquareSet(0x0000000000000010), # c5
            SquareSet(0x0000000000000020), # c6
            SquareSet(0x0000000000000040), # c7
            SquareSet(0x0000000000000080), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [unpack_ss(s, p) for s in packed]
        expected_unpacked = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x000000000000FF00), # full
            SquareSet(0x0000000000000000), # r1
            SquareSet(0x000000000000FF00), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000100), # c1
            SquareSet(0x0000000000000200), # c2
            SquareSet(0x0000000000000400), # c3
            SquareSet(0x0000000000000800), # c4
            SquareSet(0x0000000000001000), # c5
            SquareSet(0x0000000000002000), # c6
            SquareSet(0x0000000000004000), # c7
            SquareSet(0x0000000000008000), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    def test_corner(self):
        p = Pattern('CORNER', SquareSet(0x0000000000070707))
        pack_plan = p.pack_plan
        expected_pack_plan = np.array([
            (0x0000000000000007, 0),
            (0x0000000000000700, 5),
            (0x0000000000070000,10)
        ])
        npt.assert_array_equal(expected_pack_plan, pack_plan)

        packed = [pack_ss(s, p) for s in self.square_set_list]
        expected = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x00000000000001FF), # full
            SquareSet(0x0000000000000007), # r1
            SquareSet(0x0000000000000038), # r2
            SquareSet(0x00000000000001C0), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000000049), # c1
            SquareSet(0x0000000000000092), # c2
            SquareSet(0x0000000000000124), # c3
            SquareSet(0x0000000000000000), # c4
            SquareSet(0x0000000000000000), # c5
            SquareSet(0x0000000000000000), # c6
            SquareSet(0x0000000000000000), # c7
            SquareSet(0x0000000000000000), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [unpack_ss(s, p) for s in packed]
        expected_unpacked = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x0000000000070707), # full
            SquareSet(0x0000000000000007), # r1
            SquareSet(0x0000000000000700), # r2
            SquareSet(0x0000000000070000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x0000000000000000), # r8
            SquareSet(0x0000000000010101), # c1
            SquareSet(0x0000000000020202), # c2
            SquareSet(0x0000000000040404), # c3
            SquareSet(0x0000000000000000), # c4
            SquareSet(0x0000000000000000), # c5
            SquareSet(0x0000000000000000), # c6
            SquareSet(0x0000000000000000), # c7
            SquareSet(0x0000000000000000), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    def test_diag8(self):
        p = Pattern('DIAG8',  SquareSet(0x0102040810204080))
        pack_plan = p.pack_plan
        expected_pack_plan = np.array([
            (0x0000000000000080,  7),
            (0x0000000000004000, 13),
            (0x0000000000200000, 19),
            (0x0000000010000000, 25),
            (0x0000000800000000, 31),
            (0x0000040000000000, 37),
            (0x0002000000000000, 43),
            (0x0100000000000000, 49)
        ])
        npt.assert_array_equal(expected_pack_plan, pack_plan)

        packed = [pack_ss(s, p) for s in self.square_set_list]
        expected = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x00000000000000FF), # full
            SquareSet(0x0000000000000001), # r1
            SquareSet(0x0000000000000002), # r2
            SquareSet(0x0000000000000004), # r3
            SquareSet(0x0000000000000008), # r4
            SquareSet(0x0000000000000010), # r5
            SquareSet(0x0000000000000020), # r6
            SquareSet(0x0000000000000040), # r7
            SquareSet(0x0000000000000080), # r8
            SquareSet(0x0000000000000080), # c1
            SquareSet(0x0000000000000040), # c2
            SquareSet(0x0000000000000020), # c3
            SquareSet(0x0000000000000010), # c4
            SquareSet(0x0000000000000008), # c5
            SquareSet(0x0000000000000004), # c6
            SquareSet(0x0000000000000002), # c7
            SquareSet(0x0000000000000001), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [unpack_ss(s, p) for s in packed]
        expected_unpacked = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x0102040810204080), # full
            SquareSet(0x0000000000000080), # r1
            SquareSet(0x0000000000004000), # r2
            SquareSet(0x0000000000200000), # r3
            SquareSet(0x0000000010000000), # r4
            SquareSet(0x0000000800000000), # r5
            SquareSet(0x0000040000000000), # r6
            SquareSet(0x0002000000000000), # r7
            SquareSet(0x0100000000000000), # r8
            SquareSet(0x0100000000000000), # c1
            SquareSet(0x0002000000000000), # c2
            SquareSet(0x0000040000000000), # c3
            SquareSet(0x0000000800000000), # c4
            SquareSet(0x0000000010000000), # c5
            SquareSet(0x0000000000200000), # c6
            SquareSet(0x0000000000004000), # c7
            SquareSet(0x0000000000000080), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    def test_fourc(self):
        p = Pattern('FOURC', SquareSet(0x8100000000000081))
        pack_plan = p.pack_plan
        expected_pack_plan = np.array([
            (0x0000000000000001,  0),
            (0x0000000000000080,  6),
            (0x0100000000000000, 54),
            (0x8000000000000000, 60)
        ])
        npt.assert_array_equal(expected_pack_plan, pack_plan)

        packed = [pack_ss(s, p) for s in self.square_set_list]
        expected = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x000000000000000F), # full
            SquareSet(0x0000000000000003), # r1
            SquareSet(0x0000000000000000), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x000000000000000C), # r8
            SquareSet(0x0000000000000005), # c1
            SquareSet(0x0000000000000000), # c2
            SquareSet(0x0000000000000000), # c3
            SquareSet(0x0000000000000000), # c4
            SquareSet(0x0000000000000000), # c5
            SquareSet(0x0000000000000000), # c6
            SquareSet(0x0000000000000000), # c7
            SquareSet(0x000000000000000A), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [unpack_ss(s, p) for s in packed]
        expected_unpacked = [
            SquareSet(0x0000000000000000), # empty
            SquareSet(0x8100000000000081), # full
            SquareSet(0x0000000000000081), # r1
            SquareSet(0x0000000000000000), # r2
            SquareSet(0x0000000000000000), # r3
            SquareSet(0x0000000000000000), # r4
            SquareSet(0x0000000000000000), # r5
            SquareSet(0x0000000000000000), # r6
            SquareSet(0x0000000000000000), # r7
            SquareSet(0x8100000000000000), # r8
            SquareSet(0x0100000000000001), # c1
            SquareSet(0x0000000000000000), # c2
            SquareSet(0x0000000000000000), # c3
            SquareSet(0x0000000000000000), # c4
            SquareSet(0x0000000000000000), # c5
            SquareSet(0x0000000000000000), # c6
            SquareSet(0x0000000000000000), # c7
            SquareSet(0x8000000000000080), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)


class TestPatternSymmetries(unittest.TestCase):

    ro90c = SquareSet.trans_rotate_90c
    ro180 = SquareSet.trans_rotate_180
    ro90a = SquareSet.trans_rotate_90a
    rvert = SquareSet.trans_reflection_vertical
    rh1a8 = SquareSet.trans_reflection_diag_h1a8
    rhori = SquareSet.trans_reflection_horizontal
    ra1h8 = SquareSet.trans_reflection_diag_a1h8

    TestCase = namedtuple('TestCase', ['mask', 'name', 'expected'])
    test_data = [
        TestCase(0x0000000000000107, 'ELLE',   []),
        TestCase(0x0000000C30000000, 'ZSHAPE', [ro180]),
        TestCase(0x00000000000000FF, 'EDGE',   [rvert]),
        TestCase(0x000000000000FF00, 'R2',     [rvert]),
        TestCase(0x0000000000FF0000, 'R3',     [rvert]),
        TestCase(0x00000000FF000000, 'R4',     [rvert]),
        TestCase(0x00000000000042FF, 'XEDGE',  [rvert]),
        TestCase(0x0000000000010204, 'DIAG3',  [ra1h8]),
        TestCase(0x0000000001020408, 'DIAG4',  [ra1h8]),
        TestCase(0x0000000102040810, 'DIAG5',  [ra1h8]),
        TestCase(0x0000010204081020, 'DIAG6',  [ra1h8]),
        TestCase(0x0001020408102040, 'DIAG7',  [ra1h8]),
        TestCase(0x0102040810204080, 'DIAG8',  [ro180]),
        TestCase(0x0000000000070707, 'CORNER', [ra1h8]),
        TestCase(0x0000000000001F1F, '2X5COR', []),
        TestCase(0x0000000000003F3F, '2X6COR', []),
        TestCase(0x0000003C3C000000, 'RCT2X4', [ro180, rvert, rhori]),
        TestCase(0x000000000000C3FF, 'CASTLE', [rvert]),
        TestCase(0x030304081020C0C0, 'BARBEL', [ro180, rh1a8, ra1h8]),
        TestCase(0x010204081020C0C0, 'MACE',   [rh1a8]),
        TestCase(0x8100000000000081, 'FOURC',  [ro90c, ro180, ro90a, rvert, rh1a8, rhori, ra1h8]),
        TestCase(0x0000001818000000, 'CORE',   [ro90c, ro180, ro90a, rvert, rh1a8, rhori, ra1h8]),
        TestCase(0x0000241818240000, 'CORED',  [ro90c, ro180, ro90a, rvert, rh1a8, rhori, ra1h8]),
        TestCase(0x000008381C100000, 'COREA',  [ro90c, ro180, ro90a]),
    ]

    def setUp(self):
        pass

    def tearDown(self):
        pass
    
    def test_dummy(self):
        self.assertEqual(True, True)
    
    def test_pattern_invariance(self):

        for mask_val, name, expected in self.test_data:
            with self.subTest(name=name):
                mask = SquareSet(mask_val)
                p = Pattern(name, mask)

                if True:
                    print()
                    p.print()

                # 1. Verify symmetry function list matches
                self.assertEqual(
                    p.symmetry_fs,
                    expected,
                    f"Symmetry list mismatch for {name}"
                )
