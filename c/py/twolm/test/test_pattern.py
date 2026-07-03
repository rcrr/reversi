#
# test_pattern.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2026 Roberto Corradini. All rights reserved.
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
# How to use the unit tests pattern module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# time PERF=0 PYTHONPATH="./py" python3 -m unittest twolm.test.test_pattern
#

import unittest
from unittest.mock import patch, mock_open, MagicMock

from twolm.board import *
from twolm.pattern import *

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import io
import time        
import os

from collections import namedtuple

from typing import Callable, TypeAlias, List

import pydantic
from pydantic import ValidationError



ar                 = Bitboard(0x22120a0e1222221e)

ar_reflection_h    = Bitboard(0x1e2222120e0a1222)
ar_reflection_v    = Bitboard(0x4448507048444478)
ar_reflection_h1a8 = Bitboard(0x00ff888c92610000)
ar_reflection_a1h8 = Bitboard(0x000086493111ff00)
ar_rotate_180      = Bitboard(0x7844444870504844)
ar_rotate_90c      = Bitboard(0x000061928c88ff00)
ar_rotate_90a      = Bitboard(0x00ff113149860000)

full               = Bitboard(0xffffffffffffffff)
empty              = Bitboard(0x0000000000000000)
sqa1               = Bitboard(0x0000000000000001)
sqa8               = Bitboard(0x0000000000000080)
sqh1               = Bitboard(0x0100000000000000)
sqh8               = Bitboard(0x8000000000000000)

row_1              = Bitboard(0x00000000000000ff)
row_8              = Bitboard(0xff00000000000000)
column_a           = Bitboard(0x0101010101010101)
column_h           = Bitboard(0x8080808080808080)

half_left          = Bitboard(0x0f0f0f0f0f0f0f0f)
half_right         = Bitboard(0xf0f0f0f0f0f0f0f0)
half_top           = Bitboard(0x00000000ffffffff)
half_bottom        = Bitboard(0xffffffff00000000)



class TestPattern(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_dummy(self):
        self.assertEqual(True, True)

    def test_init(self):
        p = Pattern('ELLE', Bitboard(0x0000000000000107))
        self.assertEqual(p, p)
        
        p = Pattern('EDGE', Bitboard(0x00000000000000FF))
        self.assertEqual(p, p)

    def test_init_invalid_type_arg_1_raises_assertion(self):
        with self.assertRaises(TypeError):
            p = Pattern(1, Bitboard(0x0000000000000107))

    def test_init_invalid_type_arg_2_raises_assertion(self):
        with self.assertRaises(TypeError):
            p = Pattern('ELLE', 107)

    def test_init_invalid_number_of_squares(self):
        with self.assertRaises(ValueError):
            p = Pattern('TOO_BIG', Bitboard(0x000000000007FFFF))

    def test_init_attributes(self):
        mask = Bitboard(0x0000000000000107)
        all_masks = np.full(8, mask, dtype=Bitboard)
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
        nptest.assert_array_equal(p.tmasks, expected_tmasks)
        self.assertEqual(p.squares, [Square(0), Square(1), Square(2), Square(8)])
        self.assertEqual(p.snames, ['A1', 'B1', 'C1', 'A2'])
        self.assertEqual(p.n_instances, 8)
        self.assertEqual(p.n_squares, 4)
        self.assertEqual(p.n_configurations, 81)
        self.assertEqual(p.n_stabilizers, 1)
        
        computed_tmasks = [f(Bitboard(s)) for f, s in zip(p.trans_fs, all_masks)]
        nptest.assert_array_equal(computed_tmasks, expected_tmasks)
        
        computed_all_masks = [f(Bitboard(s)) for f, s in zip(p.anti_trans_fs, expected_tmasks)]
        nptest.assert_array_equal(computed_all_masks, all_masks)

        # Test snames_t field
        expected_snames_t = [
            ['a1', 'b1', 'c1', 'a2'],  # 0: ro000
            ['h1', 'h2', 'h3', 'g1'],  # 1: ro090
            ['h8', 'g8', 'f8', 'h7'],  # 2: ro180
            ['a8', 'a7', 'a6', 'b8'],  # 3: ro270
            ['h1', 'g1', 'f1', 'h2'],  # 4: fvert
            ['h8', 'h7', 'h6', 'g8'],  # 5: fh1a8
            ['a8', 'b8', 'c8', 'a7'],  # 6: fhori
            ['a1', 'a2', 'a3', 'b1'],  # 7: fa1h8
        ]
        self.assertEqual(p.snames_t, expected_snames_t)


class TestPatternPack(unittest.TestCase):

    def setUp(self):
        empty = Bitboard(0x0000000000000000)
        full  = Bitboard(0xFFFFFFFFFFFFFFFF)
        r1    = Bitboard(0x00000000000000FF)
        r2    = Bitboard(0x000000000000FF00)
        r3    = Bitboard(0x0000000000FF0000)
        r4    = Bitboard(0x00000000FF000000)
        r5    = Bitboard(0x000000FF00000000)
        r6    = Bitboard(0x0000FF0000000000)
        r7    = Bitboard(0x00FF000000000000)
        r8    = Bitboard(0xFF00000000000000)
        c1    = Bitboard(0x0101010101010101)
        c2    = Bitboard(0x0202020202020202)
        c3    = Bitboard(0x0404040404040404)
        c4    = Bitboard(0x0808080808080808)
        c5    = Bitboard(0x1010101010101010)
        c6    = Bitboard(0x2020202020202020)
        c7    = Bitboard(0x4040404040404040)
        c8    = Bitboard(0x8080808080808080)

        self.bitboard_list = [ empty, full,
                               r1, r2, r3, r4, r5, r6, r7, r8,
                               c1, c2, c3, c4, c5, c6, c7, c8,
                              ]
        
    def tearDown(self):
        pass

    def run_test_vectorized(self, p: Pattern, expected: List[Bitboard]) -> None:
        bitboard_array = np.array(self.bitboard_list, dtype=Bitboard)
        expected_array = np.array(expected, dtype=Bitboard)
        packed_array = p._pack_bb(bitboard_array)
        nptest.assert_array_equal(expected_array, packed_array)
        unpacked_array = p._unpack_bb(packed_array)
        expected_array = bitboard_array & p.mask
        nptest.assert_array_equal(expected_array, unpacked_array)

    def test_elle(self):
        p = Pattern('ELLE', Bitboard(0x0000000000000107))
        pack_masks, unpack_masks, pack_shifts = p.pack_plan
        expected_pack_masks = np.array([
            0x0000000000000007,
            0x0000000000000100
        ])
        expected_unpack_masks = np.array([
            0x0000000000000007,
            0x0000000000000008
        ])
        expected_pack_shifts = np.array([0, 5])
        nptest.assert_array_equal(expected_pack_masks, pack_masks)
        nptest.assert_array_equal(expected_unpack_masks, unpack_masks)
        nptest.assert_array_equal(expected_pack_shifts, pack_shifts)

        packed = [p._pack_bb(s) for s in self.bitboard_list]
        expected = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x000000000000000F), # full
            Bitboard(0x0000000000000007), # r1
            Bitboard(0x0000000000000008), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000009), # c1
            Bitboard(0x0000000000000002), # c2
            Bitboard(0x0000000000000004), # c3
            Bitboard(0x0000000000000000), # c4
            Bitboard(0x0000000000000000), # c5
            Bitboard(0x0000000000000000), # c6
            Bitboard(0x0000000000000000), # c7
            Bitboard(0x0000000000000000), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [p._unpack_bb(s) for s in packed]
        expected_unpacked = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x0000000000000107), # full
            Bitboard(0x0000000000000007), # r1
            Bitboard(0x0000000000000100), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000101), # c1
            Bitboard(0x0000000000000002), # c2
            Bitboard(0x0000000000000004), # c3
            Bitboard(0x0000000000000000), # c4
            Bitboard(0x0000000000000000), # c5
            Bitboard(0x0000000000000000), # c6
            Bitboard(0x0000000000000000), # c7
            Bitboard(0x0000000000000000), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)
        self.run_test_vectorized(p, expected)

    def test_edge(self):
        p = Pattern('EDGE', Bitboard(0x00000000000000FF))
        pack_masks, unpack_masks, pack_shifts = p.pack_plan
        expected_pack_masks = np.array([
            0x00000000000000ff
        ])
        expected_unpack_masks = np.array([
            0x00000000000000ff,
        ])
        expected_pack_shifts = np.array([0])
        nptest.assert_array_equal(expected_pack_masks, pack_masks)
        nptest.assert_array_equal(expected_unpack_masks, unpack_masks)
        nptest.assert_array_equal(expected_pack_shifts, pack_shifts)

        packed = [p._pack_bb(s) for s in self.bitboard_list]
        expected = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x00000000000000FF), # full
            Bitboard(0x00000000000000FF), # r1
            Bitboard(0x0000000000000000), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000001), # c1
            Bitboard(0x0000000000000002), # c2
            Bitboard(0x0000000000000004), # c3
            Bitboard(0x0000000000000008), # c4
            Bitboard(0x0000000000000010), # c5
            Bitboard(0x0000000000000020), # c6
            Bitboard(0x0000000000000040), # c7
            Bitboard(0x0000000000000080), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [p._unpack_bb(s) for s in packed]
        expected_unpacked = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x00000000000000FF), # full
            Bitboard(0x00000000000000FF), # r1
            Bitboard(0x0000000000000000), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000001), # c1
            Bitboard(0x0000000000000002), # c2
            Bitboard(0x0000000000000004), # c3
            Bitboard(0x0000000000000008), # c4
            Bitboard(0x0000000000000010), # c5
            Bitboard(0x0000000000000020), # c6
            Bitboard(0x0000000000000040), # c7
            Bitboard(0x0000000000000080), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)
        self.run_test_vectorized(p, expected)

    def test_r2(self):
        p = Pattern('R2', Bitboard(0x000000000000FF00))
        pack_masks, unpack_masks, pack_shifts = p.pack_plan
        expected_pack_masks = np.array([
            0x000000000000ff00
        ])
        expected_unpack_masks = np.array([
            0x00000000000000ff,
        ])
        expected_pack_shifts = np.array([8])
        nptest.assert_array_equal(expected_pack_masks, pack_masks)
        nptest.assert_array_equal(expected_unpack_masks, unpack_masks)
        nptest.assert_array_equal(expected_pack_shifts, pack_shifts)

        packed = [p._pack_bb(s) for s in self.bitboard_list]
        expected = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x00000000000000FF), # full
            Bitboard(0x0000000000000000), # r1
            Bitboard(0x00000000000000FF), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000001), # c1
            Bitboard(0x0000000000000002), # c2
            Bitboard(0x0000000000000004), # c3
            Bitboard(0x0000000000000008), # c4
            Bitboard(0x0000000000000010), # c5
            Bitboard(0x0000000000000020), # c6
            Bitboard(0x0000000000000040), # c7
            Bitboard(0x0000000000000080), # c8
        ]
        self.assertEqual(packed, expected)
        self.run_test_vectorized(p, expected)

        unpacked = [p._unpack_bb(s) for s in packed]
        expected_unpacked = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x000000000000FF00), # full
            Bitboard(0x0000000000000000), # r1
            Bitboard(0x000000000000FF00), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000100), # c1
            Bitboard(0x0000000000000200), # c2
            Bitboard(0x0000000000000400), # c3
            Bitboard(0x0000000000000800), # c4
            Bitboard(0x0000000000001000), # c5
            Bitboard(0x0000000000002000), # c6
            Bitboard(0x0000000000004000), # c7
            Bitboard(0x0000000000008000), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    def test_corner(self):
        p = Pattern('CORNER', Bitboard(0x0000000000070707))
        pack_masks, unpack_masks, pack_shifts = p.pack_plan
        expected_pack_masks = np.array([
            0x0000000000000007,
            0x0000000000000700,
            0x0000000000070000
        ])
        expected_unpack_masks = np.array([
            0x0000000000000007,
            0x0000000000000038,
            0x00000000000001c0
        ])
        expected_pack_shifts = np.array([0, 5, 10])
        nptest.assert_array_equal(expected_pack_masks, pack_masks)
        nptest.assert_array_equal(expected_unpack_masks, unpack_masks)
        nptest.assert_array_equal(expected_pack_shifts, pack_shifts)

        packed = [p._pack_bb(s) for s in self.bitboard_list]
        expected = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x00000000000001FF), # full
            Bitboard(0x0000000000000007), # r1
            Bitboard(0x0000000000000038), # r2
            Bitboard(0x00000000000001C0), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000000049), # c1
            Bitboard(0x0000000000000092), # c2
            Bitboard(0x0000000000000124), # c3
            Bitboard(0x0000000000000000), # c4
            Bitboard(0x0000000000000000), # c5
            Bitboard(0x0000000000000000), # c6
            Bitboard(0x0000000000000000), # c7
            Bitboard(0x0000000000000000), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [p._unpack_bb(s) for s in packed]
        expected_unpacked = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x0000000000070707), # full
            Bitboard(0x0000000000000007), # r1
            Bitboard(0x0000000000000700), # r2
            Bitboard(0x0000000000070000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x0000000000000000), # r8
            Bitboard(0x0000000000010101), # c1
            Bitboard(0x0000000000020202), # c2
            Bitboard(0x0000000000040404), # c3
            Bitboard(0x0000000000000000), # c4
            Bitboard(0x0000000000000000), # c5
            Bitboard(0x0000000000000000), # c6
            Bitboard(0x0000000000000000), # c7
            Bitboard(0x0000000000000000), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)
        self.run_test_vectorized(p, expected)

    def test_diag8(self):
        p = Pattern('DIAG8',  Bitboard(0x0102040810204080))
        pack_masks, unpack_masks, pack_shifts = p.pack_plan
        expected_pack_masks = np.array([
            0x0000000000000080,
            0x0000000000004000,
            0x0000000000200000,
            0x0000000010000000,
            0x0000000800000000,
            0x0000040000000000,
            0x0002000000000000,
            0x0100000000000000
        ])
        expected_unpack_masks = np.array([
            0x0000000000000001,
            0x0000000000000002,
            0x0000000000000004,
            0x0000000000000008,
            0x0000000000000010,
            0x0000000000000020,
            0x0000000000000040,
            0x0000000000000080
        ])
        expected_pack_shifts = np.array([7, 13, 19, 25, 31, 37, 43, 49])
        nptest.assert_array_equal(expected_pack_masks, pack_masks)
        nptest.assert_array_equal(expected_unpack_masks, unpack_masks)
        nptest.assert_array_equal(expected_pack_shifts, pack_shifts)

        packed = [p._pack_bb(s) for s in self.bitboard_list]
        expected = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x00000000000000FF), # full
            Bitboard(0x0000000000000001), # r1
            Bitboard(0x0000000000000002), # r2
            Bitboard(0x0000000000000004), # r3
            Bitboard(0x0000000000000008), # r4
            Bitboard(0x0000000000000010), # r5
            Bitboard(0x0000000000000020), # r6
            Bitboard(0x0000000000000040), # r7
            Bitboard(0x0000000000000080), # r8
            Bitboard(0x0000000000000080), # c1
            Bitboard(0x0000000000000040), # c2
            Bitboard(0x0000000000000020), # c3
            Bitboard(0x0000000000000010), # c4
            Bitboard(0x0000000000000008), # c5
            Bitboard(0x0000000000000004), # c6
            Bitboard(0x0000000000000002), # c7
            Bitboard(0x0000000000000001), # c8
        ]
        self.assertEqual(packed, expected)

        unpacked = [p._unpack_bb(s) for s in packed]
        expected_unpacked = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x0102040810204080), # full
            Bitboard(0x0000000000000080), # r1
            Bitboard(0x0000000000004000), # r2
            Bitboard(0x0000000000200000), # r3
            Bitboard(0x0000000010000000), # r4
            Bitboard(0x0000000800000000), # r5
            Bitboard(0x0000040000000000), # r6
            Bitboard(0x0002000000000000), # r7
            Bitboard(0x0100000000000000), # r8
            Bitboard(0x0100000000000000), # c1
            Bitboard(0x0002000000000000), # c2
            Bitboard(0x0000040000000000), # c3
            Bitboard(0x0000000800000000), # c4
            Bitboard(0x0000000010000000), # c5
            Bitboard(0x0000000000200000), # c6
            Bitboard(0x0000000000004000), # c7
            Bitboard(0x0000000000000080), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)
        self.run_test_vectorized(p, expected)

    def test_fourc(self):
        p = Pattern('FOURC', Bitboard(0x8100000000000081))
        pack_masks, unpack_masks, pack_shifts = p.pack_plan
        expected_pack_masks = np.array([
            0x0000000000000001,
            0x0000000000000080,
            0x0100000000000000,
            0x8000000000000000
        ])
        expected_unpack_masks = np.array([
            0x0000000000000001,
            0x0000000000000002,
            0x0000000000000004,
            0x0000000000000008
        ])
        expected_pack_shifts = np.array([0, 6, 54, 60])
        nptest.assert_array_equal(expected_pack_masks, pack_masks)
        nptest.assert_array_equal(expected_unpack_masks, unpack_masks)
        nptest.assert_array_equal(expected_pack_shifts, pack_shifts)

        packed = [p._pack_bb(s) for s in self.bitboard_list]
        expected = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x000000000000000F), # full
            Bitboard(0x0000000000000003), # r1
            Bitboard(0x0000000000000000), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x000000000000000C), # r8
            Bitboard(0x0000000000000005), # c1
            Bitboard(0x0000000000000000), # c2
            Bitboard(0x0000000000000000), # c3
            Bitboard(0x0000000000000000), # c4
            Bitboard(0x0000000000000000), # c5
            Bitboard(0x0000000000000000), # c6
            Bitboard(0x0000000000000000), # c7
            Bitboard(0x000000000000000A), # c8
        ]
        self.assertEqual(packed, expected)
        self.run_test_vectorized(p, expected)

        unpacked = [p._unpack_bb(s) for s in packed]
        expected_unpacked = [
            Bitboard(0x0000000000000000), # empty
            Bitboard(0x8100000000000081), # full
            Bitboard(0x0000000000000081), # r1
            Bitboard(0x0000000000000000), # r2
            Bitboard(0x0000000000000000), # r3
            Bitboard(0x0000000000000000), # r4
            Bitboard(0x0000000000000000), # r5
            Bitboard(0x0000000000000000), # r6
            Bitboard(0x0000000000000000), # r7
            Bitboard(0x8100000000000000), # r8
            Bitboard(0x0100000000000001), # c1
            Bitboard(0x0000000000000000), # c2
            Bitboard(0x0000000000000000), # c3
            Bitboard(0x0000000000000000), # c4
            Bitboard(0x0000000000000000), # c5
            Bitboard(0x0000000000000000), # c6
            Bitboard(0x0000000000000000), # c7
            Bitboard(0x8000000000000080), # c8
        ]
        self.assertEqual(unpacked, expected_unpacked)

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        
        N = 1_000_000
        n = len(self.bitboard_list)
        x = N // n
        bb_array = np.tile(self.bitboard_list, x + 1)
        size = len(bb_array)

        patterns = [
            Pattern('EDGE', Bitboard(0x00000000000000FF)),
            Pattern('CORNER', Bitboard(0x0000000000070707)),
            Pattern('DIAG8',  Bitboard(0x0102040810204080)),
            Pattern('XEDGE',  Bitboard(0x00000000000042FF)),
            Pattern('2X5COR', Bitboard(0x0000000000001F1F)),
        ]

        for p in patterns:
            _ = p._pack_bb(bb_array[:100])
            start_time = time.perf_counter()
            packed_array = p._pack_bb(bb_array)
            end_time = time.perf_counter()
            duration = end_time - start_time
            positions_per_sec = size / duration
            print(f"\n[PERF Pattern._pack_bb, pattern={p.name}] Processed {size:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")

            _ = p._unpack_bb(packed_array[:100])
            start_time = time.perf_counter()
            packed_array = p._unpack_bb(packed_array)
            end_time = time.perf_counter()
            duration = end_time - start_time
            positions_per_sec = size / duration
            print(f"\n[PERF Pattern._unpack_bb, pattern={p.name}] Processed {size:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")


class TestPatternSymmetries(unittest.TestCase):

    ro000 = bitboard_ro000
    ro090 = bitboard_ro090
    ro180 = bitboard_ro180
    ro270 = bitboard_ro270
    fvert = bitboard_fvert
    fh1a8 = bitboard_fh1a8
    fhori = bitboard_fhori
    fa1h8 = bitboard_fa1h8

    TestCase = namedtuple('TestCase', ['mask', 'name', 'expected'])
    test_data = [
        TestCase(0x0000000000000107, 'ELLE',   []),
        TestCase(0x0000000C30000000, 'SNAKE',  [ro180]),
        TestCase(0x00000000000000FF, 'EDGE',   [fvert]),
        TestCase(0x000000000000FF00, 'R2',     [fvert]),
        TestCase(0x0000000000FF0000, 'R3',     [fvert]),
        TestCase(0x00000000FF000000, 'R4',     [fvert]),
        TestCase(0x00000000000042FF, 'XEDGE',  [fvert]),
        TestCase(0x0000000000010204, 'DIAG3',  [fa1h8]),
        TestCase(0x0000000001020408, 'DIAG4',  [fa1h8]),
        TestCase(0x0000000102040810, 'DIAG5',  [fa1h8]),
        TestCase(0x0000010204081020, 'DIAG6',  [fa1h8]),
        TestCase(0x0001020408102040, 'DIAG7',  [fa1h8]),
        TestCase(0x0102040810204080, 'DIAG8',  [ro180]),
        TestCase(0x0000000000070707, 'CORNER', [fa1h8]),
        TestCase(0x0000000000001F1F, '2X5COR', []),
        TestCase(0x0000000000003F3F, '2X6COR', []),
        TestCase(0x0000003C3C000000, 'RCT2X4', [ro180, fvert, fhori]),
        TestCase(0x000000000000C3FF, 'CASTLE', [fvert]),
        TestCase(0x030304081020C0C0, 'BARBEL', [ro180, fh1a8, fa1h8]),
        TestCase(0x010204081020C0C0, 'MACE',   [fh1a8]),
        TestCase(0x8100000000000081, 'FOURC',  [ro090, ro180, ro270, fvert, fh1a8, fhori, fa1h8]),
        TestCase(0x0000001818000000, 'CORE',   [ro090, ro180, ro270, fvert, fh1a8, fhori, fa1h8]),
        TestCase(0x0000241818240000, 'CORED',  [ro090, ro180, ro270, fvert, fh1a8, fhori, fa1h8]),
        TestCase(0x000008381C100000, 'COREA',  [ro090, ro180, ro270]),
        TestCase(0x83800000000001C1, 'WHIRL',  [ro090, ro180, ro270]),
        TestCase(0x010100C1C1000101, 'TAU',    [fhori]),
        TestCase(0x0000000000000001, 'DOTA1',  []),
        TestCase(0x0000000000000002, 'DOTB1',  []),
        TestCase(0x0000000000000201, 'TWOND',  []),
    ]
    
    def test_pattern_invariance(self):

        for mask_val, name, expected in self.test_data:
            with self.subTest(name=name):
                mask = Bitboard(mask_val)
                p = Pattern(name, mask)

                self.assertEqual(
                    p.symmetry_fs,
                    expected,
                    f"Symmetry list mismatch for {name}"
                )

class TestPatternMdpRecord(unittest.TestCase):

    def test_elle_pattern(self):
        p = Pattern('ELLE', Bitboard(0x0000000000000107))
        expected = (
            "ELLE,0000000000000107,4,81,8,1,A1:B1:C1:A2,"
            "0000000000000107:00000000008080C0:E080000000000000:0301010000000000:"
            "00000000000080E0:C080800000000000:0701000000000000:0000000000010103,"
            "0:1:2:3:4:5:6:7,"
            "0000000000000107:00000000008080C0:E080000000000000:0301010000000000:"
            "00000000000080E0:C080800000000000:0701000000000000:0000000000010103,"
            "0:1:2:3:4:5:6:7,"
            "ro000:ro090:ro180:ro270:fvert:fh1a8:fhori:fa1h8,"
            "ro000:ro270:ro180:ro090:fvert:fh1a8:fhori:fa1h8,"
            ","
            "a1:b1:c1:a2,h1:h2:h3:g1,h8:g8:f8:h7,a8:a7:a6:b8,"
            "h1:g1:f1:h2,h8:h7:h6:g8,a8:b8:c8:a7,a1:a2:a3:b1,"
            "I0:T1:T2:T3:T4:T5:T6:T7,"
            "0"
        )
        actual = p.mdp_record()
        self.assertEqual(actual, expected)

    def test_edge_pattern(self):
        p = Pattern('EDGE', Bitboard(0x00000000000000FF))
        expected = (
            "EDGE,00000000000000FF,8,6561,4,2,A1:B1:C1:D1:E1:F1:G1:H1,"
            "00000000000000FF:8080808080808080:FF00000000000000:0101010101010101:"
            "00000000000000FF:8080808080808080:FF00000000000000:0101010101010101,"
            "0:1:2:3:0:1:2:3,"
            "00000000000000FF:8080808080808080:FF00000000000000:0101010101010101,"
            "0:1:2:3,"
            "ro000:ro090:ro180:ro270,"
            "ro000:ro270:ro180:ro090,"
            "fvert,"
            "a1:b1:c1:d1:e1:f1:g1:h1,h1:h2:h3:h4:h5:h6:h7:h8,"
            "h8:g8:f8:e8:d8:c8:b8:a8,a8:a7:a6:a5:a4:a3:a2:a1,"
            "h1:g1:f1:e1:d1:c1:b1:a1,h8:h7:h6:h5:h4:h3:h2:h1,"
            "a8:b8:c8:d8:e8:f8:g8:h8,a1:a2:a3:a4:a5:a6:a7:a8,"
            "I0:T1:T2:T3:S0:S1:S2:S3,"
            "2"
        )
        actual = p.mdp_record()
        self.assertEqual(actual, expected)

class TestTransformationsCayleyTable(unittest.TestCase):

    Tr: TypeAlias = Callable[[np.uint64], np.uint64]

    # Transformations as described in the LaTeX paper
    e:  Tr = bitboard_ro000 # 0 - e   - $e$     - Identity
    r:  Tr = bitboard_ro090 # 1 - r   - $R_90$  - 90 degree clockwise rotation
    r2: Tr = bitboard_ro180 # 2 - r2  - $R_180$ - 180 degree clockwise rotation
    r3: Tr = bitboard_ro270 # 3 - r3  - $R_270$ - 270 degree clockwise rotation
    sh: Tr = bitboard_fhori # 4 - s   - $S_h$   - horizontal reflection
    d1: Tr = bitboard_fa1h8 # 5 - sr  - $S_d1$  - principal diag ( a1h8 ) reflection
    sv: Tr = bitboard_fvert # 6 - sr2 - $S_v$   - vertical reflection
    d2: Tr = bitboard_fh1a8 # 7 - sr3 - $S_d2$  - secondary diag ( a8h1 ) reflection

    trl: list[Tr] = [e, r, r2, r3, sv, sh, d1, d2]
    trs = np.array(trl, dtype=object)
    # Mapping between the ordering of transformations. See bitboard_transformation() documentation.
    trm = np.array([0, 1, 2, 3, 4, 6, 7, 5], dtype=np.int8)

    def transformations(self, s: np.uint64) -> npt.NDArray[np.uint64]:
        r = np.array([f(s) for f in self.trl], dtype=np.uint64)
        return r

    cayley_list: list[Tr] = [
        [  e,  r, r2, r3, sv, sh, d1, d2 ],
        [  r, r2, r3, e,  d1, d2, sh, sv ],
        [ r2, r3,  e, r,  sh, sv, d2, d1 ],
        [ r3,  e,  r, r2, d2, d1, sv, sh ],
        [ sv, d2, sh, d1,  e, r2, r3,  r ],
        [ sh, d1, sv, d2, r2,  e,  r, r3 ],
        [ d1, sv, d2, sh,  r, r3,  e, r2 ],
        [ d2, sh, d1, sv, r3,  r, r2,  e ],
    ]
    cayley = np.array(cayley_list, dtype=object)

    def verify_cayley_table(self, data: np.uint64):
        """
        Validates the entire Cayley table against functional composition 
        for a specific input state.
        
        Order of operation: G (row i) is applied first, then H (column j).
        Equivalent to: y = H(G(data))
        """
        for i in range(8):
            for j in range(8):
                # Expected result from the pre-computed Cayley table
                expected = self.cayley[i, j](data)
                
                # Computed result applying transformations in sequence: G then H
                # trs[i] is G (row), trs[j] is H (column)
                computed = self.trs[j](self.trs[i](data))
                
                self.assertEqual(
                    computed, 
                    expected,
                    (
                        f"Cayley consistency error at indices [{i},{j}]:\n"
                        f"Input: {data}\n"
                        f"Table says: {self.cayley[i, j].__name__}\n"
                        f"Code computed: {self.trs[j].__name__}({self.trs[i].__name__}(data))"
                    ))

    def test_the_cog_is_working(self):
        computed = self.transformations(ar)
        expected = bitboard_transformations(ar)
        nptest.assert_array_equal(computed, expected[self.trm])

    def test_cayley_with_ar(self):
        self.verify_cayley_table(ar)

    def test_cayley_with_multiple_input(self):
        tcs = [full, empty, sqa1, sqa8, sqh1, sqh8, row_1, row_8, column_a, column_h, half_left, half_right, half_top, half_bottom]
        for s in tcs:
            self.verify_cayley_table(s)

    def test_cayley_with_sample_patterns(self):
        pattern_data = [
            ('ELLE',   0x0000000000000107),
            ('SNAKE',  0x0000000C30000000),
            ('EDGE',   0x00000000000000FF),
            ('R2',     0x000000000000FF00),
            ('R3',     0x0000000000FF0000),
            ('R4',     0x00000000FF000000),
            ('XEDGE',  0x00000000000042FF),
            ('DIAG3',  0x0000000000010204),
            ('DIAG4',  0x0000000001020408),
            ('DIAG5',  0x0000000102040810),
            ('DIAG6',  0x0000010204081020),
            ('DIAG7',  0x0001020408102040),
            ('DIAG8',  0x0102040810204080),
            ('CORNER', 0x0000000000070707),
            ('2X5COR', 0x0000000000001F1F),
            ('2X6COR', 0x0000000000003F3F),
            ('RCT2X4', 0x0000003C3C000000),
            ('CASTLE', 0x000000000000C3FF),
            ('BARBEL', 0x030304081020C0C0),
            ('MACE',   0x010204081020C0C0),
            ('FOURC',  0x8100000000000081),
            ('CORE',   0x0000001818000000),
            ('CORED',  0x0000241818240000),
            ('COREA',  0x000008381C100000),
            ('WHIRL',  0x83800000000001C1),
            ('TAU',    0x010100C1C1000101),
            ('DOTA1',  0x0000000000000001),
            ('DOTB1',  0x0000000000000002),
            ('TWOND',  0x0000000000000201),
        ]
        patterns = [Pattern(name, Bitboard(mask)) for name, mask in pattern_data]
        for p in patterns:
            self.verify_cayley_table(p.mask)

class TestPatternPrint(unittest.TestCase):
        
    def test_pattern_print(self):

        p = Pattern('EDGE', Bitboard(0x00000000000000FF))
        
        expected_output_multiline = [
            '[Pattern: name = EDGE, mask = 0x00000000000000ff]',
            '  [n_squares = 8, n_configurations = 6561, n_instances = 4, n_stabilizers = 2, type = 2]',
            '  Cells:                [A1, B1, C1, D1, E1, F1, G1, H1]',
            '  Transformed masks:    [0x00000000000000FF, 0x8080808080808080, 0xFF00000000000000, 0x0101010101010101, 0x00000000000000FF, 0x8080808080808080, 0xFF00000000000000, 0x0101010101010101]',
            '  Mask indexes:         [0, 1, 2, 3, 0, 1, 2, 3]',
            '  Unique masks:         [0x00000000000000FF, 0x8080808080808080, 0xFF00000000000000, 0x0101010101010101]',
            '  Unique mask indexes:  [0, 1, 2, 3]',
            '  Transf. functions:    [ro000, ro090, ro180, ro270]',
            '  Anti-transf. f.:      [ro000, ro270, ro180, ro090]',
            '  Symmetry functions:   [fvert]',
            '  Transformed cells:    [[0, 1, 2, 3, 4, 5, 6, 7], [7, 15, 23, 31, 39, 47, 55, 63], [63, 62, 61, 60, 59, 58, 57, 56], [56, 48, 40, 32, 24, 16, 8, 0], [7, 6, 5, 4, 3, 2, 1, 0], [63, 55, 47, 39, 31, 23, 15, 7], [56, 57, 58, 59, 60, 61, 62, 63], [0, 8, 16, 24, 32, 40, 48, 56]]',
            '  Transf. sorted cells: [[0, 1, 2, 3, 4, 5, 6, 7], [7, 15, 23, 31, 39, 47, 55, 63], [56, 57, 58, 59, 60, 61, 62, 63], [0, 8, 16, 24, 32, 40, 48, 56], [0, 1, 2, 3, 4, 5, 6, 7], [7, 15, 23, 31, 39, 47, 55, 63], [56, 57, 58, 59, 60, 61, 62, 63], [0, 8, 16, 24, 32, 40, 48, 56]]',
            '  Fingerprint:          [I0, T1, T2, T3, S0, S1, S2, S3]',
        ]
        expected_output = '\n'.join(expected_output_multiline) + '\n'
        
        with io.StringIO() as buffer:
            p.print(output=buffer)
            actual_output = buffer.getvalue()
            
        self.assertEqual(actual_output, expected_output)

class TestPatternMdpCsvFile(unittest.TestCase):

    def setUp(self):
        """Set up mock Pattern instances and common test data."""
        # Create mocks to isolate the method from the actual Pattern/Bitboard logic
        self.mock_pattern1 = MagicMock(spec=Pattern)
        self.mock_pattern1.mdp_record.return_value = "EDGE,0xff,8,6561,4,2,cells,tmasks,indexes,u_masks,u_indexes,tr,at,sf,t0,t1,t2,t3,t4,t5,t6,t7,fp,2"
        
        self.mock_pattern2 = MagicMock(spec=Pattern)
        self.mock_pattern2.mdp_record.return_value = "CORNER,0x0f,4,81,1,1,cells,tmasks,indexes,u_masks,u_indexes,tr,at,sf,t0,t1,t2,t3,t4,t5,t6,t7,fp,1"
        
        self.patterns_list = [self.mock_pattern1, self.mock_pattern2]
        self.filename = "test_output.csv"

    # --- 1. TYPE VALIDATION TESTS ---

    def test_mdp_csv_file_raises_type_error_if_patterns_not_list(self):
        """Verify TypeError is raised when the patterns argument is not a list."""
        with self.assertRaises(TypeError) as context:
            Pattern.mdp_csv_file(self.mock_pattern1, self.filename)  # Passing a single object instead of a list
        self.assertEqual(str(context.exception), "Argument patterns must be a list.")

    def test_mdp_csv_file_raises_type_error_if_filename_not_string(self):
        """Verify TypeError is raised when the filename argument is not a string."""
        with self.assertRaises(TypeError) as context:
            Pattern.mdp_csv_file(self.patterns_list, 123)  # Passing an int instead of a str
        self.assertEqual(str(context.exception), "Argument filename must be a string.")

    def test_mdp_csv_file_raises_type_error_if_elements_not_pattern_instances(self):
        """Verify TypeError is raised when the patterns list contains non-Pattern items."""
        invalid_list = [self.mock_pattern1, "not_a_pattern_object"]
        with self.assertRaises(TypeError) as context:
            Pattern.mdp_csv_file(invalid_list, self.filename)
        self.assertEqual(str(context.exception), "All elements in patterns must be Pattern instances.")

    # --- 2. I/O AND CONTENT TESTS (Mocking open) ---

    @patch("builtins.open", new_callable=mock_open)
    def test_mdp_csv_file_writes_correct_content(self, mock_file):
        """Verify the file is opened correctly and the exact CSV payload (header + records) is written."""
        # Execute the method under test
        Pattern.mdp_csv_file(self.patterns_list, self.filename)

        # 1. Assert that open() was called once with the correct filename and mode ('w')
        mock_file.assert_called_once_with(self.filename, 'w')

        # 2. Extract the file handle to analyze what was written
        handle = mock_file()
        
        # Collect all positional arguments sent to the handle.write() calls
        written_strings = [call.args[0] for call in handle.write.call_args_list]

        # 3. Assert header correctness
        expected_header = (
            "name,mask,"
            "n_squares,n_configurations,n_instances,n_stabilizers,"
            "cells,tmasks,mask_indexes,unique_masks,unique_mask_indexes,tr,at,sf,"
            "cells_t0,cells_t1,cells_t2,cells_t3,cells_t4,cells_t5,cells_t6,cells_t7,"
            "fingerprint,type\n"
        )
        self.assertEqual(written_strings[0], expected_header)

        # 4. Assert that mdp_record() was triggered on each pattern instance
        self.mock_pattern1.mdp_record.assert_called_once()
        self.mock_pattern2.mdp_record.assert_called_once()

        # 5. Assert that actual data lines contain the records appended with a newline
        self.assertEqual(written_strings[1], self.mock_pattern1.mdp_record.return_value + '\n')
        self.assertEqual(written_strings[2], self.mock_pattern2.mdp_record.return_value + '\n')

    @patch("builtins.open", new_callable=mock_open)
    def test_mdp_csv_file_with_empty_list(self, mock_file):
        """Verify that an empty patterns list results in only the header line being written."""
        Pattern.mdp_csv_file([], self.filename)
        
        handle = mock_file()
        # Only 1 write call should occur (the header)
        self.assertEqual(handle.write.call_count, 1)

class TestPatternComputeIndexes(unittest.TestCase):

    def test_compute_indexes_scalar(self):

        pat = Pattern('EDGE', Bitboard(0x00000000000000FF))

        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        pos = make_position(mover, opponent)
        position_check_collisions(pos)

        indexes = pat.compute_indexes_on_position(pos)
        
        expected = np.array([1764, 5940, 1517, 81], dtype=np.uint32)

        nptest.assert_array_equal(indexes, expected)

    def test_compute_indexes_array(self):

        N = 3

        pat = Pattern('EDGE', Bitboard(0x00000000000000FF))

        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        pos = make_position(mover, opponent)
        position_check_collisions(pos)

        pos_array = np.full(N, pos)

        indexes = pat.compute_indexes_on_position(pos_array)
        
        expected_scalar = np.array([1764, 5940, 1517, 81], dtype=np.uint32)
        expected_array = np.tile(expected_scalar, (N, 1))
        nptest.assert_array_equal(indexes, expected_array)
        
    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        
        N = 1_000_000

        pat = Pattern('EDGE', Bitboard(0x00000000000000FF))

        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        pos = make_position(mover, opponent)
        position_check_collisions(pos)

        pos_array = np.full(N, pos)

        _ = pat.compute_indexes_on_position(pos_array[:10])
        
        start_time = time.perf_counter()
        indexes = pat.compute_indexes_on_position(pos_array)
        end_time = time.perf_counter()
        
        duration = end_time - start_time
        positions_per_sec = N / duration
        
        print(f"\n[PERF Pattern.compute_indexes_on_position] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")

        expected_scalar = np.array([1764, 5940, 1517, 81], dtype=np.uint32)
        expected_array = np.tile(expected_scalar, (N, 1))
        nptest.assert_array_equal(indexes, expected_array)

class TestPatternSet(unittest.TestCase):
    """
    The TestPatternSet class contains unit tests for the PatternSet class.
    """

    def setUp(self):
        """
        Set up the test environment with sample patterns.
        """
        self.pattern1 = Pattern('ELLE', Bitboard(0x0000000000000107))
        self.pattern2 = Pattern('SNAKE', Bitboard(0x0000000C30000000))
        self.pattern3 = Pattern('EDGE', Bitboard(0x00000000000000FF))
        self.pattern_set = PatternSet('SamplePatternSet', [self.pattern1, self.pattern2, self.pattern3])

    def test_init(self):
        """
        Test the initialization of the PatternSet class.
        """
        self.assertEqual(self.pattern_set.name, 'SamplePatternSet')
        self.assertEqual(len(self.pattern_set.patterns), 3)
        self.assertIn(self.pattern1, self.pattern_set.patterns)
        self.assertIn(self.pattern2, self.pattern_set.patterns)
        self.assertIn(self.pattern3, self.pattern_set.patterns)
        self.assertEqual(self.pattern_set.patterns[0], self.pattern3)
        self.assertEqual(self.pattern_set.patterns[1], self.pattern1)
        self.assertEqual(self.pattern_set.patterns[2], self.pattern2)

    def test_init_invalid_type_arg_1_raises_assertion(self):
        """
        Test that initializing with a non-string name raises a ValidationError.
        """
        with self.assertRaises(ValidationError):
            PatternSet(1, [self.pattern1, self.pattern2, self.pattern3])

    def test_init_invalid_type_arg_2_raises_assertion(self):
        """
        Test that initializing with a non-list patterns raises a ValidationError.
        """
        with self.assertRaises(ValidationError):
            PatternSet('SamplePatternSet', 'NOT A LIST')

    def test_init_invalid_element_type_arg_2_raises_assertion(self):
        """
        Test that initializing with a list containing non-Pattern elements raises a ValidationError.
        """
        with self.assertRaises(ValidationError):
            PatternSet('SamplePatternSet', [self.pattern1, 'NOT A PATTERN', self.pattern3])

    def test_names(self):
        """
        Test the names method returns the correct list of pattern names.
        """
        expected_names = ['EDGE', 'ELLE', 'SNAKE']
        self.assertEqual(self.pattern_set.names(), expected_names)

    def test_masks(self):
        """
        Test the masks method returns the correct numpy array of pattern masks.
        """
        expected_masks = np.array([self.pattern3.mask, self.pattern1.mask, self.pattern2.mask], dtype=np.uint64)
        nptest.assert_array_equal(self.pattern_set.masks(), expected_masks)

    def test_print_summary(self):
        """
        Test the print_summary method prints the correct summary.
        """
        expected_hash = self.pattern_set.hash
        
        expected_output = [
            'PatternSet: name = SamplePatternSet, lenght = 3, hash = {}',
            '  Pattern: name = EDGE, mask = 0x00000000000000ff',
            '  Pattern: name = ELLE, mask = 0x0000000000000107',
            '  Pattern: name = SNAKE, mask = 0x0000000c30000000'
        ]
        expected_output[0] = expected_output[0].format(expected_hash)
        expected_output = '\n'.join(expected_output) + '\n'
        
        with io.StringIO() as buffer:
            self.pattern_set.print_summary(output=buffer)
            actual_output = buffer.getvalue()

        self.assertEqual(actual_output, expected_output)

    def test_print(self):
        """
        Test the print method prints the correct detailed summary.
        """
        expected_hash = self.pattern_set.hash
        
        expected_output = [
            'PatternSet: name = SamplePatternSet, lenght = 3, hash = {}',
            '[Pattern: name = EDGE, mask = 0x00000000000000ff]',
            '  [n_squares = 8, n_configurations = 6561, n_instances = 4, n_stabilizers = 2, type = 2]',
            '  Cells:                [A1, B1, C1, D1, E1, F1, G1, H1]',
            '  Transformed masks:    [0x00000000000000FF, 0x8080808080808080, 0xFF00000000000000, 0x0101010101010101, 0x00000000000000FF, 0x8080808080808080, 0xFF00000000000000, 0x0101010101010101]',
            '  Mask indexes:         [0, 1, 2, 3, 0, 1, 2, 3]',
            '  Unique masks:         [0x00000000000000FF, 0x8080808080808080, 0xFF00000000000000, 0x0101010101010101]',
            '  Unique mask indexes:  [0, 1, 2, 3]',
            '  Transf. functions:    [ro000, ro090, ro180, ro270]',
            '  Anti-transf. f.:      [ro000, ro270, ro180, ro090]',
            '  Symmetry functions:   [fvert]',
            '  Transformed cells:    [[0, 1, 2, 3, 4, 5, 6, 7], [7, 15, 23, 31, 39, 47, 55, 63], [63, 62, 61, 60, 59, 58, 57, 56], [56, 48, 40, 32, 24, 16, 8, 0], [7, 6, 5, 4, 3, 2, 1, 0], [63, 55, 47, 39, 31, 23, 15, 7], [56, 57, 58, 59, 60, 61, 62, 63], [0, 8, 16, 24, 32, 40, 48, 56]]',
            '  Transf. sorted cells: [[0, 1, 2, 3, 4, 5, 6, 7], [7, 15, 23, 31, 39, 47, 55, 63], [56, 57, 58, 59, 60, 61, 62, 63], [0, 8, 16, 24, 32, 40, 48, 56], [0, 1, 2, 3, 4, 5, 6, 7], [7, 15, 23, 31, 39, 47, 55, 63], [56, 57, 58, 59, 60, 61, 62, 63], [0, 8, 16, 24, 32, 40, 48, 56]]',
            '  Fingerprint:          [I0, T1, T2, T3, S0, S1, S2, S3]',
            '[Pattern: name = ELLE, mask = 0x0000000000000107]',
            '  [n_squares = 4, n_configurations = 81, n_instances = 8, n_stabilizers = 1, type = 0]',
            '  Cells:                [A1, B1, C1, A2]',
            '  Transformed masks:    [0x0000000000000107, 0x00000000008080C0, 0xE080000000000000, 0x0301010000000000, 0x00000000000080E0, 0xC080800000000000, 0x0701000000000000, 0x0000000000010103]',
            '  Mask indexes:         [0, 1, 2, 3, 4, 5, 6, 7]',
            '  Unique masks:         [0x0000000000000107, 0x00000000008080C0, 0xE080000000000000, 0x0301010000000000, 0x00000000000080E0, 0xC080800000000000, 0x0701000000000000, 0x0000000000010103]',
            '  Unique mask indexes:  [0, 1, 2, 3, 4, 5, 6, 7]',
            '  Transf. functions:    [ro000, ro090, ro180, ro270, fvert, fh1a8, fhori, fa1h8]',
            '  Anti-transf. f.:      [ro000, ro270, ro180, ro090, fvert, fh1a8, fhori, fa1h8]',
            '  Symmetry functions:   []',
            '  Transformed cells:    [[0, 1, 2, 8], [7, 15, 23, 6], [63, 62, 61, 55], [56, 48, 40, 57], [7, 6, 5, 15], [63, 55, 47, 62], [56, 57, 58, 48], [0, 8, 16, 1]]',
            '  Transf. sorted cells: [[0, 1, 2, 8], [6, 7, 15, 23], [55, 61, 62, 63], [40, 48, 56, 57], [5, 6, 7, 15], [47, 55, 62, 63], [48, 56, 57, 58], [0, 1, 8, 16]]',
            '  Fingerprint:          [I0, T1, T2, T3, T4, T5, T6, T7]',
            '[Pattern: name = SNAKE, mask = 0x0000000c30000000]',
            '  [n_squares = 4, n_configurations = 81, n_instances = 4, n_stabilizers = 2, type = 1]',
            '  Cells:                [E4, F4, C5, D5]',
            '  Transformed masks:    [0x0000000C30000000, 0x0000101008080000, 0x0000000C30000000, 0x0000101008080000, 0x000000300C000000, 0x0000080810100000, 0x000000300C000000, 0x0000080810100000]',
            '  Mask indexes:         [0, 1, 0, 1, 4, 5, 4, 5]',
            '  Unique masks:         [0x0000000C30000000, 0x0000101008080000, 0x000000300C000000, 0x0000080810100000]',
            '  Unique mask indexes:  [0, 1, 4, 5]',
            '  Transf. functions:    [ro000, ro090, fvert, fh1a8]',
            '  Anti-transf. f.:      [ro000, ro270, fvert, fh1a8]',
            '  Symmetry functions:   [ro180]',
            '  Transformed cells:    [[28, 29, 34, 35], [36, 44, 19, 27], [35, 34, 29, 28], [27, 19, 44, 36], [27, 26, 37, 36], [28, 20, 43, 35], [36, 37, 26, 27], [35, 43, 20, 28]]',
            '  Transf. sorted cells: [[28, 29, 34, 35], [19, 27, 36, 44], [28, 29, 34, 35], [19, 27, 36, 44], [26, 27, 36, 37], [20, 28, 35, 43], [26, 27, 36, 37], [20, 28, 35, 43]]',
            '  Fingerprint:          [I0, T1, S0, S1, T4, T5, S4, S5]',
        ]
        expected_output[0] = expected_output[0].format(expected_hash)
        expected_output = '\n'.join(expected_output) + '\n'
        
        with io.StringIO() as buffer:
            self.pattern_set.print(output=buffer)
            actual_output = buffer.getvalue()

        self.assertEqual(actual_output, expected_output)

class TestPatternComputePrincipalIndexDict(unittest.TestCase):

    def setUp(self):
        pass

    def test_init(self):
        pass

    def test_small_pattern_compute_principal_index_dict(self):
        p = Pattern('SMALL', Bitboard(0x0000000000000018))

        self.assertIsNone(p.principal_index_dict)
        
        p.compute_principal_index_dict()

        expected_principal_index_dict = np.array([0, 1, 2, 1, 4, 5, 2, 5, 8], dtype=Index)
        expected_principal_indexes = np.array([0, 1, 2, 4, 5, 8], dtype=Index)
        expected_principal_index_count = 6
        
        nptest.assert_array_equal(p.principal_index_dict, expected_principal_index_dict)
        nptest.assert_array_equal(p.principal_indexes, expected_principal_indexes)
        nptest.assert_array_equal(p.principal_index_count, expected_principal_index_count)

    def test_corner_pattern_compute_principal_index_dict(self):
        p = Pattern('CORNER', Bitboard(0x0000000000070707))

        self.assertIsNone(p.principal_index_dict)
        
        p.compute_principal_index_dict()

        self.assertEqual(p.principal_index_dict[16], 16)
        self.assertEqual(p.principal_index_dict[784], 16)

    def test_core_pattern_compute_principal_index_dict(self):
        p = Pattern('CORE', Bitboard(0x0000001818000000))

        self.assertIsNone(p.principal_index_dict)
        self.assertIsNone(p.principal_indexes)
        self.assertIsNone(p.principal_index_count)
        
        p.compute_principal_index_dict()
        
        expected_principal_indexes_list = [
             0,  1,  2,  1,  4,  5,  2,  5,  8,  1,
             4,  5, 12, 13, 14, 15, 16, 17,  2,  5,
             8, 15, 16, 17, 24, 25, 26,  1, 12, 15,
             4, 13, 16,  5, 14, 17,  4, 13, 16, 13,
            40, 41, 16, 41, 44,  5, 14, 17, 16, 41,
            44, 25, 52, 53,  2, 15, 24,  5, 16, 25,
             8, 17, 26,  5, 16, 25, 14, 41, 52, 17,
            44, 53,  8, 17, 26, 17, 44, 53, 26, 53,
            80,
        ]
        expected_principal_indexes = np.array(expected_principal_indexes_list, dtype=Index)
        nptest.assert_array_equal(p.principal_index_dict, expected_principal_indexes)
        
        unique_principal_indexes_list = [0, 1, 2, 4, 5, 8, 12, 13, 14, 15, 16, 17, 24, 25, 26, 40, 41, 44, 52, 53, 80]
        nptest.assert_array_equal(p.principal_indexes, unique_principal_indexes_list)
        self.assertEqual(p.principal_index_count, len(unique_principal_indexes_list))

    def test_diag3_pattern_compute_principal_index_dict(self):
        p = Pattern('DIAG3', Bitboard(0x0000000000010204))

        self.assertIsNone(p.principal_index_dict)
        self.assertIsNone(p.principal_indexes)
        self.assertIsNone(p.principal_index_count)
        
        p.compute_principal_index_dict()

        expected_principal_indexes_list = [
            0, 1, 2, 3, 4, 5, 6, 7, 8, 1, 10, 11, 4, 13, 14, 7, 16, 17, 2, 11, 20, 5, 14, 23, 8, 17, 26
        ]
        expected_principal_indexes = np.array(expected_principal_indexes_list, dtype=Index)
        nptest.assert_array_equal(p.principal_index_dict, expected_principal_indexes)
        
        unique_principal_indexes_list = [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 13, 14, 16, 17, 20, 23, 26]
        nptest.assert_array_equal(p.principal_indexes, unique_principal_indexes_list)
        self.assertEqual(p.principal_index_count, len(unique_principal_indexes_list))

    def test_elle_pattern_compute_principal_index_dict(self):
        p = Pattern('ELLE', Bitboard(0x0000000000000107))

        self.assertIsNone(p.principal_index_dict)
        self.assertIsNone(p.principal_indexes)
        self.assertIsNone(p.principal_index_count)
        
        p.compute_principal_index_dict()

        expected_principal_indexes_list = range(3 ** p.n_squares)
        expected_principal_indexes = np.array(expected_principal_indexes_list, dtype=Index)
        nptest.assert_array_equal(p.principal_index_dict, expected_principal_indexes)
        
        unique_principal_indexes_list = range(3 ** p.n_squares)
        nptest.assert_array_equal(p.principal_indexes, unique_principal_indexes_list)
        self.assertEqual(p.principal_index_count, len(unique_principal_indexes_list))

    def test_convert_to_principal_index(self):
        p = Pattern('CORE', Bitboard(0x0000001818000000))
        indexes = np.array([9, 0, 80, 19], dtype=Index)
        expected_principal_indexes = np.array([1, 0, 80, 5], dtype=Index)
        computed_principal_indexes = p.convert_to_principal_index(indexes)
        nptest.assert_array_equal(computed_principal_indexes, expected_principal_indexes)

class TestPatternSetComputePrincipalIndexes(unittest.TestCase):

    def setUp(self):
        self.pattern1 = Pattern('ELLE', Bitboard(0x0000000000000107))
        self.pattern2 = Pattern('CORE', Bitboard(0x0000001818000000))
        self.pattern3 = Pattern('EDGE', Bitboard(0x00000000000000FF))
        self.pattern_set = PatternSet('SamplePatternSet', [self.pattern1, self.pattern2, self.pattern3])

        self.mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        self.opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        self.m = np.array([self.mover], dtype=Bitboard)
        self.o = np.array([self.opponent], dtype=Bitboard)

        self.large_set_pattern_data = [
            ('R2',     0x000000000000FF00),
            ('R3',     0x0000000000FF0000),
            ('R4',     0x00000000FF000000),
            ('XEDGE',  0x00000000000042FF),
            ('DIAG4',  0x0000000001020408),
            ('DIAG5',  0x0000000102040810),
            ('DIAG6',  0x0000010204081020),
            ('DIAG7',  0x0001020408102040),
            ('DIAG8',  0x0102040810204080),
            ('CORNER', 0x0000000000070707),
            ('2X5COR', 0x0000000000001F1F),
            ('RCT2X4', 0x0000003C3C000000),
            ('CORE',   0x0000001818000000),
        ]

        self.expected_large_set_pattern_order = [
            '2X5COR',
            'XEDGE',
            'R2',
            'CORNER',
            'R3',
            'DIAG4',
            'R4',
            'DIAG5',
            'CORE',
            'RCT2X4',
            'DIAG6',
            'DIAG7',
            'DIAG8',
        ]
        
        self.expected_large_set_pindexes_attribution = {
            '2X5COR': ( 0, 8,  0,  7),
            'XEDGE':  ( 1, 4,  8, 11),
            'R2':     ( 2, 4, 12, 15),
            'CORNER': ( 3, 4, 16, 19),
            'R3':     ( 4, 4, 20, 23),
            'DIAG4':  ( 5, 4, 24, 27),
            'R4':     ( 6, 4, 28, 31),
            'DIAG5':  ( 7, 4, 32, 35),
            'CORE':   ( 8, 1, 36, 36),
            'RCT2X4': ( 9, 2, 37, 38),
            'DIAG6':  (10, 4, 39, 42),
            'DIAG7':  (11, 4, 43, 46),
            'DIAG8':  (12, 2, 47, 48),
        }

        self.expected_large_set_principal_indexes = [
            52_551, 30_483, 58_865, 31_185, 58_497, 32_435, 57_030, 26_271, # 2X5COR
            13_542, 52_604, 14_639, 27, # XEDGE
            240, 4_256, 728, 128, # R2
            13_131, 15_972, 18_179, 11_427, # CORNER
            723, 2_158, 1_833, 1_936, # R3
            29, 22, 53, 24, # DIAG4
            3_370, 2_177, 1_372, 1_835, # R4
            51, 134, 76, 125, # DIAG5
            44, # CORE
            4_127, 3_374, # RCT2X4
            160, 133, 150, 150, # DIAG6
            701, 242, 395, 205, # DIAG7
            402, 647 # DIAG8
        ]




    def test_one_record(self):

        position = make_position(self.mover, self.opponent)
        positions = np.full(1, position)
        principal_indexes = self.pattern_set.compute_principal_indexes(positions)

        if False:
            position_print(position)
            
            print()
            print(f"Patterns:")
            i0 = 0
            for i, p in enumerate(self.pattern_set.patterns):
                I = len(p.unique_mask_indexes)
                print(f"  {i:02d}: {p.name} {I} [{i0:2d}:{(i0+I):2d}]")
                i0 += I

            print()
            print(f"principal_indexes:")
            print(f"{principal_indexes}")

            print(f"principal_indexes.shape = {principal_indexes.shape}")

        expected = np.array([[ 420, 116, 1517, 27, 9, 54, 59, 54, 15, 35, 6, 0, 44]], dtype=Index)
        nptest.assert_array_equal(principal_indexes, expected)

    def test_large_set(self):

        N = 3

        patterns = [Pattern(name, Bitboard(mask)) for name, mask in self.large_set_pattern_data]
        pset = PatternSet('50_PLUS', patterns)
        
        computed_pattern_order = [p.name for p in pset.patterns]
        self.assertEqual(computed_pattern_order, self.expected_large_set_pattern_order)

        J = 0
        for i, p in enumerate(pset.patterns):
            I = len(p.unique_mask_indexes)
            r0 = J
            r1 = J + I - 1
            ei, eI, er0, er1 = self.expected_large_set_pindexes_attribution[p.name]
            self.assertEqual(i, ei)
            self.assertEqual(I, eI)
            self.assertEqual(r0, er0)
            self.assertEqual(r1, er1)
            J += I

        positions = np.full(N, make_position(self.mover, self.opponent))
        principal_indexes = pset.compute_principal_indexes(positions)

        self.assertEqual(principal_indexes.shape, (N, J))
        self.assertEqual(principal_indexes.dtype, Index)

        expected = np.array(self.expected_large_set_principal_indexes, dtype=Index)
        computed = principal_indexes[0]
        nptest.assert_array_equal(computed, expected)

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        
        N = 1_000_000

        patterns = [Pattern(name, Bitboard(mask)) for name, mask in self.large_set_pattern_data]
        pset = PatternSet('50_PLUS', patterns)
        positions = np.full(N, make_position(self.mover, self.opponent))

        _ = pset.compute_principal_indexes(positions[:100])
        start_time = time.perf_counter()
        principal_indexes = pset.compute_principal_indexes(positions)
        end_time = time.perf_counter()
        duration = end_time - start_time
        positions_per_sec = N / duration
        print(f"\n[PERF PatternSet.compute_principal_indexes] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")

        expected = np.array(self.expected_large_set_principal_indexes, dtype=Index)
        computed = principal_indexes[0]
        nptest.assert_array_equal(computed, expected)
