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
from unittest.mock import patch

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import twolm
import twolm.board
import twolm.pattern

from twolm.board import *
from twolm.pattern import *

from twolm.pattern import pack_ss, unpack_ss

from collections import namedtuple

from typing import Callable, TypeAlias, List



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
        packed_array = pack_ss(bitboard_array, p)
        nptest.assert_array_equal(expected_array, packed_array)
        unpacked_array = unpack_ss(packed_array, p)
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

        packed = [pack_ss(s, p) for s in self.bitboard_list]
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

        unpacked = [unpack_ss(s, p) for s in packed]
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

        packed = [pack_ss(s, p) for s in self.bitboard_list]
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

        unpacked = [unpack_ss(s, p) for s in packed]
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

        packed = [pack_ss(s, p) for s in self.bitboard_list]
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

        unpacked = [unpack_ss(s, p) for s in packed]
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

        packed = [pack_ss(s, p) for s in self.bitboard_list]
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

        unpacked = [unpack_ss(s, p) for s in packed]
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

        packed = [pack_ss(s, p) for s in self.bitboard_list]
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

        unpacked = [unpack_ss(s, p) for s in packed]
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

        packed = [pack_ss(s, p) for s in self.bitboard_list]
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

        unpacked = [unpack_ss(s, p) for s in packed]
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
