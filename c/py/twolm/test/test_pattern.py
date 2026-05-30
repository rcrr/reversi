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


from typing import Callable, TypeAlias, List


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
