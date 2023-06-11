#
# test_pattern.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022, 2023 Roberto Corradini. All rights reserved.
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
import reversi.pattern

from reversi.board import *
from reversi.pattern import *

import numpy as np

import io
from contextlib import redirect_stdout


f = lambda a: SquareSet.new_from_hex(a)
    
empty         = f('0000000000000000')
full          = f('ffffffffffffffff')

n_edge        = f('00000000000000ff')
e_edge        = f('8080808080808080')
s_edge        = f('ff00000000000000')
w_edge        = f('0101010101010101')

n_r2          = f('000000000000ff00')
e_r2          = f('4040404040404040')
s_r2          = f('00ff000000000000')
w_r2          = f('0202020202020202')

n_r3          = f('0000000000ff0000')
e_r3          = f('2020202020202020')
s_r3          = f('0000ff0000000000')
w_r3          = f('0404040404040404')

n_r4          = f('00000000ff000000')
e_r4          = f('1010101010101010')
s_r4          = f('000000ff00000000')
w_r4          = f('0808080808080808')

border        = f('ff818181818181ff')
bord_1        = f('007e424242427e00')
bord_2        = f('00003c24243c0000')
bord_3        = f('0000001818000000')

zebra_o       = f('00000000000000aa')
zebra_e       = f('0000000000000055')

nw_2x2        = f('0000000000000303')

nw_3x3        = f('0000000000070707')
ne_3x3        = f('0000000000e0e0e0')
se_3x3        = f('e0e0e00000000000')
sw_3x3        = f('0707070000000000')

nw_5x5        = f('0000001f1f1f1f1f')

nw_6x6        = f('00003f3f3f3f3f3f')

nw_7x7        = f('007f7f7f7f7f7f7f')
ne_7x7        = f('00fefefefefefefe')
se_7x7        = f('fefefefefefefe00')
sw_7x7        = f('7f7f7f7f7f7f7f00')

nw_tri_2      = f('0000000000000103')
ne_tri_2      = f('00000000000080c0')
se_tri_2      = f('c080000000000000')
sw_tri_2      = f('0301000000000000')

nw_tri_3      = f('0000000000010307')
ne_tri_3      = f('000000000080c0e0')
se_tri_3      = f('e0c0800000000000')
sw_tri_3      = f('0703010000000000')

nw_tri_4      = f('000000000103070f')
ne_tri_4      = f('0000000080c0e0f0')
se_tri_4      = f('f0e0c08000000000')
sw_tri_4      = f('0f07030100000000')

nw_tri_5      = f('0000000103070f1f')
ne_tri_5      = f('00000080c0e0f0f8')
se_tri_5      = f('f8f0e0c080000000')
sw_tri_5      = f('1f0f070301000000')

ne_tri_7      = f('0080c0e0f0f8fcfe')
sw_tri_7      = f('7f3f1f0f07030100')

ne_tri_8      = f('80c0e0f0f8fcfeff')

packed_03     = f('0000000000000007')
packed_04     = f('000000000000000f')
packed_05     = f('000000000000001f')
packed_06     = f('000000000000003f')

packed_07     = f('000000000000007f')
packed_08     = f('00000000000000ff')
packed_09     = f('00000000000001ff')
packed_10     = f('00000000000003ff')

mask_edge_0   = f('00000000000000ff')
mask_corner_0 = f('0000000000070707')
mask_xedge_0  = f('00000000000042ff')
mask_r2_0     = f('000000000000ff00')
mask_r3_0     = f('0000000000ff0000')
mask_r4_0     = f('00000000ff000000')
mask_diag3_0  = f('0000000000010204')
mask_diag4_0  = f('0000000001020408')
mask_diag5_0  = f('0000000102040810')
mask_diag6_0  = f('0000010204081020')
mask_diag7_0  = f('0001020408102040')
mask_diag8_0  = f('0102040810204080')
mask_2x5cor_0 = f('0000000000001f1f')


def check_expected_indexes(t, r, p):
    square_sets, indexes, principals = r
    mover, opponent = square_sets
    board = Board(mover, opponent)
    computed_indexes, computed_principals = board.compute_pattern_principal_indexes(p)
    [t.assertEqual(e, c) for e, c in zip(indexes, computed_indexes)]
    [t.assertEqual(e, c) for e, c in zip(principals, computed_principals)]

class TestPattern(unittest.TestCase):

    def test_basics(self):
        with self.assertRaises(TypeError) as context:
            p = Pattern()
        self.assertIsInstance(context.exception, TypeError)

        self.assertIsInstance(PEdge(), Pattern)
        self.assertIsInstance(PEdge(), PEdge)

        self.assertEqual(PEdge(), PEdge())

    def test_c_patterns(self):
        sl = [zebra_o, zebra_e, nw_2x2, nw_3x3]
        n_matches = 0
        for p in patterns_as_list:
            cp = p._c_pattern
            if cp:
                n_matches += 1
                self.assertEqual(p.id, cp.id)
                self.assertEqual(p.name, cp.name.decode('utf-8'))
                self.assertEqual(p.n_instances, cp.n_instances)
                self.assertEqual(p.n_squares, cp.n_squares)
                self.assertEqual(p.n_configurations, cp.n_configurations)
                for i in range (0, p.n_instances):
                    self.assertEqual(p.masks[i], cp.masks[i])
                    self.assertEqual(p.masks[0], p.trans_to_principal_f[i](p.masks[i]))
                self.assertEqual(pow(3, p.n_squares), p.n_configurations)
                for i in range (0, p.n_instances):
                    pf = p.trans_to_principal_f[i]
                    cf = cp.trans_to_principal_f[i]
                    [self.assertEqual(pf(s), cf(s)) for s in sl]
                [self.assertEqual(p.pack(s), cp.pattern_pack_f(s)) for s in sl]
                [self.assertEqual(p.unpack(s), cp.pattern_unpack_f(s)) for s in sl]
                [self.assertEqual(p.mirror(s), cp.pattern_mirror_f(s)) for s in sl]

        self.assertEqual(len(patterns_as_set), n_matches)

    def test_get_id_by_name(self):
        self.assertEqual( 1, patterns_as_dict['CORNER'].id)
        self.assertEqual( 2, patterns_as_dict['XEDGE'].id)
        self.assertEqual(12, patterns_as_dict['DIAG3'].id)

        
class TestEdge(unittest.TestCase):
    
    def test_basics(self):
        p = PEdge()
        self.assertEqual(p, p)
        self.assertEqual(PEdge(), PEdge())

    def test_pack(self):
        p = PEdge()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_08, p.pack(mask_edge_0))
        self.assertEqual(packed_08, p.pack(full))

    def test_unpack(self):
        p = PEdge()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_edge_0, p.unpack(packed_08))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),    [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (full,     empty),    [ 3280, 3280, 3280, 3280 ], [ 3280, 3280, 3280, 3280 ] ),
            ( (empty,    full),     [ 6560, 6560, 6560, 6560 ], [ 6560, 6560, 6560, 6560 ] ),
            ( (border,   empty),    [ 3280, 3280, 3280, 3280 ], [ 3280, 3280, 3280, 3280 ] ),
            ( (empty,    border),   [ 6560, 6560, 6560, 6560 ], [ 6560, 6560, 6560, 6560 ] ),
            ( (n_edge,   empty),    [ 3280,    1,    0, 2187 ], [ 3280,    1,    0,    1 ] ),
            ( (e_edge,   empty),    [ 2187, 3280,    1,    0 ], [    1, 3280,    1,    0 ] ),
            ( (s_edge,   empty),    [    0, 2187, 3280,    1 ], [    0,    1, 3280,    1 ] ),
            ( (w_edge,   empty),    [    1,    0, 2187, 3280 ], [    1,    0,    1, 3280 ] ),
            ( (empty,    n_edge),   [ 6560,    2,    0, 4374 ], [ 6560,    2,    0,    2 ] ),
            ( (empty,    e_edge),   [ 4374, 6560,    2,    0 ], [    2, 6560,    2,    0 ] ),
            ( (empty,    s_edge),   [    0, 4374, 6560,    2 ], [    0,    2, 6560,    2 ] ),
            ( (empty,    w_edge),   [    2,    0, 4374, 6560 ], [    2,    0,    2, 6560 ] ),
            ( (zebra_e,  empty),    [  820,    0,    0, 2187 ], [  820,    0,    0,    1 ] ),
            ( (empty,    zebra_e),  [ 1640,    0,    0, 4374 ], [ 1640,    0,    0,    2 ] ),
            ( (zebra_o,  empty),    [ 2460,    1,    0,    0 ], [  820,    1,    0,    0 ] ),
            ( (empty,    zebra_o),  [ 4920,    2,    0,    0 ], [ 1640,    2,    0,    0 ] ),
            ( (zebra_e,  zebra_o),  [ 5740,    2,    0, 2187 ], [ 4100,    2,    0,    1 ] ),
            ( (zebra_o,  zebra_e),  [ 4100,    1,    0, 4374 ], [ 4100,    1,    0,    2 ] ),
            ( (n_r2,     empty),    [    0,    3,    0,  729 ], [    0,    3,    0,    3 ] ),
            ( (n_r3,     empty),    [    0,    9,    0,  243 ], [    0,    9,    0,    9 ] ),
            ( (n_r4,     empty),    [    0,   27,    0,   81 ], [    0,   27,    0,   27 ] ),
            ( (nw_3x3,   e_r2),     [ 1471,    0,    6, 3159 ], [ 1471,    0,    6,   13 ] ),
            ( (empty,    ne_tri_5), [ 6534,  242,    0,    0 ], [  242,  242,    0,    0 ] ),
        ]

        [check_expected_indexes(self, r, PEdge()) for r in test_data]


class TestCorner(unittest.TestCase):
    
    def test_basics(self):
        p = PCorner()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PCorner()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_09, p.pack(mask_corner_0))
        self.assertEqual(packed_09, p.pack(full))

    def test_unpack(self):
        p = PCorner()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_corner_0, p.unpack(packed_09))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),    [    0,      0,    0,      0 ], [     0,     0,     0,     0 ] ),
            ( (full,     empty),    [  9841,  9841,  9841,  9841 ], [  9841,  9841,  9841,  9841 ] ),
            ( (empty,    full),     [ 19682, 19682, 19682, 19682 ], [ 19682, 19682, 19682, 19682 ] ),
            ( (nw_5x5,   empty),    [  9841,     0,     0,     0 ], [  9841,     0,     0,     0 ] ),
            ( (nw_6x6,   empty),    [  9841,  9477,  6561,  6813 ], [  9841,  6813,  6561,  6813 ] ),
            ( (nw_7x7,   empty),    [  9841,  9828,  9072,  9084 ], [  9841,  9084,  9072,  9084 ] ),
            ( (empty,    nw_5x5),   [ 19682,     0,     0,     0 ], [ 19682,     0,     0,     0 ] ),
            ( (empty,    nw_6x6),   [ 19682, 18954, 13122, 13626 ], [ 19682, 13626, 13122, 13626 ] ),
            ( (empty,    nw_7x7),   [ 19682, 19656, 18144, 18168 ], [ 19682, 18168, 18144, 18168 ] ),
            ( (border,   bord_2),   [ 13891, 13891, 13891, 13891 ], [ 13891, 13891, 13891, 13891 ] ),
            ( (bord_1,   bord_3),   [  2511,  2511,  2511,  2511 ], [  2511,  2511,  2511,  2511 ] ),
            ( (nw_2x2,   se_tri_3), [   112,     0,  1700,     0 ], [   112,     0,  1700,     0 ] ),
            ( (ne_tri_8, empty),    [  6898,  9841,  9586,     0 ], [  6898,  9841,  6898,     0 ] ),
            ( (ne_tri_7, sw_tri_7), [  6141,  9841,  3453, 19682 ], [  3453,  9841,  3453, 19682 ] ),
        ]

        [check_expected_indexes(self, r, PCorner()) for r in test_data]


class TestXedge(unittest.TestCase):
    
    def test_basics(self):
        p = PXedge()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PXedge()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_10, p.pack(mask_xedge_0))
        self.assertEqual(packed_10, p.pack(full))

    def test_unpack(self):
        p = PXedge()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_xedge_0, p.unpack(packed_10))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),    [     0,     0,    0,      0 ], [     0,     0,     0,     0 ] ),
            ( (full,     empty),    [ 29524, 29524, 29524, 29524 ], [ 29524, 29524, 29524, 29524 ] ),
            ( (empty,    full),     [ 59048, 59048, 59048, 59048 ], [ 59048, 59048, 59048, 59048 ] ),
            ( (n_edge,   empty),    [  3280,     1,     0,  2187 ], [  3280,     1,     0,     1 ] ),
            ( (e_edge,   empty),    [  2187,  3280,     1,     0 ], [     1,  3280,     1,     0 ] ),
            ( (s_edge,   empty),    [     0,  2187,  3280,     1 ], [     0,     1,  3280,     1 ] ),
            ( (w_edge,   empty),    [     1,     0,  2187,  3280 ], [     1,     0,     1,  3280 ] ),
            ( (empty,    n_r2),     [ 52488, 13128,     0, 40824 ], [ 52488, 13128,     0, 13128 ] ),
            ( (empty,    e_r2),     [ 40824, 52488, 13128,     0 ], [ 13128, 52488, 13128,     0 ] ),
            ( (empty,    s_r2),     [     0, 40824, 52488, 13128 ], [     0, 13128, 52488, 13128 ] ),
            ( (empty,    w_r2),     [ 13128,     0, 40824, 52488 ], [ 13128,     0, 13128, 52488 ] ),
            ( (n_r3,     n_r4),     [     0,    63,     0,   405 ], [     0,    63,     0,    63 ] ),
            ( (e_r3,     e_r4),     [   405,     0,    63,     0 ], [    63,     0,    63,     0 ] ),
            ( (s_r3,     s_r4),     [     0,   405,     0,    63 ], [     0,    63,     0,    63 ] ),
            ( (w_r3,     w_r4),     [    63,     0,   405,     0 ], [    63,     0,    63,     0 ] ),
            ( (border,   empty),    [  3280,  3280,  3280,  3280 ], [  3280,  3280,  3280,  3280 ] ),
            ( (empty,    border),   [  6560,  6560,  6560,  6560 ], [  6560,  6560,  6560,  6560 ] ),
            ( (bord_1,   empty),    [ 26244, 26244, 26244, 26244 ], [ 26244, 26244, 26244, 26244 ] ),
            ( (empty,    bord_1),   [ 52488, 52488, 52488, 52488 ], [ 52488, 52488, 52488, 52488 ] ),
            ( (bord_2,   bord_3),   [     0,     0,     0,     0 ], [     0,     0,     0,     0 ] ),
            ( (border,   bord_1),   [ 55768, 55768, 55768, 55768 ], [ 55768, 55768, 55768, 55768 ] ),
            ( (bord_1,   border),   [ 32804, 32804, 32804, 32804 ], [ 32804, 32804, 32804, 32804 ] ),
            ( (nw_3x3,   ne_3x3),   [ 52258, 13148,     0, 22842 ], [ 35990, 13148,     0,  6574 ] ),
            ( (ne_3x3,   se_3x3),   [ 22842, 52258, 13148,     0 ], [  6574, 35990, 13148,     0 ] ),
            ( (se_3x3,   sw_3x3),   [     0, 22842, 52258, 13148 ], [     0,  6574, 35990, 13148 ] ),
            ( (sw_3x3,   nw_3x3),   [ 13148,     0, 22842, 52258 ], [ 13148,     0,  6574, 35990 ] ),
            ( (nw_7x7,   empty),    [ 27337, 26244, 26244, 29523 ], [ 27337, 26244, 26244, 27337 ] ),
            ( (ne_7x7,   empty),    [ 29523, 27337, 26244, 26244 ], [ 27337, 27337, 26244, 26244 ] ),
            ( (se_7x7,   empty),    [ 26244, 29523, 27337, 26244 ], [ 26244, 27337, 27337, 26244 ] ),
            ( (sw_7x7,   empty),    [ 26244, 26244, 29523, 27337 ], [ 26244, 26244, 27337, 27337 ] ),
            ( (empty,    nw_7x7),   [ 54674, 52488, 52488, 59046 ], [ 54674, 52488, 52488, 54674 ] ),
            ( (empty,    ne_7x7),   [ 59046, 54674, 52488, 52488 ], [ 54674, 54674, 52488, 52488 ] ),
            ( (empty,    se_7x7),   [ 52488, 59046, 54674, 52488 ], [ 52488, 54674, 54674, 52488 ] ),
            ( (empty,    sw_7x7),   [ 52488, 52488, 59046, 54674 ], [ 52488, 52488, 54674, 54674 ] ),
            ( (nw_tri_2, ne_tri_3), [ 45688, 13148,     0,  2916 ], [ 16064, 13148,     0,     4 ] ),
            ( (ne_tri_2, se_tri_3), [  2916, 45688, 13148,     0 ], [     4, 16064, 13148,     0 ] ),
            ( (se_tri_2, sw_tri_3), [     0,  2916, 45688, 13148 ], [     0,     4, 16064, 13148 ] ),
            ( (sw_tri_2, nw_tri_3), [ 13148,     0,  2916, 45688 ], [ 13148,     0,     4, 16064 ] ),
            ( (nw_tri_4, se_tri_5), [  6601, 45900, 13364, 22923 ], [  6601, 13364, 13364,  6601 ] ),
            ( (ne_tri_4, sw_tri_5), [ 22923,  6601, 45900, 13364 ], [  6601,  6601, 13364, 13364 ] ),
            ( (se_tri_4, nw_tri_5), [ 13364, 22923,  6601, 45900 ], [ 13364,  6601,  6601, 13364 ] ),
            ( (sw_tri_4, ne_tri_5), [ 45900, 13364, 22923,  6601 ], [ 13364, 13364,  6601,  6601 ] ),
        ]

        [check_expected_indexes(self, r, PXedge()) for r in test_data]


class TestR2(unittest.TestCase):
    
    def test_basics(self):
        p = PR2()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PR2()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_08, p.pack(mask_r2_0))
        self.assertEqual(packed_08, p.pack(full))

    def test_unpack(self):
        p = PR2()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_r2_0, p.unpack(packed_08))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (full,     empty),  [ 3280, 3280, 3280, 3280 ], [ 3280, 3280, 3280, 3280 ] ),
            ( (empty,    full),   [ 6560, 6560, 6560, 6560 ], [ 6560, 6560, 6560, 6560 ] ),
            ( (n_edge,   empty),  [    0,    1,    0, 2187 ], [    0,    1,    0,    1 ] ),
            ( (e_edge,   empty),  [ 2187,    0,    1,    0 ], [    1,    0,    1,    0 ] ),
            ( (s_edge,   empty),  [    0, 2187,    0,    1 ], [    0,    1,    0,    1 ] ),
            ( (w_edge,   empty),  [    1,    0, 2187,    0 ], [    1,    0,    1,    0 ] ),
            ( (empty,    n_r2),   [ 6560,    6,    0, 1458 ], [ 6560,    6,    0,    6 ] ),
            ( (empty,    e_r2),   [ 1458, 6560,    6,    0 ], [    6, 6560,    6,    0 ] ),
            ( (empty,    s_r2),   [    0, 1458, 6560,    6 ], [    0,    6, 6560,    6 ] ),
            ( (empty,    w_r2),   [    6,    0, 1458, 6560 ], [    6,    0,    6, 6560 ] ),
            ( (n_r3,     n_r4),   [    0,   63,    0,  405 ], [    0,   63,    0,   63 ] ),
            ( (e_r3,     e_r4),   [  405,    0,   63,    0 ], [   63,    0,   63,    0 ] ),
            ( (s_r3,     s_r4),   [    0,  405,    0,   63 ], [    0,   63,    0,   63 ] ),
            ( (w_r3,     w_r4),   [   63,    0,  405,    0 ], [   63,    0,   63,    0 ] ),
            ( (border,   empty),  [ 2188, 2188, 2188, 2188 ], [ 2188, 2188, 2188, 2188 ] ),
            ( (empty,    border), [ 4376, 4376, 4376, 4376 ], [ 4376, 4376, 4376, 4376 ] ),
            ( (bord_1,   empty),  [ 1092, 1092, 1092, 1092 ], [ 1092, 1092, 1092, 1092 ] ),
            ( (empty,    bord_1), [ 2184, 2184, 2184, 2184 ], [ 2184, 2184, 2184, 2184 ] ),
            ( (bord_2,   bord_3), [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (border,   bord_1), [ 4372, 4372, 4372, 4372 ], [ 4372, 4372, 4372, 4372 ] ),
            ( (bord_1,   border), [ 5468, 5468, 5468, 5468 ], [ 5468, 5468, 5468, 5468 ] ),
            ( (nw_3x3,   ne_3x3), [ 6331,   26,    0, 3159 ], [ 3185,   26,    0,   13 ] ),
            ( (ne_3x3,   se_3x3), [ 3159, 6331,   26,    0 ], [   13, 3185,   26,    0 ] ),
            ( (se_3x3,   sw_3x3), [    0, 3159, 6331,   26 ], [    0,   13, 3185,   26 ] ),
            ( (sw_3x3,   nw_3x3), [   26,    0, 3159, 6331 ], [   26,    0,   13, 3185 ] ),
            ( (nw_7x7,   empty),  [ 1093, 1093, 3279, 3279 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (ne_7x7,   empty),  [ 3279, 1093, 1093, 3279 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (se_7x7,   empty),  [ 3279, 3279, 1093, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (sw_7x7,   empty),  [ 1093, 3279, 3279, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (empty,    nw_7x7), [ 2186, 2186, 6558, 6558 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    ne_7x7), [ 6558, 2186, 2186, 6558 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    se_7x7), [ 6558, 6558, 2186, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    sw_7x7), [ 2186, 6558, 6558, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
        ]

        [check_expected_indexes(self, r, PR2()) for r in test_data]


class TestR3(unittest.TestCase):
    
    def test_basics(self):
        p = PR3()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PR3()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_08, p.pack(mask_r3_0))
        self.assertEqual(packed_08, p.pack(full))

    def test_unpack(self):
        p = PR3()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_r3_0, p.unpack(packed_08))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (full,     empty),  [ 3280, 3280, 3280, 3280 ], [ 3280, 3280, 3280, 3280 ] ),
            ( (empty,    full),   [ 6560, 6560, 6560, 6560 ], [ 6560, 6560, 6560, 6560 ] ),
            ( (n_edge,   empty),  [    0,    1,    0, 2187 ], [    0,    1,    0,    1 ] ),
            ( (e_edge,   empty),  [ 2187,    0,    1,    0 ], [    1,    0,    1,    0 ] ),
            ( (s_edge,   empty),  [    0, 2187,    0,    1 ], [    0,    1,    0,    1 ] ),
            ( (w_edge,   empty),  [    1,    0, 2187,    0 ], [    1,    0,    1,    0 ] ),
            ( (empty,    n_r2),   [    0,    6,    0, 1458 ], [    0,    6,    0,    6 ] ),
            ( (empty,    e_r2),   [ 1458,    0,    6,    0 ], [    6,    0,    6,    0 ] ),
            ( (empty,    s_r2),   [    0, 1458,    0,    6 ], [    0,    6,    0,    6 ] ),
            ( (empty,    w_r2),   [    6,    0, 1458,    0 ], [    6,    0,    6,    0 ] ),
            ( (n_r3,     n_r4),   [ 3280,   63,    0,  405 ], [ 3280,   63,    0,   63 ] ),
            ( (e_r3,     e_r4),   [  405, 3280,   63,    0 ], [   63, 3280,   63,    0 ] ),
            ( (s_r3,     s_r4),   [    0,  405, 3280,   63 ], [    0,   63, 3280,   63 ] ),
            ( (w_r3,     w_r4),   [   63,    0,  405, 3280 ], [   63,    0,   63, 3280 ] ),
            ( (border,   empty),  [ 2188, 2188, 2188, 2188 ], [ 2188, 2188, 2188, 2188 ] ),
            ( (empty,    border), [ 4376, 4376, 4376, 4376 ], [ 4376, 4376, 4376, 4376 ] ),
            ( (bord_1,   empty),  [  732,  732,  732,  732 ], [  732,  732,  732,  732 ] ),
            ( (empty,    bord_1), [ 1464, 1464, 1464, 1464 ], [ 1464, 1464, 1464, 1464 ] ),
            ( (bord_2,   bord_3), [  360,  360,  360,  360 ], [  360,  360,  360,  360 ] ),
            ( (border,   bord_1), [ 3652, 3652, 3652, 3652 ], [ 3652, 3652, 3652, 3652 ] ),
            ( (bord_1,   border), [ 5108, 5108, 5108, 5108 ], [ 5108, 5108, 5108, 5108 ] ),
            ( (nw_3x3,   ne_3x3), [ 6331,   26,    0, 3159 ], [ 3185,   26,    0,   13 ] ),
            ( (ne_3x3,   se_3x3), [ 3159, 6331,   26,    0 ], [   13, 3185,   26,    0 ] ),
            ( (se_3x3,   sw_3x3), [    0, 3159, 6331,   26 ], [    0,   13, 3185,   26 ] ),
            ( (sw_3x3,   nw_3x3), [   26,    0, 3159, 6331 ], [   26,    0,   13, 3185 ] ),
            ( (nw_7x7,   empty),  [ 1093, 1093, 3279, 3279 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (ne_7x7,   empty),  [ 3279, 1093, 1093, 3279 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (se_7x7,   empty),  [ 3279, 3279, 1093, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (sw_7x7,   empty),  [ 1093, 3279, 3279, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (empty,    nw_7x7), [ 2186, 2186, 6558, 6558 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    ne_7x7), [ 6558, 2186, 2186, 6558 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    se_7x7), [ 6558, 6558, 2186, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    sw_7x7), [ 2186, 6558, 6558, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
        ]

        [check_expected_indexes(self, r, PR3()) for r in test_data]


class TestR4(unittest.TestCase):
    
    def test_basics(self):
        p = PR4()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PR4()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_08, p.pack(mask_r4_0))
        self.assertEqual(packed_08, p.pack(full))

    def test_unpack(self):
        p = PR4()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_r4_0, p.unpack(packed_08))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (full,     empty),  [ 3280, 3280, 3280, 3280 ], [ 3280, 3280, 3280, 3280 ] ),
            ( (empty,    full),   [ 6560, 6560, 6560, 6560 ], [ 6560, 6560, 6560, 6560 ] ),
            ( (n_edge,   empty),  [    0,    1,    0, 2187 ], [    0,    1,    0,    1 ] ),
            ( (e_edge,   empty),  [ 2187,    0,    1,    0 ], [    1,    0,    1,    0 ] ),
            ( (s_edge,   empty),  [    0, 2187,    0,    1 ], [    0,    1,    0,    1 ] ),
            ( (w_edge,   empty),  [    1,    0, 2187,    0 ], [    1,    0,    1,    0 ] ),
            ( (empty,    n_r2),   [    0,    6,    0, 1458 ], [    0,    6,    0,    6 ] ),
            ( (empty,    e_r2),   [ 1458,    0,    6,    0 ], [    6,    0,    6,    0 ] ),
            ( (empty,    s_r2),   [    0, 1458,    0,    6 ], [    0,    6,    0,    6 ] ),
            ( (empty,    w_r2),   [    6,    0, 1458,    0 ], [    6,    0,    6,    0 ] ),
            ( (n_r3,     n_r4),   [ 6560,   63,    0,  405 ], [ 6560,   63,    0,   63 ] ),
            ( (e_r3,     e_r4),   [  405, 6560,   63,    0 ], [   63, 6560,   63,    0 ] ),
            ( (s_r3,     s_r4),   [    0,  405, 6560,   63 ], [    0,   63, 6560,   63 ] ),
            ( (w_r3,     w_r4),   [   63,    0,  405, 6560 ], [   63,    0,   63, 6560 ] ),
            ( (border,   empty),  [ 2188, 2188, 2188, 2188 ], [ 2188, 2188, 2188, 2188 ] ),
            ( (empty,    border), [ 4376, 4376, 4376, 4376 ], [ 4376, 4376, 4376, 4376 ] ),
            ( (bord_1,   empty),  [  732,  732,  732,  732 ], [  732,  732,  732,  732 ] ),
            ( (empty,    bord_1), [ 1464, 1464, 1464, 1464 ], [ 1464, 1464, 1464, 1464 ] ),
            ( (bord_2,   bord_3), [  468,  468,  468,  468 ], [  468,  468,  468,  468 ] ),
            ( (border,   bord_1), [ 3652, 3652, 3652, 3652 ], [ 3652, 3652, 3652, 3652 ] ),
            ( (bord_1,   border), [ 5108, 5108, 5108, 5108 ], [ 5108, 5108, 5108, 5108 ] ),
            ( (nw_3x3,   ne_3x3), [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (ne_3x3,   se_3x3), [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (se_3x3,   sw_3x3), [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (sw_3x3,   nw_3x3), [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (nw_7x7,   empty),  [ 1093, 1093, 3279, 3279 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (ne_7x7,   empty),  [ 3279, 1093, 1093, 3279 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (se_7x7,   empty),  [ 3279, 3279, 1093, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (sw_7x7,   empty),  [ 1093, 3279, 3279, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (empty,    nw_7x7), [ 2186, 2186, 6558, 6558 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    ne_7x7), [ 6558, 2186, 2186, 6558 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    se_7x7), [ 6558, 6558, 2186, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (empty,    sw_7x7), [ 2186, 6558, 6558, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
        ]

        [check_expected_indexes(self, r, PR4()) for r in test_data]


class TestDiag4(unittest.TestCase):
    
    def test_basics(self):
        p = PDiag4()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PDiag4()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_04, p.pack(mask_diag4_0))
        self.assertEqual(packed_04, p.pack(full))
        self.assertEqual(f('0000000000000008'), p.pack(f('0000000000000008')))
        self.assertEqual(f('0000000000000004'), p.pack(f('0000000000000400')))
        self.assertEqual(f('0000000000000002'), p.pack(f('0000000000020000')))
        self.assertEqual(f('0000000000000001'), p.pack(f('0000000001000000')))
        self.assertEqual(empty, p.pack(full & ~mask_diag4_0))

    def test_unpack(self):
        p = PDiag4()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_diag4_0, p.unpack(packed_04))
        self.assertEqual(mask_diag4_0, p.unpack(full))
        self.assertEqual(f('0000000000000008'), p.unpack(f('0000000000000008')))
        self.assertEqual(f('0000000000000400'), p.unpack(f('0000000000000004')))
        self.assertEqual(f('0000000000020000'), p.unpack(f('0000000000000002')))
        self.assertEqual(f('0000000001000000'), p.unpack(f('0000000000000001')))
        self.assertEqual(empty, p.unpack(full & ~packed_04))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [  0,  0,  0,  0 ], [  0,  0,  0,  0 ] ),
            ( (full,     empty),  [ 40, 40, 40, 40 ], [ 40, 40, 40, 40 ] ),
            ( (empty,    full),   [ 80, 80, 80, 80 ], [ 80, 80, 80, 80 ] ),
            ( (n_edge,   empty),  [ 27,  1,  0,  0 ], [  1,  1,  0,  0 ] ),
            ( (e_edge,   empty),  [  0, 27,  1,  0 ], [  0,  1,  1,  0 ] ),
            ( (s_edge,   empty),  [  0,  0, 27,  1 ], [  0,  0,  1,  1 ] ),
            ( (w_edge,   empty),  [  1,  0,  0, 27 ], [  1,  0,  0,  1 ] ),
            ( (empty,    n_r2),   [ 18,  6,  0,  0 ], [  6,  6,  0,  0 ] ),
            ( (empty,    e_r2),   [  0, 18,  6,  0 ], [  0,  6,  6,  0 ] ),
            ( (empty,    s_r2),   [  0,  0, 18,  6 ], [  0,  0,  6,  6 ] ),
            ( (empty,    w_r2),   [  6,  0,  0, 18 ], [  6,  0,  0,  6 ] ),
            ( (n_r3,     n_r4),   [  5, 63,  0,  0 ], [  5,  5,  0,  0 ] ),
            ( (e_r3,     e_r4),   [  0,  5, 63,  0 ], [  0,  5,  5,  0 ] ),
            ( (s_r3,     s_r4),   [  0,  0,  5, 63 ], [  0,  0,  5,  5 ] ),
            ( (w_r3,     w_r4),   [ 63,  0,  0,  5 ], [  5,  0,  0,  5 ] ),
            ( (border,   empty),  [ 28, 28, 28, 28 ], [ 28, 28, 28, 28 ] ),
            ( (empty,    border), [ 56, 56, 56, 56 ], [ 56, 56, 56, 56 ] ),
            ( (bord_1,   empty),  [ 12, 12, 12, 12 ], [ 12, 12, 12, 12 ] ),
            ( (empty,    bord_1), [ 24, 24, 24, 24 ], [ 24, 24, 24, 24 ] ),
            ( (bord_2,   bord_3), [  0,  0,  0,  0 ], [  0,  0,  0,  0 ] ),
            ( (border,   bord_1), [ 52, 52, 52, 52 ], [ 52, 52, 52, 52 ] ),
            ( (bord_1,   border), [ 68, 68, 68, 68 ], [ 68, 68, 68, 68 ] ),
            ( (nw_3x3,   ne_3x3), [ 12, 24,  0,  0 ], [ 12, 24,  0,  0 ] ),
            ( (ne_3x3,   se_3x3), [  0, 12, 24,  0 ], [  0, 12, 24,  0 ] ),
            ( (se_3x3,   sw_3x3), [  0,  0, 12, 24 ], [  0,  0, 12, 24 ] ),
            ( (sw_3x3,   nw_3x3), [ 24,  0,  0, 12 ], [ 24,  0,  0, 12 ] ),
            ( (nw_7x7,   empty),  [ 40, 13, 12, 39 ], [ 40, 13, 12, 13 ] ),
            ( (ne_7x7,   empty),  [ 39, 40, 13, 12 ], [ 13, 40, 13, 12 ] ),
            ( (se_7x7,   empty),  [ 12, 39, 40, 13 ], [ 12, 13, 40, 13 ] ),
            ( (sw_7x7,   empty),  [ 13, 12, 39, 40 ], [ 13, 12, 13, 40 ] ),
            ( (empty,    nw_7x7), [ 80, 26, 24, 78 ], [ 80, 26, 24, 26 ] ),
            ( (empty,    ne_7x7), [ 78, 80, 26, 24 ], [ 26, 80, 26, 24 ] ),
            ( (empty,    se_7x7), [ 24, 78, 80, 26 ], [ 24, 26, 80, 26 ] ),
            ( (empty,    sw_7x7), [ 26, 24, 78, 80 ], [ 26, 24, 26, 80 ] ),
        ]

        [check_expected_indexes(self, r, PDiag4()) for r in test_data]


class TestDiag5(unittest.TestCase):
    
    def test_basics(self):
        p = PDiag5()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PDiag5()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_05, p.pack(mask_diag5_0))
        self.assertEqual(packed_05, p.pack(full))
        self.assertEqual(f('0000000000000010'), p.pack(f('0000000000000010')))
        self.assertEqual(f('0000000000000008'), p.pack(f('0000000000000800')))
        self.assertEqual(f('0000000000000004'), p.pack(f('0000000000040000')))
        self.assertEqual(f('0000000000000002'), p.pack(f('0000000002000000')))
        self.assertEqual(f('0000000000000001'), p.pack(f('0000000100000000')))
        self.assertEqual(empty, p.pack(full & ~mask_diag5_0))

    def test_unpack(self):
        p = PDiag5()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_diag5_0, p.unpack(packed_05))
        self.assertEqual(mask_diag5_0, p.unpack(full))
        self.assertEqual(f('0000000000000010'), p.unpack(f('0000000000000010')))
        self.assertEqual(f('0000000000000800'), p.unpack(f('0000000000000008')))
        self.assertEqual(f('0000000000040000'), p.unpack(f('0000000000000004')))
        self.assertEqual(f('0000000002000000'), p.unpack(f('0000000000000002')))
        self.assertEqual(f('0000000100000000'), p.unpack(f('0000000000000001')))
        self.assertEqual(empty, p.unpack(full & ~packed_05))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [   0,  0,   0,    0 ], [  0,   0,   0,    0 ] ),
            ( (full,     empty),  [ 121, 121, 121, 121 ], [ 121, 121, 121, 121 ] ),
            ( (empty,    full),   [ 242, 242, 242, 242 ], [ 242, 242, 242, 242 ] ),
            ( (n_edge,   empty),  [  81,   1,   0,   0 ], [   1,   1,   0,   0 ] ),
            ( (e_edge,   empty),  [   0,  81,   1,   0 ], [   0,   1,   1,   0 ] ),
            ( (s_edge,   empty),  [   0,   0,  81,   1 ], [   0,   0,   1,   1 ] ),
            ( (w_edge,   empty),  [   1,   0,   0,  81 ], [   1,   0,   0,   1 ] ),
            ( (empty,    n_r2),   [  54,   6,   0,   0 ], [   6,   6,   0,   0 ] ),
            ( (empty,    e_r2),   [   0,  54,   6,   0 ], [   0,   6,   6,   0 ] ),
            ( (empty,    s_r2),   [   0,   0,  54,   6 ], [   0,   0,   6,   6 ] ),
            ( (empty,    w_r2),   [   6,   0,   0,  54 ], [   6,   0,   0,   6 ] ),
            ( (n_r3,     n_r4),   [  15,  63,   2, 162 ], [  15,  15,   2,   2 ] ),
            ( (e_r3,     e_r4),   [ 162,  15,  63,   2 ], [   2,  15,  15,   2 ] ),
            ( (s_r3,     s_r4),   [   2, 162,  15,  63 ], [   2,   2,  15,  15 ] ),
            ( (w_r3,     w_r4),   [  63,   2, 162,  15 ], [  15,   2,   2,  15 ] ),
            ( (border,   empty),  [  82,  82,  82,  82 ], [  82,  82,  82,  82 ] ),
            ( (empty,    border), [ 164, 164, 164, 164 ], [ 164, 164, 164, 164 ] ),
            ( (bord_1,   empty),  [  30,  30,  30,  30 ], [  30,  30,  30,  30 ] ),
            ( (empty,    bord_1), [  60,  60,  60,  60 ], [  60,  60,  60,  60 ] ),
            ( (bord_2,   bord_3), [   9,   9,   9,   9 ], [   9,   9,   9,   9 ] ),
            ( (border,   bord_1), [ 142, 142, 142, 142 ], [ 142, 142, 142, 142 ] ),
            ( (bord_1,   border), [ 194, 194, 194, 194 ], [ 194, 194, 194, 194 ] ),
            ( (nw_3x3,   ne_3x3), [   9,  18,   0,   0 ], [   9,  18,   0,   0 ] ),
            ( (ne_3x3,   se_3x3), [   0,   9,  18,   0 ], [   0,   9,  18,   0 ] ),
            ( (se_3x3,   sw_3x3), [   0,   0,   9,  18 ], [   0,   0,   9,  18 ] ),
            ( (sw_3x3,   nw_3x3), [  18,   0,   0,   9 ], [  18,   0,   0,   9 ] ),
            ( (nw_7x7,   empty),  [ 121,  40,  39, 120 ], [ 121,  40,  39,  40 ] ),
            ( (ne_7x7,   empty),  [ 120, 121,  40,  39 ], [  40, 121,  40,  39 ] ),
            ( (se_7x7,   empty),  [  39, 120, 121,  40 ], [  39,  40, 121,  40 ] ),
            ( (sw_7x7,   empty),  [  40,  39, 120, 121 ], [  40,  39,  40, 121 ] ),
            ( (empty,    nw_7x7), [ 242,  80,  78, 240 ], [ 242,  80,  78,  80 ] ),
            ( (empty,    ne_7x7), [ 240, 242,  80,  78 ], [  80, 242,  80,  78 ] ),
            ( (empty,    se_7x7), [  78, 240, 242,  80 ], [  78,  80, 242,  80 ] ),
            ( (empty,    sw_7x7), [  80,  78, 240, 242 ], [  80,  78,  80, 242 ] ),
        ]

        [check_expected_indexes(self, r, PDiag5()) for r in test_data]


class TestDiag6(unittest.TestCase):
    
    def test_basics(self):
        p = PDiag6()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PDiag6()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_06, p.pack(mask_diag6_0))
        self.assertEqual(packed_06, p.pack(full))
        self.assertEqual(f('0000000000000020'), p.pack(f('0000000000000020')))
        self.assertEqual(f('0000000000000010'), p.pack(f('0000000000001000')))
        self.assertEqual(f('0000000000000008'), p.pack(f('0000000000080000')))
        self.assertEqual(f('0000000000000004'), p.pack(f('0000000004000000')))
        self.assertEqual(f('0000000000000002'), p.pack(f('0000000200000000')))
        self.assertEqual(f('0000000000000001'), p.pack(f('0000010000000000')))
        self.assertEqual(empty, p.pack(full & ~mask_diag6_0))

    def test_unpack(self):
        p = PDiag6()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_diag6_0, p.unpack(packed_06))
        self.assertEqual(mask_diag6_0, p.unpack(full))
        self.assertEqual(f('0000000000000020'), p.unpack(f('0000000000000020')))
        self.assertEqual(f('0000000000001000'), p.unpack(f('0000000000000010')))
        self.assertEqual(f('0000000000080000'), p.unpack(f('0000000000000008')))
        self.assertEqual(f('0000000004000000'), p.unpack(f('0000000000000004')))
        self.assertEqual(f('0000000200000000'), p.unpack(f('0000000000000002')))
        self.assertEqual(f('0000010000000000'), p.unpack(f('0000000000000001')))
        self.assertEqual(empty, p.unpack(full & ~packed_06))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [   0,  0,   0,    0 ], [  0,   0,   0,    0 ] ),
            ( (full,     empty),  [ 364, 364, 364, 364 ], [ 364, 364, 364, 364 ] ),
            ( (empty,    full),   [ 728, 728, 728, 728 ], [ 728, 728, 728, 728 ] ),
            ( (n_edge,   empty),  [ 243,   1,   0,   0 ], [   1,   1,   0,   0 ] ),
            ( (e_edge,   empty),  [   0, 243,   1,   0 ], [   0,   1,   1,   0 ] ),
            ( (s_edge,   empty),  [   0,   0, 243,   1 ], [   0,   0,   1,   1 ] ),
            ( (w_edge,   empty),  [   1,   0,   0, 243 ], [   1,   0,   0,   1 ] ),
            ( (empty,    n_r2),   [ 162,   6,   0,   0 ], [   6,   6,   0,   0 ] ),
            ( (empty,    e_r2),   [   0, 162,   6,   0 ], [   0,   6,   6,   0 ] ),
            ( (empty,    s_r2),   [   0,   0, 162,   6 ], [   0,   0,   6,   6 ] ),
            ( (empty,    w_r2),   [   6,   0,   0, 162 ], [   6,   0,   0,   6 ] ),
            ( (n_r3,     n_r4),   [  45,  63,   7, 405 ], [  45,  45,   7,   7 ] ),
            ( (e_r3,     e_r4),   [ 405,  45,  63,   7 ], [   7,  45,  45,   7 ] ),
            ( (s_r3,     s_r4),   [   7, 405,  45,  63 ], [   7,   7,  45,  45 ] ),
            ( (w_r3,     w_r4),   [  63,   7, 405,  45 ], [  45,   7,   7,  45 ] ),
            ( (border,   empty),  [ 244, 244, 244, 244 ], [ 244, 244, 244, 244 ] ),
            ( (empty,    border), [ 488, 488, 488, 488 ], [ 488, 488, 488, 488 ] ),
            ( (bord_1,   empty),  [  84,  84,  84,  84 ], [  84,  84,  84,  84 ] ),
            ( (empty,    bord_1), [ 168, 168, 168, 168 ], [ 168, 168, 168, 168 ] ),
            ( (bord_2,   bord_3), [  36,  36,  36,  36 ], [  36,  36,  36,  36 ] ),
            ( (border,   bord_1), [ 412, 412, 412, 412 ], [ 412, 412, 412, 412 ] ),
            ( (bord_1,   border), [ 572, 572, 572, 572 ], [ 572, 572, 572, 572 ] ),
            ( (nw_3x3,   ne_3x3), [ 486,   1,   2, 243 ], [   2,   1,   2,   1 ] ),
            ( (ne_3x3,   se_3x3), [ 243, 486,   1,   2 ], [   1,   2,   1,   2 ] ),
            ( (se_3x3,   sw_3x3), [   2, 243, 486,   1 ], [   2,   1,   2,   1 ] ),
            ( (sw_3x3,   nw_3x3), [   1,   2, 243, 486 ], [   1,   2,   1,   2 ] ),
            ( (nw_7x7,   empty),  [ 364, 121, 120, 363 ], [ 364, 121, 120, 121 ] ),
            ( (ne_7x7,   empty),  [ 363, 364, 121, 120 ], [ 121, 364, 121, 120 ] ),
            ( (se_7x7,   empty),  [ 120, 363, 364, 121 ], [ 120, 121, 364, 121 ] ),
            ( (sw_7x7,   empty),  [ 121, 120, 363, 364 ], [ 121, 120, 121, 364 ] ),
            ( (empty,    nw_7x7), [ 728, 242, 240, 726 ], [ 728, 242, 240, 242 ] ),
            ( (empty,    ne_7x7), [ 726, 728, 242, 240 ], [ 242, 728, 242, 240 ] ),
            ( (empty,    se_7x7), [ 240, 726, 728, 242 ], [ 240, 242, 728, 242 ] ),
            ( (empty,    sw_7x7), [ 242, 240, 726, 728 ], [ 242, 240, 242, 728 ] ),
        ]

        [check_expected_indexes(self, r, PDiag6()) for r in test_data]


class TestDiag7(unittest.TestCase):
    
    def test_basics(self):
        p = PDiag7()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PDiag7()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_07, p.pack(mask_diag7_0))
        self.assertEqual(packed_07, p.pack(full))
        self.assertEqual(f('0000000000000040'), p.pack(f('0000000000000040')))
        self.assertEqual(f('0000000000000020'), p.pack(f('0000000000002000')))
        self.assertEqual(f('0000000000000010'), p.pack(f('0000000000100000')))
        self.assertEqual(f('0000000000000008'), p.pack(f('0000000008000000')))
        self.assertEqual(f('0000000000000004'), p.pack(f('0000000400000000')))
        self.assertEqual(f('0000000000000002'), p.pack(f('0000020000000000')))
        self.assertEqual(f('0000000000000001'), p.pack(f('0001000000000000')))
        self.assertEqual(empty, p.pack(full & ~mask_diag7_0))

    def test_unpack(self):
        p = PDiag7()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_diag7_0, p.unpack(packed_07))
        self.assertEqual(mask_diag7_0, p.unpack(full))
        self.assertEqual(f('0000000000000040'), p.unpack(f('0000000000000040')))
        self.assertEqual(f('0000000000002000'), p.unpack(f('0000000000000020')))
        self.assertEqual(f('0000000000100000'), p.unpack(f('0000000000000010')))
        self.assertEqual(f('0000000008000000'), p.unpack(f('0000000000000008')))
        self.assertEqual(f('0000000400000000'), p.unpack(f('0000000000000004')))
        self.assertEqual(f('0000020000000000'), p.unpack(f('0000000000000002')))
        self.assertEqual(f('0001000000000000'), p.unpack(f('0000000000000001')))
        self.assertEqual(empty, p.unpack(full & ~packed_07))
        
    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [    0,    0,    0,    0 ], [    0,    0,    0,    0 ] ),
            ( (full,     empty),  [ 1093, 1093, 1093, 1093 ], [ 1093, 1093, 1093, 1093 ] ),
            ( (empty,    full),   [ 2186, 2186, 2186, 2186 ], [ 2186, 2186, 2186, 2186 ] ),
            ( (n_edge,   empty),  [  729,    1,    0,    0 ], [    1,    1,    0,    0 ] ),
            ( (e_edge,   empty),  [    0,  729,    1,    0 ], [    0,    1,    1,    0 ] ),
            ( (s_edge,   empty),  [    0,    0,  729,    1 ], [    0,    0,    1,    1 ] ),
            ( (w_edge,   empty),  [    1,    0,    0,  729 ], [    1,    0,    0,    1 ] ),
            ( (empty,    n_r2),   [  486,    6,    2, 1458 ], [    6,    6,    2,    2 ] ),
            ( (empty,    e_r2),   [ 1458,  486,    6,    2 ], [    2,    6,    6,    2 ] ),
            ( (empty,    s_r2),   [    2, 1458,  486,    6 ], [    2,    2,    6,    6 ] ),
            ( (empty,    w_r2),   [    6,    2, 1458,  486 ], [    6,    2,    2,    6 ] ),
            ( (n_r3,     n_r4),   [  135,   63,   21,  405 ], [   63,   63,   21,   21 ] ),
            ( (e_r3,     e_r4),   [  405,  135,   63,   21 ], [   21,   63,   63,   21 ] ),
            ( (s_r3,     s_r4),   [   21,  405,  135,   63 ], [   21,   21,   63,   63 ] ),
            ( (w_r3,     w_r4),   [   63,   21,  405,  135 ], [   63,   21,   21,   63 ] ),
            ( (border,   empty),  [  730,  730,  730,  730 ], [  730,  730,  730,  730 ] ),
            ( (empty,    border), [ 1460, 1460, 1460, 1460 ], [ 1460, 1460, 1460, 1460 ] ),
            ( (bord_1,   empty),  [  246,  246,  246,  246 ], [  246,  246,  246,  246 ] ),
            ( (empty,    bord_1), [  492,  492,  492,  492 ], [  492,  492,  492,  492 ] ),
            ( (bord_2,   bord_3), [  144,  144,  144,  144 ], [  144,  144,  144,  144 ] ),
            ( (border,   bord_1), [ 1222, 1222, 1222, 1222 ], [ 1222, 1222, 1222, 1222 ] ),
            ( (bord_1,   border), [ 1706, 1706, 1706, 1706 ], [ 1706, 1706, 1706, 1706 ] ),
            ( (nw_3x3,   ne_3x3), [ 1944,    4,    8,  972 ], [    8,    4,    8,    4 ] ),
            ( (ne_3x3,   se_3x3), [  972, 1944,    4,    8 ], [    4,    8,    4,    8 ] ),
            ( (se_3x3,   sw_3x3), [    8,  972, 1944,    4 ], [    8,    4,    8,    4 ] ),
            ( (sw_3x3,   nw_3x3), [    4,    8,  972, 1944 ], [    4,    8,    4,    8 ] ),
            ( (nw_7x7,   empty),  [ 1093,  364,  363, 1092 ], [ 1093,  364,  363,  364 ] ),
            ( (ne_7x7,   empty),  [ 1092, 1093,  364,  363 ], [  364, 1093,  364,  363 ] ),
            ( (se_7x7,   empty),  [  363, 1092, 1093,  364 ], [  363,  364, 1093,  364 ] ),
            ( (sw_7x7,   empty),  [  364,  363, 1092, 1093 ], [  364,  363,  364, 1093 ] ),
            ( (empty,    nw_7x7), [ 2186,  728,  726, 2184 ], [ 2186,  728,  726,  728 ] ),
            ( (empty,    ne_7x7), [ 2184, 2186,  728,  726 ], [  728, 2186,  728,  726 ] ),
            ( (empty,    se_7x7), [  726, 2184, 2186,  728 ], [  726,  728, 2186,  728 ] ),
            ( (empty,    sw_7x7), [  728,  726, 2184, 2186 ], [  728,  726,  728, 2186 ] ),
        ]

        [check_expected_indexes(self, r, PDiag7()) for r in test_data]


class TestDiag8(unittest.TestCase):
    
    def test_basics(self):
        p = PDiag8()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PDiag8()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_08, p.pack(mask_diag8_0))
        self.assertEqual(packed_08, p.pack(full))
        self.assertEqual(f('0000000000000080'), p.pack(f('0000000000000080')))
        self.assertEqual(f('0000000000000040'), p.pack(f('0000000000004000')))
        self.assertEqual(f('0000000000000020'), p.pack(f('0000000000200000')))
        self.assertEqual(f('0000000000000010'), p.pack(f('0000000010000000')))
        self.assertEqual(f('0000000000000008'), p.pack(f('0000000800000000')))
        self.assertEqual(f('0000000000000004'), p.pack(f('0000040000000000')))
        self.assertEqual(f('0000000000000002'), p.pack(f('0002000000000000')))
        self.assertEqual(f('0000000000000001'), p.pack(f('0100000000000000')))
        self.assertEqual(empty, p.pack(full & ~mask_diag8_0))

    def test_unpack(self):
        p = PDiag8()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_diag8_0, p.unpack(packed_08))
        self.assertEqual(mask_diag8_0, p.unpack(full))
        self.assertEqual(f('0000000000000080'), p.unpack(f('0000000000000080')))
        self.assertEqual(f('0000000000004000'), p.unpack(f('0000000000000040')))
        self.assertEqual(f('0000000000200000'), p.unpack(f('0000000000000020')))
        self.assertEqual(f('0000000010000000'), p.unpack(f('0000000000000010')))
        self.assertEqual(f('0000000800000000'), p.unpack(f('0000000000000008')))
        self.assertEqual(f('0000040000000000'), p.unpack(f('0000000000000004')))
        self.assertEqual(f('0002000000000000'), p.unpack(f('0000000000000002')))
        self.assertEqual(f('0100000000000000'), p.unpack(f('0000000000000001')))
        self.assertEqual(empty, p.unpack(full & ~packed_08))
        
    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [    0,    0 ], [    0,    0 ] ),
            ( (full,     empty),  [ 3280, 3280 ], [ 3280, 3280 ] ),
            ( (empty,    full),   [ 6560, 6560 ], [ 6560, 6560 ] ),
            ( (n_edge,   empty),  [ 2187,    1 ], [    1,    1 ] ),
            ( (e_edge,   empty),  [ 2187, 2187 ], [    1,    1 ] ),
            ( (s_edge,   empty),  [    1, 2187 ], [    1,    1 ] ),
            ( (w_edge,   empty),  [    1,    1 ], [    1,    1 ] ),
            ( (empty,    n_r2),   [ 1458,    6 ], [    6,    6 ] ),
            ( (empty,    e_r2),   [ 1458, 1458 ], [    6,    6 ] ),
            ( (empty,    s_r2),   [    6, 1458 ], [    6,    6 ] ),
            ( (empty,    w_r2),   [    6,    6 ], [    6,    6 ] ),
            ( (n_r3,     n_r4),   [  405,   63 ], [   63,   63 ] ),
            ( (e_r3,     e_r4),   [  405,  405 ], [   63,   63 ] ),
            ( (s_r3,     s_r4),   [   63,  405 ], [   63,   63 ] ),
            ( (w_r3,     w_r4),   [   63,   63 ], [   63,   63 ] ),
            ( (border,   empty),  [ 2188, 2188 ], [ 2188, 2188 ] ),
            ( (empty,    border), [ 4376, 4376 ], [ 4376, 4376 ] ),
            ( (bord_1,   empty),  [  732,  732 ], [  732,  732 ] ),
            ( (empty,    bord_1), [ 1464, 1464 ], [ 1464, 1464 ] ),
            ( (bord_2,   bord_3), [  468,  468 ], [  468,  468 ] ),
            ( (border,   bord_1), [ 3652, 3652 ], [ 3652, 3652 ] ),
            ( (bord_1,   border), [ 5108, 5108 ], [ 5108, 5108 ] ),
            ( (nw_3x3,   ne_3x3), [ 6318,   13 ], [   26,   13 ] ),
            ( (ne_3x3,   se_3x3), [ 3159, 6318 ], [   13,   26 ] ),
            ( (se_3x3,   sw_3x3), [   26, 3159 ], [   26,   13 ] ),
            ( (sw_3x3,   nw_3x3), [   13,   26 ], [   13,   26 ] ),
            ( (nw_7x7,   empty),  [ 1092, 1093 ], [ 1092, 1093 ] ),
            ( (ne_7x7,   empty),  [ 3279, 1092 ], [ 1093, 1092 ] ),
            ( (se_7x7,   empty),  [ 1092, 3279 ], [ 1092, 1093 ] ),
            ( (sw_7x7,   empty),  [ 1093, 1092 ], [ 1093, 1092 ] ),
            ( (empty,    nw_7x7), [ 2184, 2186 ], [ 2184, 2186 ] ),
            ( (empty,    ne_7x7), [ 6558, 2184 ], [ 2186, 2184 ] ),
            ( (empty,    se_7x7), [ 2184, 6558 ], [ 2184, 2186 ] ),
            ( (empty,    sw_7x7), [ 2186, 2184 ], [ 2186, 2184 ] ),
        ]

        [check_expected_indexes(self, r, PDiag8()) for r in test_data]


class Test2x5cor(unittest.TestCase):
    
    def test_basics(self):
        p = P2x5cor()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = P2x5cor()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_10, p.pack(mask_2x5cor_0))
        self.assertEqual(packed_10, p.pack(full))

    def test_unpack(self):
        p = P2x5cor()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_2x5cor_0, p.unpack(packed_10))
        self.assertEqual(mask_2x5cor_0, p.unpack(full))
        
    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),   [     0,     0,     0,     0,    0,     0,     0,      0 ], [     0,     0,     0,     0,     0,     0,     0,     0 ] ),
            ( (full,     empty),   [ 29524, 29524, 29524, 29524, 29524, 29524, 29524, 29524 ], [ 29524, 29524, 29524, 29524, 29524, 29524, 29524, 29524 ] ),
            ( (empty,    full),    [ 59048, 59048, 59048, 59048, 59048, 59048, 59048, 59048 ], [ 59048, 59048, 59048, 59048, 59048, 59048, 59048, 59048 ] ),
            ( (n_edge,   empty),   [   121,   244,     0,     0,   121,     0,     0,   244 ], [   121,   244,     0,     0,   121,     0,     0,   244 ] ),
            ( (e_edge,   empty),   [     0,   121,   244,     0,   244,   121,     0,     0 ], [     0,   121,   244,     0,   244,   121,     0,     0 ] ),
            ( (s_edge,   empty),   [     0,     0,   121,   244,     0,   244,   121,     0 ], [     0,     0,   121,   244,     0,   244,   121,     0 ] ),
            ( (w_edge,   empty),   [   244,     0,     0,   121,     0,     0,   244,   121 ], [   244,     0,     0,   121,     0,     0,   244,   121 ] ),
            ( (empty,    n_r2),    [ 58806,  1464,     0,     0, 58806,     0,     0,  1464 ], [ 58806,  1464,     0,     0, 58806,     0,     0,  1464 ] ),
            ( (empty,    e_r2),    [     0, 58806,  1464,     0,  1464, 58806,     0,     0 ], [     0, 58806,  1464,     0,  1464, 58806,     0,     0 ] ),
            ( (empty,    s_r2),    [     0,     0, 58806,  1464,     0,  1464, 58806,     0 ], [     0,     0, 58806,  1464,     0,  1464, 58806,     0 ] ),
            ( (empty,    w_r2),    [  1464,     0,     0, 58806,     0,     0,  1464, 58806 ], [  1464,     0,     0, 58806,     0,     0,  1464, 58806 ] ),
            ( (n_r3,     n_r4),    [     0, 15372,     0, 39528,     0, 39528,     0, 15372 ], [     0, 15372,     0, 39528,     0, 39528,     0, 15372 ] ),
            ( (e_r3,     e_r4),    [ 39528,     0, 15372,     0, 15372,     0, 39528,     0 ], [ 39528,     0, 15372,     0, 15372,     0, 39528,     0 ] ),
            ( (s_r3,     s_r4),    [     0, 39528,     0, 15372,     0, 15372,     0, 39528 ], [     0, 39528,     0, 15372,     0, 15372,     0, 39528 ] ),
            ( (w_r3,     w_r4),    [ 15372,     0, 39528,     0, 39528,     0, 15372,     0 ], [ 15372,     0, 39528,     0, 39528,     0, 15372,     0 ] ),
            ( (border,   empty),   [   364,   364,   364,   364,   364,   364,   364,   364 ], [   364,   364,   364,   364,   364,   364,   364,   364 ] ),
            ( (empty,    border),  [   728,   728,   728,   728,   728,   728,   728,   728 ], [   728,   728,   728,   728,   728,   728,   728,   728 ] ),
            ( (bord_1,   empty),   [ 29160, 29160, 29160, 29160, 29160, 29160, 29160, 29160 ], [ 29160, 29160, 29160, 29160, 29160, 29160, 29160, 29160 ] ),
            ( (empty,    bord_1),  [ 58320, 58320, 58320, 58320, 58320, 58320, 58320, 58320 ], [ 58320, 58320, 58320, 58320, 58320, 58320, 58320, 58320 ] ),
            ( (bord_2,   bord_3),  [     0,     0,     0,     0,     0,     0,     0,     0 ], [     0,     0,     0,     0,     0,     0,     0,     0 ] ),
            ( (border,   bord_1),  [ 58684, 58684, 58684, 58684, 58684, 58684, 58684, 58684 ], [ 58684, 58684, 58684, 58684, 58684, 58684, 58684, 58684 ] ),
            ( (bord_1,   border),  [ 29888, 29888, 29888, 29888, 29888, 29888, 29888, 29888 ], [ 29888, 29888, 29888, 29888, 29888, 29888, 29888, 29888 ] ),
            ( (nw_3x3,   ne_3x3),  [  3172,  6344,     0,     0,  6344,     0,     0,  3172 ], [  3172,  6344,     0,     0,  6344,     0,     0,  3172 ] ),
            ( (ne_3x3,   se_3x3),  [     0,  3172,  6344,     0,  3172,  6344,     0,     0 ], [     0,  3172,  6344,     0,  3172,  6344,     0,     0 ] ),
            ( (se_3x3,   sw_3x3),  [     0,     0,  3172,  6344,     0,  3172,  6344,     0 ], [     0,     0,  3172,  6344,     0,  3172,  6344,     0 ] ),
            ( (sw_3x3,   nw_3x3),  [  6344,     0,     0,  3172,     0,     0,  3172,  6344 ], [  6344,     0,     0,  3172,     0,     0,  3172,  6344 ] ),
            ( (nw_7x7,   empty),   [ 29524, 29403, 29160, 29280, 29280, 29160, 29403, 29524 ], [ 29524, 29403, 29160, 29280, 29280, 29160, 29403, 29524 ] ),
            ( (ne_7x7,   empty),   [ 29280, 29524, 29403, 29160, 29524, 29280, 29160, 29403 ], [ 29280, 29524, 29403, 29160, 29524, 29280, 29160, 29403 ] ),
            ( (se_7x7,   empty),   [ 29160, 29280, 29524, 29403, 29403, 29524, 29280, 29160 ], [ 29160, 29280, 29524, 29403, 29403, 29524, 29280, 29160 ] ),
            ( (sw_7x7,   empty),   [ 29403, 29160, 29280, 29524, 29160, 29403, 29524, 29280 ], [ 29403, 29160, 29280, 29524, 29160, 29403, 29524, 29280 ] ),
            ( (empty,    nw_7x7),  [ 59048, 58806, 58320, 58560, 58560, 58320, 58806, 59048 ], [ 59048, 58806, 58320, 58560, 58560, 58320, 58806, 59048 ] ),
            ( (empty,    ne_7x7),  [ 58560, 59048, 58806, 58320, 59048, 58560, 58320, 58806 ], [ 58560, 59048, 58806, 58320, 59048, 58560, 58320, 58806 ] ),
            ( (empty,    se_7x7),  [ 58320, 58560, 59048, 58806, 58806, 59048, 58560, 58320 ], [ 58320, 58560, 59048, 58806, 58806, 59048, 58560, 58320 ] ),
            ( (empty,    sw_7x7),  [ 58806, 58320, 58560, 59048, 58320, 58806, 59048, 58560 ], [ 58806, 58320, 58560, 59048, 58320, 58806, 59048, 58560 ] ),
            ( (nw_tri_2, ne_tri_3), [   247,  1970,     0,     0,  1970,     0,     0,   247 ], [   247,  1970,     0,     0,  1970,     0,     0,   247 ] ),
            ( (ne_tri_2, se_tri_3), [     0,   247,  1970,     0,   247,  1970,     0,     0 ], [     0,   247,  1970,     0,   247,  1970,     0,     0 ] ),
            ( (se_tri_2, sw_tri_3), [     0,     0,   247,  1970,     0,   247,  1970,     0 ], [     0,     0,   247,  1970,     0,   247,  1970,     0 ] ),
            ( (sw_tri_2, nw_tri_3), [  1970,     0,     0,   247,     0,     0,   247,  1970 ], [  1970,     0,     0,   247,     0,     0,   247,  1970 ] ),
            ( (nw_tri_4, se_tri_5), [  3199, 39582, 19682,    81,    81, 19682, 39582,  3199 ], [  3199, 39582, 19682,    81,    81, 19682, 39582,  3199 ] ),
            ( (ne_tri_4, sw_tri_5), [    81,  3199, 39582, 19682,  3199,    81, 19682, 39582 ], [    81,  3199, 39582, 19682,  3199,    81, 19682, 39582 ] ),
            ( (se_tri_4, nw_tri_5), [ 19682,    81,  3199, 39582, 39582,  3199,    81, 19682 ], [ 19682,    81,  3199, 39582, 39582,  3199,    81, 19682 ] ),
            ( (sw_tri_4, ne_tri_5), [ 39582, 19682,    81,  3199, 19682, 39582,  3199,    81 ], [ 39582, 19682,    81,  3199, 19682, 39582,  3199,    81 ] ),
        ]

        [check_expected_indexes(self, r, P2x5cor()) for r in test_data]


class TestDiag3(unittest.TestCase):
    
    def test_basics(self):
        p = PDiag3()
        self.assertEqual(1, 1)

    def test_pack(self):
        p = PDiag3()
        self.assertEqual(empty, p.pack(empty))
        self.assertEqual(packed_03, p.pack(mask_diag3_0))
        self.assertEqual(packed_03, p.pack(full))
        self.assertEqual(f('0000000000000004'), p.pack(f('0000000000000004')))
        self.assertEqual(f('0000000000000002'), p.pack(f('0000000000000200')))
        self.assertEqual(f('0000000000000001'), p.pack(f('0000000000010000')))
        self.assertEqual(empty, p.pack(full & ~mask_diag3_0))

    def test_unpack(self):
        p = PDiag3()
        self.assertEqual(empty, p.unpack(empty))
        self.assertEqual(mask_diag3_0, p.unpack(packed_03))
        self.assertEqual(mask_diag3_0, p.unpack(full))
        self.assertEqual(f('0000000000000004'), p.unpack(f('0000000000000004')))
        self.assertEqual(f('0000000000000200'), p.unpack(f('0000000000000002')))
        self.assertEqual(f('0000000000010000'), p.unpack(f('0000000000000001')))
        self.assertEqual(empty, p.unpack(full & ~packed_03))

    def test_compute_indexes(self):
        test_data = [
            ( (empty,    empty),  [  0,  0,  0,  0 ], [  0,  0,  0,  0 ] ),
            ( (full,     empty),  [ 13, 13, 13, 13 ], [ 13, 13, 13, 13 ] ),
            ( (empty,    full),   [ 26, 26, 26, 26 ], [ 26, 26, 26, 26 ] ),
            ( (n_edge,   empty),  [  9,  1,  0,  0 ], [  1,  1,  0,  0 ] ),
            ( (e_edge,   empty),  [  0,  9,  1,  0 ], [  0,  1,  1,  0 ] ),
            ( (s_edge,   empty),  [  0,  0,  9,  1 ], [  0,  0,  1,  1 ] ),
            ( (w_edge,   empty),  [  1,  0,  0,  9 ], [  1,  0,  0,  1 ] ),
            ( (empty,    n_r2),   [  6,  6,  0,  0 ], [  6,  6,  0,  0 ] ),
            ( (empty,    e_r2),   [  0,  6,  6,  0 ], [  0,  6,  6,  0 ] ),
            ( (empty,    s_r2),   [  0,  0,  6,  6 ], [  0,  0,  6,  6 ] ),
            ( (empty,    w_r2),   [  6,  0,  0,  6 ], [  6,  0,  0,  6 ] ),
            ( (n_r3,     n_r4),   [  1,  9,  0,  0 ], [  1,  1,  0,  0 ] ),
            ( (e_r3,     e_r4),   [  0,  1,  9,  0 ], [  0,  1,  1,  0 ] ),
            ( (s_r3,     s_r4),   [  0,  0,  1,  9 ], [  0,  0,  1,  1 ] ),
            ( (w_r3,     w_r4),   [  9,  0,  0,  1 ], [  1,  0,  0,  1 ] ),
            ( (border,   empty),  [ 10, 10, 10, 10 ], [ 10, 10, 10, 10 ] ),
            ( (empty,    border), [ 20, 20, 20, 20 ], [ 20, 20, 20, 20 ] ),
            ( (bord_1,   empty),  [  3,  3,  3,  3 ], [  3,  3,  3,  3 ] ),
            ( (empty,    bord_1), [  6,  6,  6,  6 ], [  6,  6,  6,  6 ] ),
            ( (bord_2,   bord_3), [  0,  0,  0,  0 ], [  0,  0,  0,  0 ] ),
            ( (border,   bord_1), [ 16, 16, 16, 16 ], [ 16, 16, 16, 16 ] ),
            ( (bord_1,   border), [ 23, 23, 23, 23 ], [ 23, 23, 23, 23 ] ),
            ( (nw_3x3,   ne_3x3), [ 13, 26,  0,  0 ], [ 13, 26,  0,  0 ] ),
            ( (ne_3x3,   se_3x3), [  0, 13, 26,  0 ], [  0, 13, 26,  0 ] ),
            ( (se_3x3,   sw_3x3), [  0,  0, 13, 26 ], [  0,  0, 13, 26 ] ),
            ( (sw_3x3,   nw_3x3), [ 26,  0,  0, 13 ], [ 26,  0,  0, 13 ] ),
            ( (nw_7x7,   empty),  [ 13,  4,  3, 12 ], [ 13,  4,  3,  4 ] ),
            ( (ne_7x7,   empty),  [ 12, 13,  4,  3 ], [  4, 13,  4,  3 ] ),
            ( (se_7x7,   empty),  [  3, 12, 13,  4 ], [  3,  4, 13,  4 ] ),
            ( (sw_7x7,   empty),  [  4,  3, 12, 13 ], [  4,  3,  4, 13 ] ),
            ( (empty,    nw_7x7), [ 26,  8,  6, 24 ], [ 26,  8,  6,  8 ] ),
            ( (empty,    ne_7x7), [ 24, 26,  8,  6 ], [  8, 26,  8,  6 ] ),
            ( (empty,    se_7x7), [  6, 24, 26,  8 ], [  6,  8, 26,  8 ] ),
            ( (empty,    sw_7x7), [  8,  6, 24, 26 ], [  8,  6,  8, 26 ] ),
        ]

        [check_expected_indexes(self, r, PDiag3()) for r in test_data]

        
class TestModuleMethods(unittest.TestCase):

    def test_board_pattern_packed_to_index(self):
        packed = Board.new_from_hexes('0000000000000002', '0000000000000001')
        n_squares = 8
        index = board_pattern_packed_to_index(packed, n_squares)
        self.assertEqual(5, index)

    def test_board_pattern_index_to_packed(self):
        expected = Board.new_from_hexes('0000000000000002', '0000000000000001')
        packed = board_pattern_index_to_packed(5)
        self.assertEqual(expected, packed)

    def test_board_pattern_index_to_from_packed(self):

        def execute_test(t):
            mover, opponent, n_squares, index, comment = t
            m = SquareSet.new_from_hex('000000000000' + mover)
            o = SquareSet.new_from_hex('000000000000' + opponent)
            packed = Board(m, o)
            computed_index = board_pattern_packed_to_index(packed, n_squares)
            self.assertEqual(index, computed_index)
            computed_packed = board_pattern_index_to_packed(index)
            computed_mover = computed_packed.mover
            computed_opponent = computed_packed.opponent
            self.assertEqual(m, computed_mover)
            self.assertEqual(o, computed_opponent)

        test_data = [ ('0000', '0000',  8,     0, 'edge 00000000'),
                      ('00ff', '0000',  8,  3280, 'edge 11111111'),
                      ('0000', '00ff',  8,  6560, 'edge 22222222'),
                      ('0220', '015c', 10, 34740, 'xedge 0022212021'),
                      ('0024', '01d0',  9, 19368, 'corner 001021222'),
                      ('0081', '007e',  8,  4372, 'R2 12222221'),
                      ('0006', '0001',  3,    14, 'diag3 211'),
                      ('0002', '000c',  4,    75, 'diag4 0122'),
                      ('0006', '0019',  5,   230, 'diag5 21122'),
                      ('0027', '0008',  6,   310, 'diag6 111201'),
                      ('0022', '005c',  7,  1938, 'diag7 0122212'),
                      ('00f4', '0008',  8,  3303, 'diag8 00121111'),
                      ('0000', '01ff', 10, 19682, '2x5cor 2222222220'),
                      ('0000', '03ff', 10, 59048, '2x5cor 2222222222'),
                     ]

        [execute_test(t) for t in test_data]

class TestComputePatternIndexes(unittest.TestCase):

    def setUp(self):
        # This is game position 61412414:
        m = SquareSet.new_from_signed_int(np.int64(4039748727574502410))
        o = SquareSet.new_from_signed_int(np.int64(327827004853592689))
        self.b = Board(m, o)
    
    def test_compute_pattern_indexes_edge(self):
        indexes = self.b.compute_pattern_indexes(PEdge())
        expected = np.array([2138, 2169, 603, 4374], dtype=np.int32)
        self.assertIsNone(np.testing.assert_array_equal(expected, indexes))
    
    def test_compute_pattern_indexes_2x6cor(self):
        indexes = self.b.compute_pattern_indexes(P2x6cor())
        expected = np.array([444641, 146511, 494136, 183708, 269160, 276045, 111906, 188813], dtype=np.int32)
        self.assertIsNone(np.testing.assert_array_equal(expected, indexes))

    def test_compute_patternlist_indexes(self):
        indexes = self.b.compute_patternlist_indexes([PEdge(), P2x6cor()])
        expected = np.array([2138, 2169, 603, 4374, 444641, 146511, 494136, 183708, 269160, 276045, 111906, 188813], dtype=np.int32)
        self.assertIsNone(np.testing.assert_array_equal(expected, indexes))

    def test_compute_pattern_principal_indexes(self):
        indexes = np.array([444641, 146511, 494136, 183708, 269160, 276045, 111906, 188813], dtype=np.int32)
        principals = compute_pattern_principal_indexes(indexes, P2x6cor())
        expected = indexes
        self.assertIsNone(np.testing.assert_array_equal(expected, principals))
        
    # Aggiungere i tests sui PRINCIPAL INDEX ... dovrebbero fallire perchè sono solo 16 bit ....
