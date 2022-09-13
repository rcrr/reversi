#
# test_pattern.py
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

f = None

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

class TestEdge(unittest.TestCase):
    
    def test_basics(self):
        p = PEdge()
        self.assertEqual(p, p)
        self.assertEqual(PEdge(), PEdge())


class TestCorner(unittest.TestCase):
    
    def test_basics(self):
        p = PCorner()
        self.assertEqual(1, 1)
