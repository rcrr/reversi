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


class TestPattern(unittest.TestCase):

    def test_basics(self):
        with self.assertRaises(TypeError) as context:
            p = Pattern()
        self.assertIsInstance(context.exception, TypeError)

        self.assertIsInstance(PEdge(), Pattern)
        self.assertIsInstance(PEdge(), PEdge)

        self.assertEqual(PEdge(), PEdge())


class TestEdge(unittest.TestCase):
    
    def test_basics(self):
        p = PEdge()
        self.assertEqual(p, p)
        self.assertEqual(PEdge(), PEdge())


class TestCorner(unittest.TestCase):
    
    def test_basics(self):
        p = PCorner()
        self.assertEqual(1, 1)
