#
# test_feature.py
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
# How to use the unit tests feature module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# time PERF=0 PYTHONPATH="./py" python3 -m unittest twolm.test.test_feature
#

import unittest
from unittest.mock import patch, mock_open, MagicMock

from twolm.board import *
from twolm.feature import *
from twolm.pattern import *

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import io
import time        
import os

from typing import Callable, TypeAlias, List

import pydantic
from pydantic import ValidationError



class TestFeature(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_init_raises_runtime_error(self):
        with self.assertRaises(RuntimeError):
            Feature(Feature.Category.INTERCEPT, "INTERCEPT", 1, 1)

    def test_new_intercept(self):
        f = Feature.new_intercept()
        self.assertIsNotNone(f)
        self.assertEqual(f.category, Feature.Category.INTERCEPT)
        self.assertEqual(f.name, "INTERCEPT")
        self.assertEqual(f.n_configurations, 1)
        self.assertEqual(f.n_instances, 1)
        self.assertIsNone(f.pattern)

    def test_new_from_pattern(self):
        p = Pattern("EDGE", Bitboard(0x00000000000000FF))
        f = Feature.new_from_pattern(p)
        self.assertIsNotNone(f)
        self.assertEqual(f.category, Feature.Category.PATTERN)
        self.assertEqual(f.name, "EDGE")
        self.assertEqual(f.n_configurations, 3**8)
        self.assertEqual(f.n_instances, 4)
        self.assertIsNotNone(f.pattern)

    def test_new_from_pattern_invalid_type(self):
        """Ensure input validation fires if wrong type object is injected."""
        with self.assertRaises(ValidationError):
            Feature.new_from_pattern("NotAPatternObject")

