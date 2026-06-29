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
from twolm.mobility import *
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
        self.assertIsNone(f.ref)

    def test_new_from_mobility(self):
        m = Mobility("LMC", Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0x0000000000000000))
        f = Feature.new_from_mobility(m)
        self.assertIsNotNone(f)
        self.assertEqual(f.category, Feature.Category.MOBILITY)
        self.assertEqual(f.name, "LMC")
        self.assertEqual(f.n_configurations, 64)
        self.assertEqual(f.n_instances, 1)
        self.assertIsNotNone(f.ref)
        self.assertEqual(f.ref, m)

    def test_new_from_pattern(self):
        p = Pattern("EDGE", Bitboard(0x00000000000000FF))
        f = Feature.new_from_pattern(p)
        self.assertIsNotNone(f)
        self.assertEqual(f.category, Feature.Category.PATTERN)
        self.assertEqual(f.name, "EDGE")
        self.assertEqual(f.n_configurations, 3**8)
        self.assertEqual(f.n_instances, 4)
        self.assertIsNotNone(f.ref)
        self.assertEqual(f.ref, p)

    def test_new_from_pattern_invalid_type(self):
        """Ensure input validation fires if wrong type object is injected."""
        with self.assertRaises(ValidationError):
            Feature.new_from_pattern("NotAPatternObject")

class TestFeatureSet(unittest.TestCase):

    def setUp(self):
        pattern_set_data = [
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
        plf = lambda x: [Pattern(n, Bitboard(m)) for n, m in x]
        mobility_set_data = [
            ('LMC',  0xFFFFFFFFFFFFFFFF, 0x0000000000000000),
            ('ALMC', 0x0000000000000000, 0xFFFFFFFFFFFFFFFF),
            ('DLMC', 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF),
        ]
        mlf = lambda x: [Mobility(n, Bitboard(m), Bitboard(a)) for n, m, a in x]
        self.intercept = Feature.new_intercept()
        self.mset = MobilitySet('TestMobilitySet', mlf(mobility_set_data))
        self.pset = PatternSet('TestPatternSet', plf(pattern_set_data))

    def test_init(self):
        feature_set = FeatureSet('TestFeatureSet', self.intercept, self.mset, self.pset)
        self.assertIsNotNone(feature_set)

        self.assertEqual(feature_set.name, 'TestFeatureSet')
        self.assertEqual(feature_set.intercept, self.intercept)
        self.assertEqual(feature_set.mset, self.mset)
        self.assertEqual(feature_set.pset, self.pset)

        self.assertEqual(feature_set.n_instances, 53)

        expected_n_instances = [1, 1, 1, 1, 8, 4, 4, 4, 4, 4, 4, 4, 1, 2, 4, 4, 2]
        computed_n_instances = [f.n_instances for f in feature_set.features]
        self.assertEqual(computed_n_instances, expected_n_instances)

        expected_hash = 'bc8897b689ed90c5a6e9440fa9766a8f763d5ff0a8661550f48b45eaff01ebec'
        self.assertEqual(feature_set.hash, expected_hash)

    def test_print_summary(self):
        feature_set = FeatureSet('TestFeatureSet', self.intercept, self.mset, self.pset)
        with io.StringIO() as buffer:
            feature_set.print_summary(output=buffer)
            actual_output = buffer.getvalue()

        self.assertIn("FeatureSet: name = TestFeatureSet, lenght = 17, hash = bc8897b689ed90c5a6e9440fa9766a8f763d5ff0a8661550f48b45eaff01ebec", actual_output)
        self.assertIn("  Intercept is present.", actual_output)
        self.assertIn("  MobilitySet: name = TestMobilitySet, hash = cee59abd002aefce5dbdc545629111e2975fc2ea278e66ffa4e22571560d5f34", actual_output)
        self.assertIn("    00 name = ALMC      , mask = 0x0000000000000000, amask = 0xFFFFFFFFFFFFFFFF", actual_output)
        self.assertIn("  MobilitySet: name = TestPatternSet, hash = f48dbd6313cf8ccbc72d46f56566b06d159560a0c4ebf7f81310756890ca2a1f", actual_output)
        self.assertIn("    04 name = R3        , mask = 0x0000000000FF0000", actual_output)
        self.assertIn("  Features: [<i>, <category>, <name>, <n_instances>, <n_configurations>]", actual_output)
        self.assertIn("    00 0 INTERCEPT  1          1", actual_output)
        self.assertIn("    01 1 ALMC       1         64", actual_output)
        self.assertIn("    04 2 2X5COR     8     59,049", actual_output)
        self.assertIn("    16 2 DIAG8      2      6,561", actual_output)

