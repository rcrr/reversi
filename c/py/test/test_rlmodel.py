#
# test_rlmodel.py
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
# How to use the unit tests rlmodel module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest test.test_rlmodel
#

import unittest

import twolm
import twolm.domain
import twolm.rdata
import twolm.rlmodel

from twolm.domain import *
from twolm.rdata import *
from twolm.rlmodel import *

from twolm.rlmodel import PatternConfig

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import tempfile
import shutil

from pathlib import Path

import os


class TestReversiLogisticModelInit(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_init(self):
        json_config = 'py/test/data/tlm/rlmodel_00.json'
        rlm = ReversiLogisticModel(json_config, base_dir_override=self.tmp_dir)

        retrieved_name = rlm.cfg.name
        retrieved_description = rlm.cfg.description
        retrieved_project_dir = rlm.cfg.project_dir

        expected_name = 'RGLM (Logistic) - Unit Test 00 (rlmodel_00.json)'
        expected_description = 'Used for unit testing.'
        expected_base_dir = "It is always different. It is not checked."
        expected_project_dir = Path("UNITTEST")

        self.assertEqual(expected_name, retrieved_name)
        self.assertEqual(expected_description, retrieved_description)
        self.assertEqual(expected_project_dir, retrieved_project_dir)

        self.assertIsNotNone(rlm.cfg.regab_indexed_data_set_cached)
        ridsc = rlm.cfg.regab_indexed_data_set_cached
        self.assertIsNotNone(ridsc.regab_indexed_data_set)
        rids = ridsc.regab_indexed_data_set
        self.assertIsNotNone(rids.regab_data_set_cached)
        rdsc = rids.regab_data_set_cached
        self.assertIsNotNone(rdsc.regab_data_set)
        rds = rdsc.regab_data_set
        self.assertIsNotNone(rds.regab_db_connection)
        conn = rds.regab_db_connection
        self.assertEqual('tst_regab', conn.dbname)
        self.assertEqual('tst_regab', conn.user)
        self.assertEqual('localhost', conn.host)
        self.assertEqual([8], rds.bid)
        self.assertEqual(['CMS', 'CMR'], rds.status)
        self.assertEqual(20, rds.ec)
        self.assertEqual(Path('regab_data_set.dat'), rdsc.filename)
        self.assertIsNotNone(rids.pattern_set)
        pset = rids.pattern_set
        self.assertEqual('BasicPatternSet', pset.name)
        self.assertIsNotNone(pset.patterns)
        self.assertIsInstance(pset.patterns, list)
        self.assertEqual([PatternConfig(name='ELLE', mask='0000000000000107'),
                          PatternConfig(name='EDGE', mask='00000000000000FF')],
                         pset.patterns)
        self.assertEqual(False, rids.has_indexes)
        self.assertEqual(False, rids.has_findexes)
        self.assertEqual(True, rids.has_pindexes)
        self.assertEqual(True, rids.has_lookup)
        self.assertEqual(True, rids.has_revmap)
        self.assertEqual(Path('regab_indexed_data_set.dat'), ridsc.filename)


class TestReversiLogisticModel(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        json_config = 'py/test/data/tlm/rlmodel_00.json'
        self.rlm = ReversiLogisticModel(json_config, base_dir_override=self.tmp_dir)
        Path(self.rlm.cfg.full_project_dir).mkdir(parents=False, exist_ok=True)

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_load_regab_data_set_from_db(self):
        self.rlm.load_regab_data_set_from_db()
        self.assertIsNotNone(self.rlm.rds)

        if False:
            os.system(f"ls -l {self.rlm.cfg.full_project_dir}")

    def test_load_regab_data_set_from_file(self):
        self.assertIsNone(self.rlm.rds)
        self.rlm.load_regab_data_set_from_db()
        self.assertIsNotNone(self.rlm.rds)
        self.rlm.rds = None
        self.assertIsNone(self.rlm.rds)

        # So now we have the file saved into the tempdir.

        if False:
            os.system(f"ls -l {self.rlm.cfg.full_project_dir}")

        self.rlm.load_regab_data_set_from_file()
        self.assertIsNotNone(self.rlm.rds)

    def test_load_regab_data_set_from_file(self):
        self.assertIsNone(self.rlm.rds)
        self.rlm.load_regab_data_set_from_db()
        self.assertIsNotNone(self.rlm.rds)
        self.rlm.rds = None
        self.assertIsNone(self.rlm.rds)

        # So now we have the file saved into the tempdir.

        if False:
            os.system(f"ls -l {self.rlm.cfg.full_project_dir}")

        self.rlm.load_regab_data_set_from_file()
        self.assertIsNotNone(self.rlm.rds)

    def test_load_regab_data_set_option_no_file(self):
        self.assertIsNone(self.rlm.rds)
        self.rlm.load_regab_data_set()
        self.assertIsNotNone(self.rlm.rds)

    def test_load_regab_data_set_option_with_file(self):
        self.assertIsNone(self.rlm.rds)
        self.rlm.load_regab_data_set_from_db()
        self.assertIsNotNone(self.rlm.rds)
        self.rlm.rds = None
        self.assertIsNone(self.rlm.rds)

        self.rlm.load_regab_data_set()
        self.assertIsNotNone(self.rlm.rds)

    def test_load_regab_indexed_data_set(self):
        self.assertIsNone(self.rlm.rds)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)

    def test_load_regab_indexed_data_set_from_cache_file(self):
        self.assertIsNone(self.rlm.rds)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)

        self.rlm.rids = None
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)

        if False:
            os.system(f"ls -l {self.rlm.cfg.full_project_dir}")


class TestReversiLogisticModelComputeWmaps(unittest.TestCase):

    def setUp(self):
        json_config = 'py/test/data/tlm/rlmodel_01.json'
        self.rlm = ReversiLogisticModel(json_config)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)

    def tearDown(self):
        pass

    def test_compute_wmaps(self):
        self.assertIsNone(self.rlm.pattern_w_ranges)
        self.assertIsNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNone(self.rlm.iwmap)
        self.assertIsNone(self.rlm.wmap)
        self.assertIsNone(self.rlm.wmap_fallback)
        self.rlm.compute_wmaps()
        self.assertIsNotNone(self.rlm.pattern_w_ranges)
        self.assertIsNotNone(self.rlm.iwmap)
        self.assertIsNotNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNotNone(self.rlm.wmap)
        self.assertIsNotNone(self.rlm.wmap_fallback)

        expected_cut_off = 2
        self.assertEqual(expected_cut_off, self.rlm.cfg.stat_model.frequency_cut_off)

        # Checking pattern_w_ranges
        P = len(self.rlm.rids.pset.patterns)
        expected_pwr_shape = (P, 3)
        self.assertEqual(expected_pwr_shape, self.rlm.pattern_w_ranges.shape)
        expected_pwr = np.array([[0, 0, 10], [11, 11, 15]], dtype=np.int64)
        nptest.assert_array_equal(expected_pwr, self.rlm.pattern_w_ranges)

        # Checkin iwmap_pattern_offset
        expected_iwmap_po_shape = (P + 1,)
        self.assertEqual(expected_iwmap_po_shape, self.rlm.iwmap_pattern_offset.shape)
        expected_iwmap_po = np.array([0, 27, 108] , dtype=np.uint32)
        nptest.assert_array_equal(expected_iwmap_po, self.rlm.iwmap_pattern_offset)

        # Checking iwmap
        expected_iwmap_shape = (108,)
        self.assertEqual(expected_iwmap_shape, self.rlm.iwmap.shape)
        expected_iwmap = np.array(
            [ 1,  2,  3,  0,  4,  0,  5,  6,  7, -1,  0,  0, -1, -1,  8, -1, -1,  9, -1, -1,
              0, -1, -1,  0, -1, -1, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
             -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
             -1, -1, -1, -1, -1, -1, -1, 11, 12, -1, -1, 13, -1, -1, -1, -1, -1, -1, -1, 14,
             15, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
             -1, -1, -1, -1, -1, -1, -1, -1
             ], dtype=np.int64)
        nptest.assert_array_equal(expected_iwmap, self.rlm.iwmap)

        # Checking wmap
        expected_wmap_shape = (16, 3)
        self.assertEqual(expected_wmap_shape, self.rlm.wmap.shape)
        expected_wmap = np.array(
            [[ 0, -1,  6],
             [ 0,  0,  2],
             [ 0,  1,  7],
             [ 0,  2,  5],
             [ 0,  4,  2],
             [ 0,  6,  5],
             [ 0,  7,  2],
             [ 0,  8,  4],
             [ 0, 14,  3],
             [ 0, 17,  2],
             [ 0, 26,  2],
             [ 1, -1,  1],
             [ 1, 41,  2],
             [ 1, 44,  3],
             [ 1, 52,  2],
             [ 1, 53,  2],
             ], dtype=np.int64)
        nptest.assert_array_equal(expected_wmap, self.rlm.wmap)

        # Checking wmap_fallback
        expected_wmap_fallback_shape = (7, 3)
        self.assertEqual(expected_wmap_fallback_shape, self.rlm.wmap_fallback.shape)
        expected_wmap_fallback = np.array(
            [[ 0,  3,  1],
             [ 0,  5,  1],
             [ 0, 10,  1],
             [ 0, 11,  1],
             [ 0, 20,  1],
             [ 0, 23,  1],
             [ 1, 40,  1],
             ], dtype=np.int64)
        nptest.assert_array_equal(expected_wmap_fallback, self.rlm.wmap_fallback)
