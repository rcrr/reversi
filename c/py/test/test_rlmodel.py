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
        rlm = ReversiLogisticModel.from_json_path(json_config, base_dir_override=self.tmp_dir)

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
        self.rlm = ReversiLogisticModel.from_json_path(json_config, base_dir_override=self.tmp_dir)
        Path(self.rlm.cfg.full_project_dir).mkdir(parents=False, exist_ok=True)

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_load_regab_data_set_from_db(self):
        self.rlm.load_regab_data_set_from_db()
        self.assertIsNotNone(self.rlm.rds)

        if False:
            os.system(f"ls -l {self.rlm.cfg.full_project_dir}")

    def test_load_regab_indexed_data_set(self):
        self.assertIsNone(self.rlm.rids)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)

    def test_load_regab_indexed_data_set_from_cache_file(self):
        self.assertIsNone(self.rlm.rids)
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
        self.rlm = ReversiLogisticModel.from_json_path(json_config)
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
        self.assertIsNone(self.rlm.w)
        self.rlm.compute_wmaps()
        self.assertIsNotNone(self.rlm.pattern_w_ranges)
        self.assertIsNotNone(self.rlm.iwmap)
        self.assertIsNotNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNotNone(self.rlm.wmap)
        self.assertIsNotNone(self.rlm.wmap_fallback)
        self.assertIsNotNone(self.rlm.w)

        expected_cut_off = 2
        self.assertEqual(expected_cut_off, self.rlm.cfg.stat_model.frequency_cut_off)

        expected_logit_clipping = 0.03
        self.assertEqual(expected_logit_clipping, self.rlm.cfg.stat_model.logit_clipping)

        expected_ridge_regularization = 0.00
        self.assertEqual(expected_ridge_regularization, self.rlm.cfg.stat_model.ridge_regularization)
        
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

        self.assertTrue(np.unique(self.rlm.rids.pindexes).size == np.count_nonzero(self.rlm.iwmap >= 0))

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

        # Checking w (weights)
        expected_w_length = 16
        expected_w = np.zeros(expected_w_length, dtype=np.float32)
        w = self.rlm.w
        self.assertEqual(len(w), expected_w_length)
        self.assertEqual(w.shape, (expected_w_length,))
        self.assertEqual(w.dtype, np.float32)
        nptest.assert_array_equal(expected_w, w)

class TestReversiLogisticModelComputeDesignMatrix(unittest.TestCase):

    def setUp(self):
        json_config = 'py/test/data/tlm/rlmodel_01.json'
        self.rlm = ReversiLogisticModel.from_json_path(json_config)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)
        self.rlm.compute_wmaps()
        self.assertIsNotNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNotNone(self.rlm.iwmap)

    def tearDown(self):
        pass

    def test_compute_design_matrix(self):
        self.assertIsNone(self.rlm.X)
        self.rlm.compute_design_matrix()
        self.assertIsNotNone(self.rlm.X)

        X = self.rlm.X

        # Matrix X must have the same shape of principal indexes.
        self.assertEqual(X.shape, (10, 5))
        self.assertEqual(X.dtype, np.uint32)

        expected_X = np.array(
            [[ 2, 6,  5, 1, 13],
             [ 2, 0, 10, 2, 13],
             [ 3, 5,  7, 6, 13],
             [ 7, 5,  9, 1, 11],
             [ 7, 0,  7, 4, 12],
             [ 4, 0, 10, 0, 14],
             [ 3, 3,  0, 0, 15],
             [ 5, 5,  8, 2, 15],
             [ 8, 3,  2, 2, 14],
             [ 9, 8,  3, 2, 12],
             ], dtype=np.int64)
        nptest.assert_array_equal(expected_X, X)


class TestReversiLogisticModelStoreAndLoad(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        json_config_name = Path('rlmodel_02.json')
        json_config_src = Path('py/test/data/tlm') / json_config_name
        dst_dir = self.tmp_dir
        json_config = dst_dir / json_config_name
        shutil.copy(json_config_src, json_config)
        self.rlm = ReversiLogisticModel.from_json_path(json_config, base_dir_override=self.tmp_dir)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)
        self.rlm.compute_wmaps()
        self.assertIsNotNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNotNone(self.rlm.iwmap)
        self.assertIsNone(self.rlm.X)
        self.rlm.compute_design_matrix()
        self.assertIsNotNone(self.rlm.X)

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_store_and_load(self):
        self.assertEqual(True, True)

        if False:
            print(f"self.rlm.cfg.full_project_dir = {self.rlm.cfg.full_project_dir}")
            os.system(f"ls -l {self.rlm.cfg.full_project_dir}")

        filename = self.tmp_dir / Path('reversi_logistic_model_test_02.dat')
        self.rlm.store_to_file(filename)
        self.assertTrue(filename.is_file())

        m = ReversiLogisticModel.load_from_file(filename)

        nptest.assert_array_equal(self.rlm.pattern_w_ranges, m.pattern_w_ranges)
        nptest.assert_array_equal(self.rlm.iwmap_pattern_offset, m.iwmap_pattern_offset)
        nptest.assert_array_equal(self.rlm.iwmap, m.iwmap)
        nptest.assert_array_equal(self.rlm.wmap, m.wmap)
        nptest.assert_array_equal(self.rlm.wmap_fallback, m.wmap_fallback)
        nptest.assert_array_equal(self.rlm.X, m.X)

        # Verify the loaded dataset matches the original
        rds = self.rlm.rids.rds
        m_rds = m.rids.rds
        self.assertEqual(rds.bid, m_rds.bid)
        self.assertEqual(rds.status, m_rds.status)
        self.assertEqual(rds.ec, m_rds.ec)
        self.assertTrue(rds.positions.equals(m_rds.positions))
        self.assertEqual(rds.length, m_rds.length)

class TestReversiLogisticModelComputeZed(unittest.TestCase):

    def setUp(self):
        json_config = 'py/test/data/tlm/rlmodel_01.json'
        self.rlm = ReversiLogisticModel.from_json_path(json_config)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)
        self.rlm.compute_wmaps()
        self.assertIsNotNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNotNone(self.rlm.iwmap)
        self.assertIsNone(self.rlm.X)
        self.rlm.compute_design_matrix()
        self.assertIsNotNone(self.rlm.X)

    def tearDown(self):
        pass

    def test_zed_fun_factory(self):
        alpha = 0.02
        y2z, z2y = ReversiLogisticModel.zed_fun_factory(alpha)

        values = np.array([-64, 0, 6, +64], dtype=np.int8)
        transformed_values = y2z(values)
        expected_transformed_values = np.array([0.02, 0.5, 0.545, 0.98], dtype=np.float32)
        nptest.assert_allclose(expected_transformed_values, transformed_values, rtol=1e-6, atol=1e-7)
        
        re_transformed_values =z2y(transformed_values)
        nptest.assert_allclose(np.float32(values), re_transformed_values, rtol=1e-6, atol=1e-7)

        
    def test_compute_z(self):
        self.assertIsNone(self.rlm.y2z)
        self.assertIsNone(self.rlm.z)

        expected_logit_clipping = 0.03
        logit_clipping = self.rlm.cfg.stat_model.logit_clipping
        self.assertEqual(expected_logit_clipping, logit_clipping)

        expected_y = np.array([10, 36, -22, 18, -16, 14, -22, 20, 0, 2], dtype=np.int8)
        y = self.rlm.rids.rds.positions['game_value'].to_numpy()
        nptest.assert_array_equal(expected_y, y)

        self.rlm.compute_z()
        self.assertIsNotNone(self.rlm.y2z)
        
        self.assertIsNotNone(self.rlm.z)
        self.assertEqual(type(self.rlm.z), np.ndarray)
        self.assertEqual(self.rlm.z.dtype, np.float32)
        self.assertEqual(self.rlm.z.shape, (10,))
                
        expected_z = np.array([0.5734375,
                               0.764375,
                               0.3384375,
                               0.6321875,
                               0.3825,
                               0.6028125,
                               0.3384375,
                               0.646875,
                               0.5,
                               0.5146875], dtype=np.float32)

        nptest.assert_array_equal(expected_z, self.rlm.z)

class TestReversiLogisticModelGenerateGradient(unittest.TestCase):

    def setUp(self): 
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        json_config = 'py/test/data/tlm/rlmodel_03.json'
        self.rlm = ReversiLogisticModel.from_json_path(json_config, base_dir_override=self.tmp_dir)
        Path(self.rlm.cfg.full_project_dir).mkdir(parents=False, exist_ok=True)
        self.rlm.load_regab_indexed_data_set()
        self.assertIsNotNone(self.rlm.rids)
        self.rlm.compute_wmaps()
        self.assertIsNotNone(self.rlm.iwmap_pattern_offset)
        self.assertIsNotNone(self.rlm.iwmap)
        self.assertIsNone(self.rlm.X)
        self.rlm.compute_design_matrix()
        self.assertIsNotNone(self.rlm.X)
        self.rlm.compute_z()

    def tearDown(self):
        pass

    def test_generate_gradient(self):

        m = self.rlm

        self.assertIsNone(m.fg)

        self.assertEqual(m.X.shape, (199952, 8))

        m.generate_gradient()
        
        self.assertIsNotNone(m.fg)
        self.assertTrue(callable(m.fg), "The attribute fg should be callable")
        self.assertEqual(len(m.w), 2981)

        f, g = m.fg(m.w)
        
        expected_f = np.float32(4097.77197)
        nptest.assert_allclose(f, expected_f, rtol=1e-6, atol=1e-7)

        expected_g_0000 = np.float32(71.15)
        nptest.assert_allclose(g[0], expected_g_0000, rtol=1e-4, atol=1e-7)

        expected_g_0001 = np.float32(-47.89)
        nptest.assert_allclose(g[1], expected_g_0001, rtol=1e-4, atol=1e-7)

        expected_g_2980 = np.float32(220.11)
        nptest.assert_allclose(g[2980], expected_g_2980, rtol=1e-4, atol=1e-7)

class TestReversiLogisticModelFootprint(unittest.TestCase):

    def setUp(self): 
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        json_config = 'py/test/data/tlm/rlmodel_03.json'
        self.rlm = ReversiLogisticModel.from_json_path(json_config, base_dir_override=self.tmp_dir)
        Path(self.rlm.cfg.full_project_dir).mkdir(parents=False, exist_ok=True)
        self.rlm.build()

    def tearDown(self):
        pass
    
    def test_footprint(self):
        m = self.rlm

        self.assertIsNotNone(m.fg)
        
        if False:
            print(f"")
            m.footprint()

        
