#
# test_lm_worker_wmaps.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
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

# twolm/test/test_lm_worker_wmaps.py
import unittest
from unittest.mock import patch
from io import StringIO

import tempfile
import shutil
from pathlib import Path

import numpy as np
import numpy.testing as nptest

from twolm.enums import Verbosity
from twolm.logistic_model import LogisticModel


class TestLMWorkerWmaps(unittest.TestCase):
    """Tests for the WMAPS worker basic movements and cache interactions."""

    def setUp(self):
        # Patch stdout to keep test output clean and capture logs
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        
        # Instance 1: Will populate the cache
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
        self.assertEqual(self.rlm.current_step, 0)
        self.assertEqual(self.rlm.current_worker_name, 'CREATED')
        
        # Move to INDEXES first, as WMAPS depends on the rlm_indexes loaded there
        self.rlm.move_to_step('INDEXES')
        self.assertEqual(self.rlm.current_step, 4)
        self.assertEqual(self.rlm.current_worker_name, 'INDEXES')

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_compute_and_populate_cache(self):
        """First run should compute WMAPS and create the cache file."""
        self.rlm.move_to_step('WMAPS')
        self.assertEqual(self.rlm.current_step, 5)
        self.assertEqual(self.rlm.current_worker_name, 'WMAPS')
        
        # Verify cache files were actually created on disk
        cache_file = Path(self.tmp_dir) / 'rlmwf_05_WMAPS.dat'
        checksum_file = Path(self.tmp_dir) / 'rlmwf_05_WMAPS.dat.SHA3-256'
        self.assertTrue(cache_file.exists(), "Cache .dat file was not created.")
        self.assertTrue(checksum_file.exists(), "Checksum file was not created.")
        
        # Check logs to confirm computation was executed
        logs = self.mock_stdout.getvalue()
        self.assertIn("Forcing computation", logs)
        
        # Verify context was updated with the 6 attributes
        self.assertIsNotNone(self.rlm.context.feature_w_ranges)
        self.assertIsNotNone(self.rlm.context.iwmap_feature_offset)
        self.assertIsNotNone(self.rlm.context.iwmap)
        self.assertIsNotNone(self.rlm.context.wmap)
        self.assertIsNotNone(self.rlm.context.wmap_fallback)
        self.assertIsNotNone(self.rlm.context.w)
        self.assertGreater(len(self.rlm.context.w), 0, "Weight vector W is empty.")

    def test_read_cache_on_new_instance(self):
        """A second instance should read from the cache and NOT recompute."""
        
        # 1. Run the first instance to populate the cache
        self.rlm.move_to_step('WMAPS')
        self.assertEqual(self.rlm.current_step, 5)

        # 2. Create a completely new instance pointing to the same cache directory
        rlm2 = LogisticModel(self.json_config,
                             verbosity=Verbosity.HIGH,
                             base_dir_override=self.tmp_dir)
        rlm2.move_to_step('INDEXES') # Reach the step before WMAPS
        
        # Clear the StringIO buffer to only capture logs from the second run
        self.mock_stdout.seek(0)
        self.mock_stdout.truncate(0)
        
        # 3. Move to WMAPS in the second instance
        rlm2.move_to_step('WMAPS')
        self.assertEqual(rlm2.current_step, 5)
        
        # Check logs to confirm Cache was hit successfully
        logs = self.mock_stdout.getvalue()
        self.assertIn("Cache successfully loaded and validated", logs)
        
        # And confirm heavy computation was NOT executed
        self.assertNotIn("Forcing computation", logs, "WMAPS were recomputed even though a valid cache existed.")
        
        # Verify context was updated with cached data
        self.assertIsNotNone(rlm2.context.feature_w_ranges)
        self.assertIsNotNone(rlm2.context.w)
        self.assertGreater(len(rlm2.context.w), 0, "Weight vector W is empty in cached run.")


class TestLMWorkerWmapsExactValues(unittest.TestCase):
    """Tests the exact values of the WMAPS computation using rlm_01.json."""

    def setUp(self):
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_01.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
        # Move to INDEXES first
        self.rlm.move_to_step('INDEXES')
        self.assertEqual(self.rlm.current_step, 4)

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_compute_wmaps_exact_values(self):
        ctx = self.rlm.context
        
        self.assertIsNone(ctx.feature_w_ranges)
        self.assertIsNone(ctx.iwmap_feature_offset)
        self.assertIsNone(ctx.iwmap)
        self.assertIsNone(ctx.wmap)
        self.assertIsNone(ctx.wmap_fallback)
        self.assertIsNone(ctx.w)
        
        self.rlm.move_to_step('WMAPS')
        
        self.assertIsNotNone(ctx.feature_w_ranges)
        self.assertIsNotNone(ctx.iwmap)
        self.assertIsNotNone(ctx.iwmap_feature_offset)
        self.assertIsNotNone(ctx.wmap)
        self.assertIsNotNone(ctx.wmap_fallback)
        self.assertIsNotNone(ctx.w)

        expected_cut_off = 2
        self.assertEqual(expected_cut_off, ctx.cfg.stat_model.frequency_cut_off)

        expected_logit_clipping = 0.03
        self.assertEqual(expected_logit_clipping, ctx.cfg.stat_model.logit_clipping)

        expected_ridge_regularization = 0.00
        self.assertEqual(expected_ridge_regularization, ctx.cfg.stat_model.ridge_regularization)
        
        # Checking feature_w_ranges
        # NOTE: F is now the count of all features (Intercept + Mobility + Patterns)
        F = len(ctx.feature_set.features)
        expected_fwr_shape = (F, 3)
        self.assertEqual(expected_fwr_shape, ctx.feature_w_ranges.shape)
        
        expected_fwr = np.array([[-1, 0, 0], [1, 1, 4], [5, 5, 15], [16, 16, 20]], dtype=np.int64)
        nptest.assert_array_equal(expected_fwr, ctx.feature_w_ranges)

        # Checking iwmap_feature_offset
        expected_iwmap_fo_shape = (F + 1,)
        self.assertEqual(expected_iwmap_fo_shape, ctx.iwmap_feature_offset.shape)
        
        expected_iwmap_fo = np.array([0, 1, 66, 93, 174], dtype=np.uint32)
        nptest.assert_array_equal(expected_iwmap_fo, ctx.iwmap_feature_offset)

        # Checking iwmap
        expected_iwmap_shape = (174,)
        self.assertEqual(expected_iwmap_shape, ctx.iwmap.shape)
        expected_iwmap = np.array(
            [  0, -1, -1, -1, -1, -1, -1,  1, -1,  2,  3, -1,  1, -1,  4, -1,  1, -1, -1, -1, -1, -1, -1, -1,
              -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
              -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  6,  7,  8,  5,  9,  5,
              10, 11, 12, -1,  5,  5, -1, -1, 13, -1, -1, 14, -1, -1,  5, -1, -1,  5, -1, -1, 15, -1, -1, -1,
              -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
              -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 16, 17, -1, -1, 18, -1, -1, -1, -1, -1, -1,
              -1, 19, 20, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
              -1, -1, -1, -1, -1, -1], dtype=np.int64)
        nptest.assert_array_equal(expected_iwmap, ctx.iwmap)

        # Checking wmap
        expected_wmap_shape = (21, 3)
        self.assertEqual(expected_wmap_shape, ctx.wmap.shape)
        expected_wmap = np.array(
            [[ 0,  0, 10],
             [ 1, -1,  3],
             [ 1,  8,  2],
             [ 1,  9,  2],
             [ 1, 13,  3],
             [ 2, -1,  6],
             [ 2,  0,  2],
             [ 2,  1,  7],
             [ 2,  2,  5],
             [ 2,  4,  2],
             [ 2,  6,  5],
             [ 2,  7,  2],
             [ 2,  8,  4],
             [ 2, 14,  3],
             [ 2, 17,  2],
             [ 2, 26,  2],
             [ 3, -1,  1],
             [ 3, 41,  2],
             [ 3, 44,  3],
             [ 3, 52,  2],
             [ 3, 53,  2],
             ], dtype=np.int64)
        nptest.assert_array_equal(expected_wmap, ctx.wmap)

        # Checking wmap_fallback
        expected_wmap_fallback_shape = (10, 3)
        self.assertEqual(expected_wmap_fallback_shape, ctx.wmap_fallback.shape)
        expected_wmap_fallback = np.array(
            [[ 1,  6,  1],
             [ 1, 11,  1],
             [ 1, 15,  1],
             [ 2,  3,  1],
             [ 2,  5,  1],
             [ 2, 10,  1],
             [ 2, 11,  1],
             [ 2, 20,  1],
             [ 2, 23,  1],
             [ 3, 40,  1],
             ], dtype=np.int64)
        nptest.assert_array_equal(expected_wmap_fallback, ctx.wmap_fallback)

        # Checking w (weights)
        expected_w_length = 21
        expected_w = np.zeros(expected_w_length, dtype=np.float32)
        w = ctx.w
        self.assertEqual(len(w), expected_w_length)
        self.assertEqual(w.shape, (expected_w_length,))
        self.assertEqual(w.dtype, np.float32)
        nptest.assert_array_equal(expected_w, w)


if __name__ == '__main__':
    unittest.main()
