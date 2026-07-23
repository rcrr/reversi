#
# test_lm_worker_design_matrix.py
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

# twolm/test/test_lm_worker_design_matrix.py
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


class TestLMWorkerDesignMatrix(unittest.TestCase):
    """Tests for the DESIGN_MTR worker basic movements and cache interactions."""

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
        
        # Move to WMAPS first, as DESIGN_MTR depends on the iwmap and indexes
        self.rlm.move_to_step('WMAPS')
        self.assertEqual(self.rlm.current_step, 5)
        self.assertEqual(self.rlm.current_worker_name, 'WMAPS')

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_compute_and_populate_cache(self):
        """First run should compute the Design Matrix and create the cache file."""
        self.rlm.move_to_step('DESIGN_MTR')
        self.assertEqual(self.rlm.current_step, 6)
        self.assertEqual(self.rlm.current_worker_name, 'DESIGN_MTR')
        
        # Verify cache files were actually created on disk
        cache_file = Path(self.tmp_dir) / 'rlmwf_06_DESIGN_MTR.dat'
        checksum_file = Path(self.tmp_dir) / 'rlmwf_06_DESIGN_MTR.dat.SHA3-256'
        self.assertTrue(cache_file.exists(), "Cache .dat file was not created.")
        self.assertTrue(checksum_file.exists(), "Checksum file was not created.")
        
        # Check logs to confirm computation was executed
        logs = self.mock_stdout.getvalue()
        self.assertIn("Forcing computation", logs)
        
        # Verify context was updated
        self.assertIsNotNone(self.rlm.context.design_matrix)
        
        # The Design Matrix must have the same shape as the principal indexes matrix
        expected_shape = self.rlm.context.rlm_indexes.indexes.shape
        self.assertEqual(self.rlm.context.design_matrix.shape, expected_shape)
        
        # And must be of type uint32
        self.assertEqual(self.rlm.context.design_matrix.dtype, np.uint32)

    def test_read_cache_on_new_instance(self):
        """A second instance should read from the cache and NOT recompute."""
        
        # 1. Run the first instance to populate the cache
        self.rlm.move_to_step('DESIGN_MTR')
        self.assertEqual(self.rlm.current_step, 6)

        # 2. Create a completely new instance pointing to the same cache directory
        rlm2 = LogisticModel(self.json_config,
                             verbosity=Verbosity.HIGH,
                             base_dir_override=self.tmp_dir)
        rlm2.move_to_step('WMAPS') # Reach the step before DESIGN_MTR
        
        # Clear the StringIO buffer to only capture logs from the second run
        self.mock_stdout.seek(0)
        self.mock_stdout.truncate(0)
        
        # 3. Move to DESIGN_MTR in the second instance
        rlm2.move_to_step('DESIGN_MTR')
        self.assertEqual(rlm2.current_step, 6)
        
        # Check logs to confirm Cache was hit successfully
        logs = self.mock_stdout.getvalue()
        self.assertIn("Cache successfully loaded and validated", logs)
        
        # And confirm heavy computation was NOT executed
        self.assertNotIn("Forcing computation", logs, "Design Matrix was recomputed even though a valid cache existed.")
        
        # Verify context was updated with cached data
        self.assertIsNotNone(rlm2.context.design_matrix)
        expected_shape = rlm2.context.rlm_indexes.indexes.shape
        self.assertEqual(rlm2.context.design_matrix.shape, expected_shape)
        self.assertEqual(rlm2.context.design_matrix.dtype, np.uint32)


class TestLMWorkerDesignMatrixExactValues(unittest.TestCase):
    """Tests the exact values of the Design Matrix using rlm_01.json."""

    def setUp(self):
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_01.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
        # Move up to WMAPS to prepare the state for DESIGN_MTR
        self.rlm.move_to_step('WMAPS')
        self.assertEqual(self.rlm.current_step, 5)

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_compute_design_matrix_exact_values(self):
        """Compute the Design Matrix and verify exact values."""
        self.assertIsNone(self.rlm.context.design_matrix)
        
        self.rlm.move_to_step('DESIGN_MTR')
        
        self.assertIsNotNone(self.rlm.context.design_matrix)
        
        X = self.rlm.context.design_matrix

        # Matrix X must have the same shape of principal indexes.
        self.assertEqual(X.shape, (10, 7))
        self.assertEqual(X.dtype, np.uint32)
        
        expected_X = np.array([
            [ 0, 4,  7, 11, 10,  6, 18 ],
            [ 0, 1,  7,  5, 15,  7, 18 ],
            [ 0, 3,  8, 10, 12, 11, 18 ],
            [ 0, 4, 12, 10, 14,  6, 16 ],
            [ 0, 1, 12,  5, 12,  9, 17 ],
            [ 0, 4,  9,  5, 15,  5, 19 ],
            [ 0, 1,  8,  8,  5,  5, 20 ],
            [ 0, 3, 10, 10, 13,  7, 20 ],
            [ 0, 2, 13,  8,  7,  7, 19 ],
            [ 0, 2, 14, 13,  8,  7, 17 ],
        ], dtype=np.uint32)
        
        nptest.assert_array_equal(expected_X, X)


if __name__ == '__main__':
    unittest.main()
