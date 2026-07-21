#
# test_rlm_wmaps.py
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

# twolm/test/test_rlm_wmaps.py
import unittest
from unittest.mock import patch
from io import StringIO
import tempfile
import shutil
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import numpy.testing as nptest

from twolm.rlm_wmaps import (ReversiLogisticModelWMaps,
                             rlm_wmaps_compute,
                             rlm_wmaps_store_to_file,
                             rlm_wmaps_load_from_file,
                             rlm_wmaps_is_cache_consistent)


class TestRLMWmapsCompute(unittest.TestCase):
    """Tests the mathematical logic of rlm_wmaps_compute in isolation."""

    def setUp(self):
        # Create a dummy context to feed into rlm_wmaps_compute
        self.ctx = SimpleNamespace()
        
        # 1. Define a mock feature set with 2 features
        # Feature 0: 1 instance, 3 possible configurations (0, 1, 2)
        # Feature 1: 2 instances, 2 possible configurations (0, 1)
        self.ctx.feature_set = SimpleNamespace(
            hash="abc123",
            features=[
                SimpleNamespace(name="F0", n_instances=1, n_configurations=3),
                SimpleNamespace(name="F1", n_instances=2, n_configurations=2)
            ]
        )
        
        # 2. Define a mock configuration with a cut-off of 2
        self.ctx.cfg = SimpleNamespace(
            stat_model=SimpleNamespace(frequency_cut_off=2)
        )
        
        # 3. Define a mock indexes matrix (Shape: N=4, I=3)
        # F0 (col 0): 0 appears 2x, 1 appears 1x, 2 appears 1x
        # F1 (col 1,2): raveled 0 appears 4x, 1 appears 4x
        self.ctx.rlm_indexes = SimpleNamespace(
            indexes=np.array([
                [0, 0, 0],  # F0=0, F1=0,0
                [0, 0, 0],  # F0=0, F1=0,0
                [1, 1, 1],  # F0=1, F1=1,1
                [2, 1, 1],  # F0=2, F1=1,1
            ], dtype=np.uint32)
        )

    def test_compute_with_fallback(self):
        """Test WMAPS computation with frequency cut-off active."""
        wmaps = rlm_wmaps_compute(self.ctx)
        
        # Check basic attributes
        self.assertEqual(wmaps.feature_set_hash, "abc123")
        self.assertEqual(wmaps.cut_off, 2)
        self.assertEqual(len(wmaps.w), 4)  # Expected weights: 1 fallback + 1 above for F0, 2 above for F1
        
        # Check iwmap_feature_offset (accumulated n_configurations: [0, 3, 5])
        nptest.assert_array_equal(wmaps.iwmap_feature_offset, np.array([0, 3, 5], dtype=np.uint32))
        
        # Check feature_w_ranges [fallback, w_min, w_max]
        # F0: fallback=0, w_min=0, w_max=1
        # F1: fallback=-1, w_min=2, w_max=3
        nptest.assert_array_equal(wmaps.w_ranges, np.array([
            [0, 0, 1],
            [-1, 2, 3]
        ], dtype=np.int64))
        
        # Check iwmap (length 5)
        # F0 configs (0, 1, 2) -> 0 gets w=1, 1 gets w=0 (fallback), 2 gets w=0 (fallback)
        # F1 configs (0, 1)    -> 0 gets w=2, 1 gets w=3
        nptest.assert_array_equal(wmaps.iwmap, np.array([1, 0, 0, 2, 3], dtype=np.int64))
        
        # Check wmap [fid, config_id, freq]
        # w=0 (F0 fallback): config_id=-1, freq=2 (from configs 1 and 2)
        # w=1 (F0 config 0): config_id=0, freq=2
        # w=2 (F1 config 0): config_id=0, freq=4
        # w=3 (F1 config 1): config_id=1, freq=4
        nptest.assert_array_equal(wmaps.wmap, np.array([
            [0, -1, 2],
            [0,  0, 2],
            [1,  0, 4],
            [1,  1, 4]
        ], dtype=np.int64))
        
        # Check wmap_fallback [fid, config_id, freq]
        # F0 config 1 (freq 1) and F0 config 2 (freq 1)
        nptest.assert_array_equal(wmaps.wmap_fallback, np.array([
            [0, 1, 1],
            [0, 2, 1]
        ], dtype=np.int64))

    def test_compute_without_fallback(self):
        """Test WMAPS computation when cut-off is 0 (no fallbacks)."""
        # Modify cut-off to 0
        self.ctx.cfg.stat_model.frequency_cut_off = 0
        
        wmaps = rlm_wmaps_compute(self.ctx)
        
        # With no fallback, every unique config gets its own weight
        # F0 has 3 configs (w=0, 1, 2), F1 has 2 configs (w=3, 4)
        self.assertEqual(len(wmaps.w), 5)
        
        # wmap_fallback should be empty but maintain shape (0, 3)
        self.assertEqual(wmaps.wmap_fallback.shape, (0, 3))
        
        # F0 should have fallback -1
        self.assertEqual(wmaps.w_ranges[0, 0], -1)


# twolm/test/test_rlm_wmaps.py (sostituisci la classe TestRLMWmapsCacheConsistency)

class TestRLMWmapsCacheConsistency(unittest.TestCase):
    """Tests the cache validation logic."""

    def setUp(self):
        self.ctx = SimpleNamespace()
        self.ctx.feature_set = SimpleNamespace(hash="abc123")
        self.ctx.cfg = SimpleNamespace(stat_model=SimpleNamespace(frequency_cut_off=5))
        
        # Use empty arrays instead of None to satisfy Pydantic validation
        empty_arr = np.array([])
        
        self.wmaps_valid = ReversiLogisticModelWMaps(
            feature_set_hash="abc123", cut_off=5,
            w_ranges=empty_arr, iwmap_feature_offset=empty_arr, iwmap=empty_arr,
            wmap=empty_arr, wmap_fallback=empty_arr, w=np.zeros(1)
        )
        
        self.wmaps_wrong_hash = ReversiLogisticModelWMaps(
            feature_set_hash="wrong", cut_off=5,
            w_ranges=empty_arr, iwmap_feature_offset=empty_arr, iwmap=empty_arr,
            wmap=empty_arr, wmap_fallback=empty_arr, w=np.zeros(1)
        )
        
        self.wmaps_wrong_cutoff = ReversiLogisticModelWMaps(
            feature_set_hash="abc123", cut_off=99,
            w_ranges=empty_arr, iwmap_feature_offset=empty_arr, iwmap=empty_arr,
            wmap=empty_arr, wmap_fallback=empty_arr, w=np.zeros(1)
        )

    def test_consistent_cache(self):
        self.assertTrue(rlm_wmaps_is_cache_consistent(self.ctx, self.wmaps_valid))

    def test_inconsistent_hash(self):
        self.assertFalse(rlm_wmaps_is_cache_consistent(self.ctx, self.wmaps_wrong_hash))

    def test_inconsistent_cutoff(self):
        self.assertFalse(rlm_wmaps_is_cache_consistent(self.ctx, self.wmaps_wrong_cutoff))


class TestRLMWmapsIO(unittest.TestCase):
    """Tests the binary serialization and deserialization."""

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.filepath = Path(self.tmp_dir) / "wmaps_test.dat"
        
        # Create a fully populated WMaps object to serialize
        self.wmaps_original = ReversiLogisticModelWMaps(
            feature_set_hash="io_test_hash",
            cut_off=42,
            w_ranges=np.array([[0, 1, 2]], dtype=np.int64),
            iwmap_feature_offset=np.array([0, 3], dtype=np.uint32),
            iwmap=np.array([1, 0, 2], dtype=np.int64),
            wmap=np.array([[0, 0, 10], [0, 1, 20]], dtype=np.int64),
            wmap_fallback=np.array([[0, 2, 5]], dtype=np.int64),
            w=np.array([0.1, 0.2, 0.3], dtype=np.float32)
        )

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_store_and_load_cycle(self):
        """Test that storing and loading returns an identical object."""
        # Write to disk
        rlm_wmaps_store_to_file(self.wmaps_original, self.filepath)
        self.assertTrue(self.filepath.exists())
        
        # Read from disk
        wmaps_loaded = rlm_wmaps_load_from_file(self.filepath, checksum=False)
        
        # Validate scalar attributes
        self.assertEqual(wmaps_loaded.feature_set_hash, "io_test_hash")
        self.assertEqual(wmaps_loaded.cut_off, 42)
        
        # Validate numpy arrays
        nptest.assert_array_equal(wmaps_loaded.w_ranges, self.wmaps_original.w_ranges)
        nptest.assert_array_equal(wmaps_loaded.iwmap_feature_offset, self.wmaps_original.iwmap_feature_offset)
        nptest.assert_array_equal(wmaps_loaded.iwmap, self.wmaps_original.iwmap)
        nptest.assert_array_equal(wmaps_loaded.wmap, self.wmaps_original.wmap)
        nptest.assert_array_equal(wmaps_loaded.wmap_fallback, self.wmaps_original.wmap_fallback)
        nptest.assert_array_equal(wmaps_loaded.w, self.wmaps_original.w)
        nptest.assert_array_almost_equal(wmaps_loaded.w, self.wmaps_original.w)


if __name__ == '__main__':
    unittest.main()
