#
# test_rlm_optimize.py
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

# twolm/test/test_rlm_optimize.py
import unittest
import tempfile
import shutil
from pathlib import Path
from types import SimpleNamespace

import numpy as np
import numpy.testing as nptest

from twolm.rlm_optimize import (save_optimization_checkpoint,
                                load_optimization_checkpoint,
                                is_optimization_cache_consistent)


class TestRLMOptimizeIO(unittest.TestCase):
    """Tests the binary serialization and deserialization of the optimization checkpoint."""

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.filepath = Path(self.tmp_dir) / "checkpoint_test.dat"
        
        # Create dummy data to save
        self.w = np.array([0.1, 0.2, 0.3], dtype=np.float32)
        self.sl = [np.array([0.01, 0.02, 0.03], dtype=np.float32)]
        self.yl = [np.array([0.04, 0.05, 0.06], dtype=np.float32)]
        self.rho = [1.5]
        
        save_optimization_checkpoint(
            filepath=self.filepath,
            start_max_iters=500,
            start_m=50,
            is_converged=True,
            stop_reason="CONVERGED_MIN_GRAD",
            iterations_done=42,
            final_f=0.123,
            final_g_norm=0.001,
            w=self.w,
            sl=self.sl,
            yl=self.yl,
            rho=self.rho
        )

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_store_and_load_cycle(self):
        """Test that loading a checkpoint returns the exact saved data."""
        self.assertTrue(self.filepath.exists())
        
        cp = load_optimization_checkpoint(self.filepath)
        
        # Check scalars
        self.assertEqual(cp['start_max_iters'], 500)
        self.assertEqual(cp['start_m'], 50)
        self.assertTrue(cp['is_converged'])
        self.assertEqual(cp['stop_reason'], "CONVERGED_MIN_GRAD")
        self.assertEqual(cp['iterations_done'], 42)
        self.assertAlmostEqual(cp['final_f'], 0.123)
        self.assertAlmostEqual(cp['final_g_norm'], 0.001)
        
        # Check arrays
        nptest.assert_array_equal(cp['w'], self.w)
        self.assertEqual(len(cp['sl']), 1)
        nptest.assert_array_equal(cp['sl'][0], self.sl[0])
        nptest.assert_array_equal(cp['yl'][0], self.yl[0])
        self.assertEqual(cp['rho'][0], self.rho[0])


class TestRLMOptimizeCacheConsistency(unittest.TestCase):
    """Tests the cache validation logic."""

    def setUp(self):
        # Mock context with config
        self.ctx = SimpleNamespace()
        self.ctx.cfg = SimpleNamespace(
            optimization=SimpleNamespace(max_iters=500, m=50)
        )
        
        self.cp_valid = {
            'start_max_iters': 500,
            'start_m': 50
        }
        
        self.cp_wrong_iters = {
            'start_max_iters': 1000, # Changed
            'start_m': 50
        }
        
        self.cp_wrong_m = {
            'start_max_iters': 500,
            'start_m': 10 # Changed
        }

    def test_consistent_cache(self):
        self.assertTrue(is_optimization_cache_consistent(self.ctx, self.cp_valid))

    def test_inconsistent_iters(self):
        self.assertFalse(is_optimization_cache_consistent(self.ctx, self.cp_wrong_iters))

    def test_inconsistent_m(self):
        self.assertFalse(is_optimization_cache_consistent(self.ctx, self.cp_wrong_m))


if __name__ == '__main__':
    unittest.main()
