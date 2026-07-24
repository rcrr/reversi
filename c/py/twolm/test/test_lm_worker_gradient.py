#
# test_lm_worker_gradient.py
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

# twolm/test/test_lm_worker_gradient.py
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


class TestLMWorkerGradient(unittest.TestCase):
    """Tests the GRADIENT worker integration and exact values using rlm_02.json."""

    def setUp(self):
        # Patch stdout to keep test output clean, especially for large datasets
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_02.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.LOW,
                                 base_dir_override=self.tmp_dir)
        
        # Move up to ZED to prepare the full state for GRADIENT
        self.rlm.move_to_step('ZED')
        self.assertEqual(self.rlm.current_step, 7)

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_generate_gradient(self):
        ctx = self.rlm.context

        # Before moving to GRADIENT, fg should be None
        self.assertIsNone(ctx.fg)

        # Verify prerequisites are correctly populated by previous steps
        self.assertEqual(ctx.design_matrix.shape, (199952, 8))
        self.assertEqual(len(ctx.w), 2981)

        # Move to GRADIENT
        self.rlm.move_to_step('GRADIENT')
        self.assertEqual(self.rlm.current_step, 8)
        
        self.assertIsNotNone(ctx.fg)
        self.assertTrue(callable(ctx.fg), "The attribute fg should be callable")

        # Evaluate the function at the initial weights (all zeros)
        f, g = ctx.fg(ctx.w)
        
        # Assertions on outputs
        expected_f = np.float32(0.02049378)
        nptest.assert_allclose(f, expected_f, rtol=1e-6, atol=1e-7)

        expected_g_0000 = np.float32(3.558e-4)
        nptest.assert_allclose(g[0], expected_g_0000, rtol=1e-4, atol=1e-7)

        expected_g_0001 = np.float32(-2.395e-4)
        nptest.assert_allclose(g[1], expected_g_0001, rtol=1e-4, atol=1e-7)

        expected_g_2980 = np.float32(1.101e-3)
        nptest.assert_allclose(g[2980], expected_g_2980, rtol=1e-4, atol=1e-7)


if __name__ == '__main__':
    unittest.main()
