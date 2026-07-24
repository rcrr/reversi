#
# test_lm_worker_optimize.py
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

# twolm/test/test_lm_worker_optimize.py
import unittest
import os
from unittest import skipUnless
from unittest.mock import patch
from io import StringIO

import tempfile
import shutil
from pathlib import Path

import numpy as np
import numpy.testing as nptest

from twolm.enums import Verbosity
from twolm.logistic_model import LogisticModel


class TestLMWorkerOptimize(unittest.TestCase):
    """Tests the OPTIMIZE worker integration."""

    def setUp(self):
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_02.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
        # Move to GRADIENT first
        self.rlm.move_to_step('GRADIENT')
        self.assertEqual(self.rlm.current_step, 8)

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_optimization_runs(self):
        """Run optimization and verify loss decreases."""
        ctx = self.rlm.context
        
        # Record initial loss (at w=0)
        initial_f, _ = ctx.fg(ctx.w)
        
        # Run optimization
        self.rlm.move_to_step('OPTIMIZE')
        self.assertEqual(self.rlm.current_step, 9)
        
        # Calculate final loss
        final_f, _ = ctx.fg(ctx.w)
        
        # The optimization should have significantly reduced the loss
        self.assertLess(final_f, initial_f)
        self.assertGreater(np.sum(np.abs(ctx.w)), 0.0, "Weights should not be all zeros after optimization.")

    def test_max_iters_treated_as_completed(self):
        """Test that reaching max iterations is treated as a valid completion and cached."""
        ctx = self.rlm.context
        
        # Override config to hit max_iters quickly
        ctx.cfg.optimization.max_iters = 10
        ctx.cfg.optimization.log_every_n = 0 # Keep test output clean
        
        # 1. First run: should run for 10 iterations and stop at MAX_ITERS_REACHED
        self.rlm.move_to_step('OPTIMIZE')
        self.assertEqual(self.rlm.current_step, 9)
        
        # Weights should be updated and not all zeros
        self.assertFalse(np.all(ctx.w == 0), "Weights should not be all zeros after 10 iterations.")
        weights_from_first_run = ctx.w.copy()
        
        # 2. Second run: new instance, same directory, same config
        rlm2 = LogisticModel(self.json_config,
                             verbosity=Verbosity.HIGH,
                             base_dir_override=self.tmp_dir)
        rlm2.move_to_step('CONFIG')
        rlm2.context.cfg.optimization.max_iters = 10
        
        # Clear buffer to capture second run logs
        self.mock_stdout.seek(0)
        self.mock_stdout.truncate(0)
        
        # Move to OPTIMIZE
        rlm2.move_to_step('OPTIMIZE')
        
        # Check logs to confirm it loaded from checkpoint instead of running
        logs = self.mock_stdout.getvalue()
        self.assertIn("Reached max iterations", logs)
        self.assertIn("Proceeding with current weights", logs)
        
        # And confirm L-BFGS did NOT run (no optimizer logs)
        self.assertNotIn("Starting L-BFGS optimization", logs)
        
        # Weights must be identical to the first run (loaded from checkpoint)
        nptest.assert_array_equal(rlm2.context.w, weights_from_first_run)


@skipUnless(os.environ.get('LONG') == '1', "Skipping long-running test (set LONG=1 to run)")
class TestLMWorkerOptimizeA2050(unittest.TestCase):
    """Tests the OPTIMIZE worker on the A2050 model."""

    suppress_stdout_io = False
    
    def setUp(self):
        if self.suppress_stdout_io:
            self.patcher_stdout = patch('sys.stdout', new=StringIO())
            self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_03.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
        # Move to GRADIENT first
        self.rlm.move_to_step('GRADIENT')
        self.assertEqual(self.rlm.current_step, 8)

    def tearDown(self):
        if self.suppress_stdout_io:        
            self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)


    def test_optimization(self):
        """Run optimization on the A2050 model."""
        ctx = self.rlm.context
        
        # Record initial loss (at w=0)
        initial_f, _ = ctx.fg(ctx.w)
        
        # Run optimization
        self.rlm.move_to_step('OPTIMIZE')
        self.assertEqual(self.rlm.current_step, 9)
        
        # Calculate final loss
        final_f, _ = ctx.fg(ctx.w)
        
        # The optimization should have significantly reduced the loss
        self.assertLess(final_f, initial_f)
        self.assertGreater(np.sum(np.abs(ctx.w)), 0.0, "Weights should not be all zeros after optimization.")


#: ###

if __name__ == '__main__':
    unittest.main()
