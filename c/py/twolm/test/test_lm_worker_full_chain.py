#
# test_lm_worker_full_chain.py
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

# twolm/test/test_lm_worker_full_chain.py
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


class TestFullChainA2030(unittest.TestCase):
    """Tests the full Reversi Logistic Model chain on the A2030 configuration."""

    suppress_stdout_io = True
    
    def setUp(self):
        if self.suppress_stdout_io:
            self.patcher_stdout = patch('sys.stdout', new=StringIO())
            self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_04.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
    def tearDown(self):
        if self.suppress_stdout_io:        
            self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)


    def test_full_worker_chain(self):
        """Run full chain on the A2030 model."""
        ctx = self.rlm.context
        
        # Run the model chain
        self.rlm.move_to_step('ANALYTICS')

        # Validation Loss
        expected_vld_loss = 5.76E-03
        np.testing.assert_approx_equal(ctx.vld_loss, expected_vld_loss, significant=3)

        m = ctx.vld_metrics
        
        # Validation samples
        expected_vld_samples = 20_000
        self.assertEqual(m['vld_samples'], expected_vld_samples)
        
        # Validation MAE(y)
        expected_vld_mae_y = 11.053
        np.testing.assert_approx_equal(m['vld_mae_y'], expected_vld_mae_y, significant=4)
        
        # Validation RMSE(y)
        expected_vld_rmse_y = 14.021
        np.testing.assert_approx_equal(m['vld_rmse_y'], expected_vld_rmse_y, significant=4)


@skipUnless(os.environ.get('LONG') == '1', "Skipping long-running test (set LONG=1 to run)")
class TestFullChainA2050(unittest.TestCase):
    """Tests the full Reversi Logistic Model chain on the A2050 configuration."""

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
        
    def tearDown(self):
        if self.suppress_stdout_io:        
            self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)


    def test_full_worker_chain(self):
        """Run full chain on the A2050 model."""
        ctx = self.rlm.context
        
        # Run the model chain
        self.rlm.move_to_step('ANALYTICS')

        # Validation Loss
        expected_vld_loss = 1.578E-03
        np.testing.assert_approx_equal(ctx.vld_loss, expected_vld_loss, significant=3)

        m = ctx.vld_metrics
        
        # Validation samples
        expected_vld_samples = 199_932
        self.assertEqual(m['vld_samples'], expected_vld_samples)
        
        # Validation MAE(y)
        expected_vld_mae_y = 5.643
        np.testing.assert_approx_equal(m['vld_mae_y'], expected_vld_mae_y, significant=4)
        
        # Validation RMSE(y)
        expected_vld_rmse_y = 7.338
        np.testing.assert_approx_equal(m['vld_rmse_y'], expected_vld_rmse_y, significant=4)

#:
        # --- Analytics Console Report Checks ---
        self.assertIsNotNone(ctx.analytics_report, "Analytics report should be stored in context")
        
        console_report = ctx.analytics_report
        # Check key console lines (no detailed tables should be here)
        self.assertIn("MODEL ANALYTICS REPORT: RGLM (Reversi Generalized Linear Model) Logistic: Edge and Mobility", console_report)
        self.assertIn("  Generalization Gap (RMSE)   : 0.19", console_report)
        self.assertIn("  Loss (MSE/2)    | 5.6141e-03     | 5.7623e-03     | 2.0438e-02", console_report)
        # Ensure detailed sections are NOT in the console report
        self.assertNotIn("FEATURE SET SUMMARY:", console_report)
        self.assertNotIn("MOBILITY FEATURES DETAILS:", console_report)

        # --- Analytics File Report Checks ---
        report_path = ctx.cfg.base_dir / ctx.cfg.analytics.report_file_name
        self.assertTrue(report_path.exists(), f"Analytics report file not found at {report_path}")

        file_report = report_path.read_text(encoding="utf-8")
        
        # Check file specific headers
        self.assertIn("Creation Date               :", file_report)
        self.assertIn("Training/Validation Records : 199,932 / 20,000", file_report)
        self.assertIn("Frequency Cut-Off           : 10", file_report)
        
        # Check Feature Set Summary
        self.assertIn("FEATURE SET SUMMARY:", file_report)
        self.assertIn("FeatureSet: name = EDGE_and_MOBILITY", file_report)
        
        # Check Mobility Details
        self.assertIn("MOBILITY FEATURE: LMC (fid=1) (Fallback: w_idx=1, weight=+0.295, freq=5)", file_report)
        self.assertIn("  Eta-Squared (Discrimination): 0.3050", file_report)
        # Check a specific row in the mobility table
        self.assertIn("  10           | C        | 11       | 9        |     26,262 |      -4.10 |    22.46 |     +0.015", file_report)
        # Check omitted rows (since detailed_report is empty in test config, rows should be omitted)
        self.assertIn("(Table rows omitted: feature not in config 'analytics.detailed_report')", file_report)
        
        # Check Pattern Details
        self.assertIn("PATTERN FEATURE: EDGE (fid=2) (Fallback: w_idx=22, weight=+0.121, freq=1,927)", file_report)
        self.assertIn("  Eta-Squared (Discrimination): 0.2170", file_report)
        # Check a specific row in the pattern table
        self.assertIn("  66           | C        | 23       | 0        | 0              |     17,912 |      -2.08 |    27.84 |     -0.185", file_report)

        
#: ###

if __name__ == '__main__':
    unittest.main()
