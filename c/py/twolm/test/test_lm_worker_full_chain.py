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
import tempfile
import shutil

import numpy as np
import numpy.testing as nptest

from unittest import skipUnless
from unittest.mock import patch
from io import StringIO
from pathlib import Path

from twolm.state_machine import Verbosity
from twolm.logistic_model import LogisticModel
from twolm.binio import verify_sha3_256_sidecar
from twolm.model_weights import model_weights_read_file
from twolm.rlm_client import make_evaluation_function
from twolm.rlm_gradient import sigmoid



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
        self.rlm.move_to_step('SAVE')

        # Validation Loss
        expected_vld_loss = 5.76E-03
        computed_vld_loss = ctx.vld_metrics['vld_loss']
        np.testing.assert_approx_equal(computed_vld_loss, expected_vld_loss, significant=3)

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

        # Verify file exists and checksum is valid
        output_path = ctx.cfg.base_dir / ctx.cfg.output
        self.assertTrue(output_path.exists(), f"Output file not found at {output_path}")
        self.assertTrue(verify_sha3_256_sidecar(output_path), "Checksum verification failed for saved model")
        
        # Read the file back using the Client's read function
        mw = model_weights_read_file(output_path, compressed=True)
        
        # Assert Metadata matches
        self.assertEqual(mw.name, ctx.cfg.name)
        self.assertEqual(mw.ec, ctx.cfg.regab_data_set.ec)
        self.assertAlmostEqual(mw.logit_clipping, ctx.cfg.stat_model.logit_clipping, places=5)

        # Assert Optimization info
        self.assertEqual(mw.opt_info['reason'], ctx.opt_info['reason'])
        self.assertEqual(mw.opt_info['iters'], ctx.opt_info['iters'])
        self.assertAlmostEqual(mw.opt_info['f'], ctx.opt_info['f'], places=6)

        # Assert Validation metrics
        self.assertEqual(mw.vld_metrics['vld_samples'], ctx.vld_metrics['vld_samples'])
        self.assertAlmostEqual(mw.vld_metrics['vld_rmse_y'], ctx.vld_metrics['vld_rmse_y'], places=4)

        # Assert FeatureSet matches (using the hash as a definitive structural check)
        self.assertEqual(mw.feature_set.hash, ctx.feature_set.hash, 
                         "Reconstructed FeatureSet hash does not match original")

        # Assert Core Inference Arrays match
        np.testing.assert_array_equal(mw.iwmap_feature_offset, ctx.iwmap_feature_offset)
        np.testing.assert_allclose(mw.w_dense, ctx.w_dense, rtol=1e-5, atol=1e-6)

        # Test the Client's Evaluation Function (EF)
        ef = make_evaluation_function(mw)

        sample_count = 100
        test_positions = ctx.positions[:sample_count]
        
        # Let's extract the indexes for the first sample_count positions
        idxs = ctx.rlm_indexes.indexes[:sample_count] # shape (sample_count, total_instances)
        offsets = ctx.iwmap_feature_offset
        
        # Expand offsets to match the number of instances (columns in idxs)
        n_instances_per_feature = [f.n_instances for f in ctx.feature_set.features]
        expanded_offsets = np.repeat(offsets[:-1], n_instances_per_feature)
        
        for i, p in enumerate(test_positions):
            # Client EF prediction
            y_client = ef(p)
                
            # Internal equivalent prediction using w_dense
            w_indices = expanded_offsets + idxs[i]
            weights = ctx.w_dense[w_indices]
            linear_pred = np.sum(weights)
            z_pred = sigmoid(linear_pred)
            y_internal = ctx.z2y(np.array([z_pred], dtype=np.float32))[0]
                
            # They should be virtually identical
            self.assertAlmostEqual(y_client, float(y_internal), places=4, 
                                   msg=f"EF mismatch at position {i}")


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
        self.rlm.move_to_step('SAVE')

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

        # --- Analytics Console Report Checks ---
        self.assertIsNotNone(ctx.analytics_report, "Analytics report should be stored in context")
        
        console_report = ctx.analytics_report
        # Check key console lines (no detailed tables should be here)
        self.assertIn("MODEL ANALYTICS REPORT: RGLM (Logistic) - Long Test 03 (rlm_03.json)", console_report)
        self.assertIn("  Generalization Gap (RMSE)   : 0.32", console_report)
        self.assertIn("  Loss (MSE/2)    | 1.4447e-03     | 1.5783e-03     | 2.0453e-02", console_report)
        # Ensure detailed sections are NOT in the console report
        self.assertNotIn("FEATURE SET SUMMARY:", console_report)
        self.assertNotIn("MOBILITY FEATURES DETAILS:", console_report)

        # --- Analytics File Report Checks ---
        report_path = ctx.cfg.base_dir / ctx.cfg.analytics.report_file_name
        self.assertTrue(report_path.exists(), f"Analytics report file not found at {report_path}")

        file_report = report_path.read_text(encoding="utf-8")
        
        # Check file specific headers
        self.assertIn("Creation Date               :", file_report)

        # Verify file exists and checksum is valid
        output_path = ctx.cfg.base_dir / ctx.cfg.output
        self.assertTrue(output_path.exists(), f"Output file not found at {output_path}")
        self.assertTrue(verify_sha3_256_sidecar(output_path), "Checksum verification failed for saved model")
        
        # Read the file back using the Client's read function
        mw = model_weights_read_file(output_path, compressed=True)
        
        # Assert Metadata matches
        self.assertEqual(mw.name, ctx.cfg.name)
        self.assertEqual(mw.ec, ctx.cfg.regab_data_set.ec)
        self.assertAlmostEqual(mw.logit_clipping, ctx.cfg.stat_model.logit_clipping, places=5)

        # Assert Optimization info
        self.assertEqual(mw.opt_info['reason'], ctx.opt_info['reason'])
        self.assertEqual(mw.opt_info['iters'], ctx.opt_info['iters'])
        self.assertAlmostEqual(mw.opt_info['f'], ctx.opt_info['f'], places=6)

        # Assert Validation metrics
        self.assertEqual(mw.vld_metrics['vld_samples'], ctx.vld_metrics['vld_samples'])
        self.assertAlmostEqual(mw.vld_metrics['vld_rmse_y'], ctx.vld_metrics['vld_rmse_y'], places=4)

        # Assert FeatureSet matches (using the hash as a definitive structural check)
        self.assertEqual(mw.feature_set.hash, ctx.feature_set.hash, 
                         "Reconstructed FeatureSet hash does not match original")

        # Assert Core Inference Arrays match
        np.testing.assert_array_equal(mw.iwmap_feature_offset, ctx.iwmap_feature_offset)
        np.testing.assert_allclose(mw.w_dense, ctx.w_dense, rtol=1e-5, atol=1e-6)

        # Test the Client's Evaluation Function (EF)
        ef = make_evaluation_function(mw)

        sample_count = 100
        test_positions = ctx.positions[:sample_count]
        
        # Let's extract the indexes for the first sample_count positions
        idxs = ctx.rlm_indexes.indexes[:sample_count] # shape (sample_count, total_instances)
        offsets = ctx.iwmap_feature_offset
        
        # Expand offsets to match the number of instances (columns in idxs)
        n_instances_per_feature = [f.n_instances for f in ctx.feature_set.features]
        expanded_offsets = np.repeat(offsets[:-1], n_instances_per_feature)
        
        for i, p in enumerate(test_positions):
            # Client EF prediction
            y_client = ef(p)
                
            # Internal equivalent prediction using w_dense
            w_indices = expanded_offsets + idxs[i]
            weights = ctx.w_dense[w_indices]
            linear_pred = np.sum(weights)
            z_pred = sigmoid(linear_pred)
            y_internal = ctx.z2y(np.array([z_pred], dtype=np.float32))[0]
                
            # They should be virtually identical
            self.assertAlmostEqual(y_client, float(y_internal), places=4, 
                                   msg=f"EF mismatch at position {i}")
        
#: ###

if __name__ == '__main__':
    unittest.main()
