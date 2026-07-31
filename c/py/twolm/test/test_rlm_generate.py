#
# test_rlm_generate.py
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

# twolm/test/test_rlm_generate.py
import unittest
from types import SimpleNamespace
import numpy as np
import numpy.testing as nptest

from twolm.rlm_generate import compute_w_dense


class TestRLMGenerate(unittest.TestCase):
    """Tests the dense weight generation logic."""

    def setUp(self):
        # Mock context with 3 features: Intercept, Pattern, Mobility
        self.ctx = SimpleNamespace()
        self.ctx.z = np.array([0.6, 0.4, 0.6, 0.4], dtype=np.float32) # mean z = 0.5
        
        # Feature 0: Intercept (cat 0, 1 config)
        # Feature 1: Pattern (cat 2, 3 configs) -> AGGIUNTO ref mock
        # Feature 2: Mobility (cat 1, 5 configs)
        self.ctx.feature_set = SimpleNamespace(
            hash="abc",
            features=[
                SimpleNamespace(name="INT", category=0, n_instances=1, n_configurations=1),
                SimpleNamespace(name="PAT", category=2, n_instances=1, n_configurations=3, 
                                ref=SimpleNamespace(convert_to_principal_index=lambda x: x)), # Mock identity
                SimpleNamespace(name="MOB", category=1, n_instances=1, n_configurations=5)
            ]
        )
        
        # iwmap_feature_offset: [0, 1, 4, 9]
        self.ctx.iwmap_feature_offset = np.array([0, 1, 4, 9], dtype=np.uint32)
        
        # iwmap
        # INT (idx 0): [0]
        # PAT (idx 1,2,3): [-1, 1, 0]  (config 0 fallback->0, config 1->1, config 2 fallback->0)
        # MOB (idx 4,5,6,7,8): [-1, 2, -1, 3, -1] (config 1->2, config 3->3)
        self.ctx.iwmap = np.array(
            [0,  -1, 1, 0,  -1, 2, -1, 3, -1], 
            dtype=np.int64
        )
        
        # w_ranges [fallback, w_min, w_max]
        # INT: [ -1, 0, 0 ]
        # PAT: [ 0, 0, 1 ]  (fallback is weight 0, w_min=0, w_max=1)
        # MOB: [ -1, 2, 3 ] (no fallback, w_min=2, w_max=3)
        self.ctx.feature_w_ranges = np.array([
            [-1, 0, 0],
            [ 0, 0, 1],
            [-1, 2, 3]
        ], dtype=np.int64)
        
        # wmap [fid, config_id, freq]
        # PAT config 1 has freq 10. PAT fallback (-1) has freq 3.
        # MOB config 1 has freq 5, config 3 has freq 5. No fallback.
        self.ctx.wmap = np.array([
            [1, -1, 3],
            [1, 1, 10],
            [2, 1, 5],
            [2, 3, 5]
        ], dtype=np.int64)
        
        # Compressed weights vector w
        # w[0] = 0.5 (Intercept, also Pattern fallback)
        # w[1] = -0.2 (Pattern config 1)
        # w[2] = 1.0 (Mobility config 1)
        # w[3] = 3.0 (Mobility config 3)
        self.ctx.w = np.array([0.5, -0.2, 1.0, 3.0], dtype=np.float32)
        
        # Mock logger
        self.ctx.log_event = lambda rel, msg: None

    def test_compute_w_dense(self):
        dense_obj = compute_w_dense(self.ctx)
        w_dense = dense_obj.w_dense
        
        self.assertEqual(w_dense.dtype, np.float32)
        self.assertEqual(len(w_dense), 9) # 1 + 3 + 5
        
        # Intercept (index 0)
        nptest.assert_allclose(w_dense[0], 0.5)
        
        # Pattern (indices 1, 2, 3)
        # Config 0 is fallback -> weighted_mean = (-0.2 * 10) / 10 = -0.2
        # Config 1 is seen -> -0.2
        # Config 2 is fallback -> -0.2
        nptest.assert_allclose(w_dense[1:4], np.array([-0.2, -0.2, -0.2], dtype=np.float32))
        
        # Mobility (indices 4, 5, 6, 7, 8)
        # Config 0 (idx 4) unseen -> clamp to config 1 (1.0)
        # Config 1 (idx 5) seen -> 1.0
        # Config 2 (idx 6) unseen -> interpolate between 1.0 and 3.0 -> 2.0
        # Config 3 (idx 7) seen -> 3.0
        # Config 4 (idx 8) unseen -> clamp to config 3 (3.0)
        expected_mob = np.array([1.0, 1.0, 2.0, 3.0, 3.0], dtype=np.float32)
        nptest.assert_allclose(w_dense[4:9], expected_mob)


if __name__ == '__main__':
    unittest.main()
