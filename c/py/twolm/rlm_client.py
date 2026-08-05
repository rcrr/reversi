#
# rlm_client.py
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

# twolm/rlm_client.py
from __future__ import annotations

import numpy as np

from typing import Callable
from scipy.special import expit

from twolm.model_weights import ModelWeights, model_weights_read_file
from twolm.rlm_zed import zed_fun_factory
from twolm.board import Position


def make_evaluation_function(mw: ModelWeights) -> Callable[[Position], float]:
    """
    Factory that creates a scalar Evaluation Function (EF) in Y space.
    """
    offsets = mw.iwmap_feature_offset
    _, z2y = zed_fun_factory(mw.logit_clipping)
    
    # Pre-compute the expanded offsets for all instances
    # e.g. if EDGE has 4 instances, its offset is repeated 4 times
    n_instances_per_feature = [f.n_instances for f in mw.feature_set.features]
    expanded_offsets = np.repeat(offsets[:-1], n_instances_per_feature)

    def ef(p: Position) -> float:
        # 1. Compute Principal Indexes (returns array of length total_instances)
        indexes = mw.feature_set.compute_indexes(np.array([p]))[0]
        
        # 2. Map directly to w_dense using expanded offsets
        w_indices = expanded_offsets + indexes
        weights = mw.w_dense[w_indices]
        
        # 3. Linear predictor and Sigmoid (Z space)
        linear_predictor = np.sum(weights)
        z_pred = expit(linear_predictor)
        
        # 4. Transform back to Y space
        y_pred = z2y(np.array([z_pred], dtype=np.float32))[0]
        
        return float(y_pred)

    return ef
