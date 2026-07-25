#
# rlm_config_schemas.py
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

# twolm/rlm_validate.py
from __future__ import annotations

from typing import TYPE_CHECKING, Dict
import numpy as np
from scipy.special import expit

from twolm.enums import Relevance
from twolm.regab import RegabDBConnection, regab_extract_data_set_from_db

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['validate_model']


def validate_model(ctx: "RLMContext") -> Dict[str, float]:
    """
    Extracts validation data, computes predictions using w_dense, and calculates metrics.
    """
    vld_cfg = ctx.cfg.validation_data_set
    train_cfg = ctx.cfg.regab_data_set
    
    # 1. Extract validation data from DB
    conn_params = train_cfg.regab_db_connection
    rc = RegabDBConnection(conn_params.dbname, conn_params.user, conn_params.host)
    ctx.log_event(Relevance.INFO, "Connecting to DB for validation data...")
    
    try:
        # Reuse 'ec' from training config
        rds_vld = regab_extract_data_set_from_db(rc, vld_cfg.bid, vld_cfg.status, train_cfg.ec)
        vld_positions, vld_game_values = rds_vld.generate_positions_and_game_values()
        ctx.log_event(Relevance.INFO, f"Extracted {len(vld_positions):,} validation positions.")
    finally:
        rc.close()

    # 2. Compute indexes for validation positions using the SAME feature_set
    ctx.log_event(Relevance.INFO, "Computing indexes for validation set...")
    vld_indexes = ctx.feature_set.compute_indexes(vld_positions)
    M_vld, P_vld = vld_indexes.shape

    # 3. Map to dense weights and compute linear predictor
    # We need the column offsets for each feature instance
    n_instances_per_feature = [f.n_instances for f in ctx.feature_set.features]
    col_offsets = np.repeat(ctx.iwmap_feature_offset[:-1], n_instances_per_feature)
    
    # Apply offsets to raw indexes to point directly into w_dense
    dense_indices = vld_indexes + col_offsets

    ctx.log_event(Relevance.INFO, "Computing predictions (forward pass) on validation set...")
    # Cast to float64 for the sum to prevent any accumulation errors
    linear_predictor = np.sum(ctx.w_dense[dense_indices].astype(np.float64), axis=1)
    
    # Predicted probabilities
    z_pred = expit(linear_predictor)

    # 4. Compute true Z for validation set
    y_vld = np.asarray(vld_game_values, dtype=np.int8)
    z_vld = ctx.y2z(y_vld)

    # 5. Calculate Metrics
    rn = z_pred - z_vld
    norm_rn = np.dot(rn, rn)
    
    # Normalized MSE (consistent with training loss, divided by 2)
    vld_loss = 0.5 * (norm_rn / M_vld)
    
    # Mean Squared Error
    mse = norm_rn / M_vld
    
    # Mean Absolute Error
    mae = np.mean(np.abs(rn))

    return {
        'vld_loss': float(vld_loss),
        'vld_mse': float(mse),
        'vld_mae': float(mae),
        'vld_samples': int(M_vld)
    }
