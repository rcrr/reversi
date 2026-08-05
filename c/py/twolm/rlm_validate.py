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

import numpy as np

from typing import TYPE_CHECKING, Dict

from twolm.rlm_gradient import sigmoid
from twolm.state_machine import Relevance
from twolm.regab import RegabDBConnection, regab_extract_data_set_from_db

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['validate_model']



def validate_model(ctx: "RLMContext") -> Dict[str, Any]:
    """
    Extracts validation data, computes predictions using w_dense, and calculates metrics.

    Validation Metrics for Logistic Curve Fitting on an Ordinal Scale
    -----------------------------------------------------------------
    When using a logistic model as a non-linear regression function, the discrete game 
    values (Y, on an ordinal scale like [-64, +64] with steps of 2) are transformed 
    into a continuous target space Z (range [0, 1]) via an affine transformation.
    
    The metrics below are computed in this Z space. They act as pure geometric 
    indicators of distance, measuring how well the continuous logistic curve aligns 
    with the discrete target points.

    1. MAE (Mean Absolute Error)
    ----------------------------
    MAE computes the average of the absolute differences between the true transformed 
    values (z) and the continuous predictions from the logistic curve (z_hat).
    
    MAE = (1 / n) * sum(|z_i - z_hat_i|)

    - Physical Interpretation: It tells you, on average, how far the logistic curve 
      misses the true target in the Z space. Because Z is bounded between 0 and 1, 
      an MAE of 0.04 means the model's predictions deviate by 4% of the total scale 
      on average.
    - Mathematical Weight: MAE treats all errors linearly. A single large mistake 
      affects the final metric exactly the same as multiple small mistakes. It 
      provides an intuitive baseline for the "typical" accuracy of the curve.

    2. MSE (Mean Squared Error)
    ---------------------------
    MSE computes the average of the squared differences between the true transformed 
    values and the logistic predictions.
    
    MSE = (1 / n) * sum((z_i - z_hat_i)^2)

    - Physical Interpretation: It measures the variance of the residuals. To bring 
      it back to the Z scale units, you can take its square root to get the RMSE 
      (Root Mean Squared Error), which represents the standard deviation of the 
      prediction error.
    - Mathematical Weight: MSE penalizes outliers and large deviations aggressively. 
      If the curve misses a point by 0.1, it adds a penalty of 0.01; a miss of 0.5 
      adds a penalty of 0.25.
    - Why it matters for the Logistic Curve: Logistic curves naturally flatten out 
      into horizontal asymptotes at their tails (near 0 and 1) and have a steep 
      inflection point in the middle. If the empirical data does not flatten out at 
      the same rate as the mathematical curve, the model will generate massive errors 
      at the boundaries. MSE is the perfect alarm system to tell you if the logistic 
      shape is fundamentally a bad fit for certain regions of the dataset.

    3. RMSE (Root Mean Squared Error)
    ---------------------------------
    It is computed as:

    RMSE = sqrt(MSE)

    - When RMSE / MAE = sqrt(pi/2) = 1.253... the distribution is normal.
    
    """
    vld_positions = ctx.vld_positions
    vld_game_values = ctx.vld_game_values
        
    # Compute indexes for validation positions using the SAME feature_set
    ctx.log_event(Relevance.INFO, "Computing indexes for validation set...")
    vld_indexes = ctx.feature_set.compute_indexes(vld_positions)
    vld_M, vld_P = vld_indexes.shape

    # Map to dense weights and compute linear predictor
    # We need the column offsets for each feature instance
    n_instances_per_feature = [f.n_instances for f in ctx.feature_set.features]
    col_offsets = np.repeat(ctx.iwmap_feature_offset[:-1], n_instances_per_feature)
    
    # Apply offsets to raw indexes to point directly into w_dense
    dense_indices = vld_indexes + col_offsets

    # Predictions in Z space [0, 1]
    ctx.log_event(Relevance.INFO, "Computing predictions (forward pass) on validation set...")
    linear_predictor = np.sum(ctx.w_dense[dense_indices], axis=1)
    z_pred = sigmoid(linear_predictor)

    # True values in Z space
    vld_z = ctx.y2z(vld_game_values)

    # Calculate Metrics in Z space
    rn_z = z_pred - vld_z
    norm_rn_z = np.dot(rn_z, rn_z)
    vld_loss = 0.5 * (norm_rn_z / vld_M)
    mse_z = norm_rn_z / vld_M
    mae_z = np.mean(np.abs(rn_z))

    # Calculate Metrics in Y space (Original game points scale)
    # Transform predictions back to Y space
    y_pred = ctx.z2y(z_pred)
    rn_y = y_pred - vld_game_values
    mae_y = np.mean(np.abs(rn_y))
    mse_y = np.dot(rn_y, rn_y) / vld_M
    rmse_y = np.sqrt(mse_y)

    return {
        'vld_loss': float(vld_loss),
        'vld_mse_z': float(mse_z),
        'vld_mae_z': float(mae_z),
        'vld_mae_y': float(mae_y),
        'vld_rmse_y': float(rmse_y),
        'vld_samples': int(vld_M)
    }
