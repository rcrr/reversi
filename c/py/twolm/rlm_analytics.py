#
# rlm_analytics.py
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

# twolm/rlm_analytics.py
from __future__ import annotations

from typing import TYPE_CHECKING, Dict, Optional
import numpy as np
from scipy.special import expit

from twolm.rlm_gradient import sigmoid
from twolm.enums import Relevance

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['compute_training_metrics', 'format_analytics_report']


def compute_training_metrics(ctx: "RLMContext") -> Dict[str, float]:
    """
    Computes MAE(y) and RMSE(y) on the training set using the optimized weights (w).
    """
    ctx.log_event(Relevance.INFO, "Computing metrics on the training set...")
    
    # We use the compressed weights (ctx.w) and the training design matrix (ctx.design_matrix)
    # This is very fast as the data is already in memory.
    linear_predictor = np.sum(ctx.w[ctx.design_matrix].astype(np.float64), axis=1)
    z_pred = expit(linear_predictor)
    
    # Transform back to Y space
    y_pred = ctx.z2y(z_pred)
    y_true = ctx.game_values.astype(np.float64)
    
    rn_y = y_pred - y_true
    mae_y = np.mean(np.abs(rn_y))
    rmse_y = np.sqrt(np.dot(rn_y, rn_y) / len(rn_y))
    
    return {
        'train_mae_y': float(mae_y),
        'train_rmse_y': float(rmse_y),
        'train_loss': float(ctx.opt_info['f']) if ctx.opt_info else 0.0
    }

def format_analytics_report(ctx: "RLMContext", train_metrics: Dict[str, float], val_metrics: Optional[Dict[str, float]]) -> str:
    """
    Formats the final model summary into a readable ASCII table.
    """
    model_name = ctx.cfg.name
    total_params = len(ctx.w)
    dense_params = len(ctx.w_dense) if ctx.w_dense is not None else 0
    
    opt_info = ctx.opt_info or {}
    # If opt_info is empty, it likely means the model was loaded from a pre-existing state.
    opt_status = opt_info.get('reason', 'Loaded from disk / N/A')
    opt_iters = opt_info.get('iters', 0)
    
    train_rmse = train_metrics.get('train_rmse_y', 0.0)
    train_mae = train_metrics.get('train_mae_y', 0.0)
    train_loss = train_metrics.get('train_loss', 0.0)
    
    if val_metrics:
        val_rmse = val_metrics.get('vld_rmse_y', 0.0)
        val_mae = val_metrics.get('vld_mae_y', 0.0)
        val_loss = val_metrics.get('vld_loss', 0.0)
        gen_gap = val_rmse - train_rmse
        
        # Format validation metrics as individual strings for proper column alignment
        val_rmse_str = f"{val_rmse:.2f}"
        val_mae_str = f"{val_mae:.2f}"
        val_loss_str = f"{val_loss:.4e}"
    else:
        val_rmse_str = val_mae_str = val_loss_str = "N/A"
        gen_gap = 0.0

    report = (
        f"\n{'=' * 120}\n"
        f"MODEL ANALYTICS REPORT: {model_name}\n"
        f"{'=' * 120}\n"
        f"  Total Parameters (W)  : {total_params:,}\n"
        f"  Dense Parameters (K)  : {dense_params:,}\n"
        f"  Optimization Status   : {opt_status} ({opt_iters} iters)\n"
        f"{'-' * 120}\n"
        f"  {'METRIC':<15} | {'TRAINING':<14} | {'VALIDATION':<14}\n"
        f"{'-' * 120}\n"
        f"  {'RMSE (y)':<15} | {train_rmse:<14.2f} | {val_rmse_str:<14}\n"
        f"  {'MAE (y)':<15} | {train_mae:<14.2f} | {val_mae_str:<14}\n"
        f"  {'Loss (MSE/2)':<15} | {train_loss:<14.4e} | {val_loss_str:<14}\n"
        f"{'-' * 120}\n"
        f"  Generalization Gap (RMSE) : {gen_gap:.2f}\n"
        f"{'=' * 120}\n"
    )
    return report

