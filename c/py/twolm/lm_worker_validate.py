#
# lm_worker_validate.py
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

# twolm/lm_worker_validate.py
from __future__ import annotations

import numpy as np

from typing import TYPE_CHECKING

from twolm.state_machine import Worker, Relevance
from twolm.rlm_validate import validate_model

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_validate']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Starting model validation...")
    
    metrics = validate_model(ctx)    
    ctx.vld_metrics = metrics
    
    ctx.log_event(Relevance.INFO, f"Validation completed. Loss (MSE/2): {metrics['vld_loss']:.8e}")
    ctx.log_event(Relevance.INFO, f"  MSE:      {metrics['vld_mse_z']:.4e}")
    ctx.log_event(Relevance.INFO, f"  MAE:      {metrics['vld_mae_z']:.4e}")
    ctx.log_event(Relevance.INFO, f"  MAE(y):   {metrics['vld_mae_y']:.2f}")
    ctx.log_event(Relevance.INFO, f"  RMSE(y):  {metrics['vld_rmse_y']:.2f}")
    ctx.log_event(Relevance.INFO, f"  RMSE/MAE: {(metrics['vld_rmse_y']/metrics['vld_mae_y']):.2f}")

def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing validation attributes...")
    ctx.vld_metrics = None


def lm_worker_validate() -> Worker:
    """Factory function that returns the VALIDATE worker instance."""
    return Worker("VALIDATE", _up, _down)
