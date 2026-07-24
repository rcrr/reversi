#
# rlm_gradient.py
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

# twolm/rlm_gradient.py
from __future__ import annotations

from typing import TYPE_CHECKING, Tuple, Callable
import numpy as np
import numpy.typing as npt

from scipy.special import expit

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['rlm_gradient_compute', 'sigmoid']


def sigmoid(x: np.ndarray) -> np.ndarray:
    """
    Computes the sigmoid (logistic) function on the given array of float values.
    It was computed as:
      return 1. / (1. + np.exp(-x))
    The expit funxtion from scipy provide more numerical stability for very small values of x.
    """
    return expit(x)


def rlm_gradient_compute(ctx: "RLMContext") -> Callable[[npt.NDArray[np.float32]], Tuple[np.float32, npt.NDArray[np.float32]]]:
    """
    Builds the function returning loss and gradient.
    Captures X, z, and alpha in a closure for fast repeated evaluations during optimization.
    The loss and gradient are normalized by the number of positions (M) to ensure
    dataset-size independent convergence criteria.
    """
    alpha = ctx.cfg.stat_model.ridge_regularization
    N = len(ctx.w)
    X = ctx.design_matrix
    M, P = X.shape  # M is the number of positions
    X_flat = X.ravel()
    z = ctx.z
    
    # Pre-allocate buffers for performance inside the closure
    bincount_weights_buffer = np.empty(M * P, dtype=np.float32)
    bincount_weights_view = bincount_weights_buffer.reshape(M, P)
    
    def fg(w: npt.NDArray[np.float32]) -> Tuple[np.float32, npt.NDArray[np.float32]]:
        linear_predictor = np.sum(w[X], axis=1)
        zh = sigmoid(linear_predictor)
        dzh = zh * (1. - zh)
        rn = zh - z
        
        # Normalized loss (Mean Squared Error + Ridge regularization)
        norm_rn = np.dot(rn, rn)
        norm_w = np.dot(w, w)
        f = 0.5 * ((norm_rn / M) + alpha * norm_w)
        
        # Normalized gradient
        dzh_rn = dzh * rn
        bincount_weights_view[:] = dzh_rn[:, None]
        g0 = np.bincount(X_flat, weights=bincount_weights_buffer, minlength=N) / M
        g1 = alpha * w
        g = g0 + g1
        
        return f, g
    
    return fg
