#
# rlm_zed.py
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

# twolm/rlm_zed.py
from __future__ import annotations

from typing import TYPE_CHECKING, Tuple, Callable
import numpy as np
import numpy.typing as npt

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['ReversiLogisticModelZed',
           'rlm_zed_compute',
           'zed_fun_factory']


class ReversiLogisticModelZed:
    """Holds the Z vector and the transformation functions y2z and z2y."""
    def __init__(self, y2z: Callable, z2y: Callable, z: np.ndarray):
        self.y2z = y2z
        self.z2y = z2y
        self.z = z


def zed_fun_factory(logit_clipping: float) -> Tuple[Callable[[npt.NDArray[np.int8]], npt.NDArray[np.float32]],
                                                     Callable[[npt.NDArray[np.float32]], npt.NDArray[np.float32]]]:
    """
    Returns two functions that perform an affine transformation.
    One on int8 arrays, it maps the range [-64, 64] to [logit_clipping, 1 - logit_clipping].
    The second does the inverted transformation.
    """
    a = np.float32(0.5)
    b = np.float32((1. - 2. * logit_clipping) / 128.)
    def y_to_z(y: npt.NDArray[np.int8]) -> npt.NDArray[np.float32]:
        y = np.float32(y)
        z = a + y * b
        return z
    
    a1 = np.float32(-64. / (1. - 2. * logit_clipping))
    b1 = np.float32(128. / (1. - 2. * logit_clipping))
    def z_to_y(z: npt.NDArray[np.float32]) -> npt.NDArray[np.float32]:
        y = a1 + z * b1
        return y
    
    return y_to_z, z_to_y


def rlm_zed_compute(ctx: "RLMContext") -> ReversiLogisticModelZed:
    """Computes Z vector and transformation functions based on logit_clipping."""
    alpha = ctx.cfg.stat_model.logit_clipping
    y2z, z2y = zed_fun_factory(alpha)
    
    # Ensure game_values is a numpy array of int8
    y = np.asarray(ctx.game_values, dtype=np.int8)
    z = y2z(y)
    
    return ReversiLogisticModelZed(y2z=y2z, z2y=z2y, z=z)
