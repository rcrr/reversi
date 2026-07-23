#
# lm_worker_zed.py
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

# twolm/lm_worker_zed.py
from __future__ import annotations

from typing import TYPE_CHECKING

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.rlm_zed import rlm_zed_compute

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_zed']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Computing Zed array (Z) and transformations...")
    
    # ZED computation is very fast (vectorized math), so we compute it on the fly without disk caching.
    zed_obj = rlm_zed_compute(ctx)
    
    ctx.y2z = zed_obj.y2z
    ctx.z2y = zed_obj.z2y
    ctx.z = zed_obj.z
    
    ctx.log_event(Relevance.INFO, f"Zed array computed. Shape: {ctx.z.shape}")
        
def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing Zed attributes...")
    ctx.y2z = None
    ctx.z2y = None
    ctx.z = None


def lm_worker_zed() -> Worker:
    """Factory function that returns the ZED worker instance."""
    return Worker("ZED", _up, _down)
