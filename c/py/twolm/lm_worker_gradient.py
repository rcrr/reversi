#
# lm_worker_gradient.py
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

# twolm/lm_worker_gradient.py
from __future__ import annotations

from typing import TYPE_CHECKING

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.rlm_gradient import rlm_gradient_compute

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['lm_worker_gradient']


def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Building gradient and loss function (fg)...")
    
    # GRADIENT building is a closure generation, very fast. No cache needed.
    ctx.fg = rlm_gradient_compute(ctx)
    
    ctx.log_event(Relevance.INFO, "Gradient function generated.")
        
def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing gradient function...")
    ctx.fg = None


def lm_worker_gradient() -> Worker:
    """Factory function that returns the GRADIENT worker instance."""
    return Worker("GRADIENT", _up, _down)
