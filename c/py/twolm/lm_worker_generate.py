#
# lm_worker_generate.py
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

# twolm/lm_worker_generate.py
from __future__ import annotations

from typing import TYPE_CHECKING

from twolm.state_machine import Worker, Relevance
from twolm.rlm_generate import (compute_w_dense,
                                rlm_generate_store_to_file,
                                rlm_generate_load_from_file,
                                rlm_generate_is_cache_consistent)
from twolm.cache_manager import cache_manager_load_or_compute

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_generate']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Generating dense weight vector (w_dense) for inference...")

    cache_hit, dense_obj, dense_obj_checksum = cache_manager_load_or_compute(
        cache_path  = ctx.get_cache_file_full_path_for_next_level(),
        is_allowed  = ctx.use_cache,
        load_fn     = rlm_generate_load_from_file,
        store_fn    = rlm_generate_store_to_file,
        validate_fn = lambda cached_dw: rlm_generate_is_cache_consistent(ctx, cached_dw),
        compute_fn  = lambda: compute_w_dense(ctx),
        logger_fn   = ctx.log_event
    )
    
    ctx.w_dense = dense_obj.w_dense
    ctx.log_event(Relevance.INFO, f"Dense weight vector generated/loaded. Shape: {ctx.w_dense.shape}")

def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing dense weight vector...")
    ctx.w_dense = None

def lm_worker_generate() -> Worker:
    return Worker("GENERATE", _up, _down)
