#
# lm_worker_wmaps.py
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

# twolm/lm_worker_wmaps.py
from __future__ import annotations

from typing import TYPE_CHECKING

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.rlm_wmaps import (rlm_wmaps_load_from_file,
                             rlm_wmaps_store_to_file,
                             rlm_wmaps_compute,
                             rlm_wmaps_is_cache_consistent)
from twolm.cache_manager import cache_manager_load_or_compute

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_wmaps']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Computing weight maps (WMAPS) for the feature set...")

    cache_hit, wmaps_obj = cache_manager_load_or_compute(
        cache_path  = ctx.get_cache_file_full_path_for_next_level(),
        is_allowed  = ctx.use_cache,
        load_fn     = rlm_wmaps_load_from_file,
        store_fn    = rlm_wmaps_store_to_file,
        validate_fn = lambda cached_wmaps: rlm_wmaps_is_cache_consistent(ctx, cached_wmaps),
        compute_fn  = lambda: rlm_wmaps_compute(ctx),
        logger_fn   = ctx.log_event
    )
    
    # Populate the 6 separate attributes in the context
    ctx.feature_w_ranges = wmaps_obj.w_ranges
    ctx.iwmap_feature_offset = wmaps_obj.iwmap_feature_offset
    ctx.iwmap = wmaps_obj.iwmap
    ctx.wmap = wmaps_obj.wmap
    ctx.wmap_fallback = wmaps_obj.wmap_fallback
    ctx.w = wmaps_obj.w
    
    ctx.log_event(Relevance.INFO, f"WMAPS computed/loaded. Total weights (W): {len(ctx.w)}")
        
def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing WMAPS and W attributes...")
    ctx.feature_w_ranges = None
    ctx.iwmap_feature_offset = None
    ctx.iwmap = None
    ctx.wmap = None
    ctx.wmap_fallback = None
    ctx.w = None


def lm_worker_wmaps() -> Worker:
    """Factory function that returns the WMAPS worker instance."""
    return Worker("WMAPS", _up, _down)
