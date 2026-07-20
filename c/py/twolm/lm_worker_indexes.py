#
# lm_worker_indexes.py
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

# twolm/lm_worker_indexes.py
from __future__ import annotations

from typing import TYPE_CHECKING

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.rlm_indexes import (ReversiLogisticModelIndexes,
                               rlm_indexes_load_from_file,
                               rlm_indexes_store_to_file,
                               rlm_indexes_compute,
                               rlm_indexes_is_cache_consistent)
from twolm.cache_manager import cache_manager_load_or_compute

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['lm_worker_indexes']


def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Computing indexes for the feature set...")

    # Execute the abstracted pipeline.
    cache_hit, rlm_indexes = cache_manager_load_or_compute(
        cache_path  = ctx.get_cache_file_full_path_for_next_level(),
        is_allowed  = ctx.use_cache,
        load_fn     = rlm_indexes_load_from_file,
        store_fn    = rlm_indexes_store_to_file,
        validate_fn = lambda cached_rlm_indexes: rlm_indexes_is_cache_consistent(ctx, cached_rlm_indexes),
        compute_fn  = lambda: rlm_indexes_compute(ctx),
        logger_fn   = ctx.log_event
    )
    
    ctx.log_event(Relevance.INFO, "Model attribute rlm_indexes has been set.")
    ctx.rlm_indexes = rlm_indexes
        
def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing rlm_indexes...")
    ctx.rlm_indexes = None


def lm_worker_indexes() -> Worker:
    """Factory function that returns the INDEXES worker instance."""
    return Worker("INDEXES", _up, _down)
