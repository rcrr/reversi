#
# lm_worker_vld_positions.py
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

# twolm/lm_worker_vld_positions.py
from __future__ import annotations

import numpy as np

from typing import TYPE_CHECKING
from pathlib import Path

from twolm.state_machine import Worker, Relevance
from twolm.regab import (
    RegabDataSet, 
    RegabDBConnection, 
    regab_extract_data_set_from_db,
    regab_load_data_set_from_file, 
    regab_store_data_set_to_file
)
from twolm.cache_manager import cache_manager_load_or_compute
from twolm.lm_worker_positions import load_from_db, is_cache_consistent

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_vld_positions']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Loading validation game positions...")

    vld_cfg = ctx.cfg.validation_data_set

    # Execute the abstracted pipeline.
    cache_hit, vld_rds, vld_rds_checksum = cache_manager_load_or_compute(
        cache_path  = ctx.get_cache_file_full_path_for_next_level(),
        is_allowed  = ctx.use_cache,
        load_fn     = regab_load_data_set_from_file,
        store_fn    = regab_store_data_set_to_file,
        validate_fn = lambda cached_vld_rds: is_cache_consistent(ctx, cached_vld_rds, bid=vld_cfg.bid, status=vld_cfg.status),
        compute_fn  = lambda: load_from_db(ctx, bid=vld_cfg.bid, status=vld_cfg.status),
        logger_fn   = ctx.log_event
    )

    # Post-processing (unrelated to caching logic).
    vld_positions, vld_game_values = vld_rds.generate_positions_and_game_values()
    ctx.vld_positions = vld_positions
    ctx.vld_game_values = vld_game_values
    ctx.vld_rds_checksum = vld_rds_checksum
    
    ctx.log_event(Relevance.INFO, f"Regab validation data set object checksum (vld_rds_checksum): {vld_rds_checksum}.")
    ctx.log_event(Relevance.INFO, "Model attributes vld_positions, vld_game_values and vld_rds_checksum have been set.")

    count = len(vld_game_values)
    mean = np.mean(vld_game_values)
    std_dev = np.std(vld_game_values)
    variance = np.var(vld_game_values)
    ctx.log_event(Relevance.INFO, f"Statistical properties of population for validation: [COUNT: {count:,}], [MEAN: {mean:.2f}], [STD: {std_dev:.2f}], [VARIANCE: {variance:.2f}].")

    ctx.vld_pop_stats = {
        'count': count,
        'mean': mean,
        'std_dev': std_dev,
        'variance': variance
    }
    
    return

        
def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing validation game positions vld_positions, vld_game_values and vld_rds_checksum attributes.")
    ctx.vld_positions = None
    ctx.vld_game_values = None
    ctx.vld_rds_checksum = None
    ctx.vld_pop_stats = None

    return


def lm_worker_vld_positions() -> Worker:
    """Factory function that returns the VLD_POSITIONS worker instance."""
    return Worker("VLD_POSITIONS", _up, _down)
