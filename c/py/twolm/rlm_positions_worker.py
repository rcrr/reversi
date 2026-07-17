#
# rlm_positions_worker.py
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

#
# Reversi Logistic Model Positions Worker
#
# To properly access the database server.
#
# ssh -N -f -L 5432:localhost:5432 username@database.server.local
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.rlm_abstract_worker import ReversiLogisticModelWorker
from twolm.enums import Relevance

from twolm.regab import (
    RegabDataSet, 
    RegabDBConnection, 
    regab_extract_data_set_from_db,
    regab_load_data_set_from_file, 
    regab_store_data_set_to_file
)

from twolm.cache_manager import cache_manager_load_or_compute



__all__ = ['RLMPositionsWorker']



class RLMPositionsWorker(ReversiLogisticModelWorker):

    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(Relevance.INFO, "Loading game positions...")

        # Execute the abstracted pipeline.
        cache_hit, rds = cache_manager_load_or_compute(
            cache_path  = model.get_cache_file_full_path_for_next_level(),
            is_allowed  = model.use_cache,
            load_fn     = regab_load_data_set_from_file,
            store_fn    = regab_store_data_set_to_file,
            validate_fn = lambda cached_rds: _is_cache_consistent(model, cached_rds),
            compute_fn  = lambda: _load_from_db(model),
            logger_fn   = model.log_event
        )

        # If we recomputed at this level, all subsequent levels must recompute too.
        if not cache_hit:
            model.log_event(Relevance.INFO, "Cache invalidated. Forcing computation for all subsequent pipeline steps.")
            model.use_cache = False

        # Post-processing (unrelated to caching logic).
        positions, game_values = rds.generate_positions_and_game_values()
        model.positions = positions
        model.game_values = game_values
        
        model.log_event(Relevance.INFO, "Model attributes positions and game_values have been set.")
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(Relevance.INFO, "Clearing game positions and game_values attributes.")
        model.positions = None
        model.game_values = None


# ----------------------------------------------------------------------------------------
# Private Helper Functions
# ----------------------------------------------------------------------------------------

def _is_cache_consistent(model: ReversiLogisticModel, rds: RegabDataSet) -> bool:
    """Compare live configuration with cached dataset metadata."""
    
    # 1. Check DB Connection parameters
    cfg_conn = model.cfg.regab_data_set.regab_db_connection
    cache_conn = rds.rc
    
    cfg_conn_data = (cfg_conn.dbname, cfg_conn.user, cfg_conn.host, cfg_conn.port)
    cache_conn_data = (cache_conn.dbname, cache_conn.user, cache_conn.host, int(cache_conn.port))
    
    is_conn_consistent = cfg_conn_data == cache_conn_data

    # 2. Check DB Query parameters
    cfg_query = model.cfg.regab_data_set
    cache_query = rds
    
    cfg_query_data = (cfg_query.bid, cfg_query.status, cfg_query.ec)
    cache_query_data = (cache_query.bid, cache_query.status, cache_query.ec)
    
    is_query_consistent = cfg_query_data == cache_query_data

    is_cache_consistent = is_conn_consistent and is_query_consistent
    model.log_event(Relevance.DEBUG, f"is_cache_consistent = {is_cache_consistent}.")

    # Log details only if inconsistencies are found (avoids building strings for no reason)
    if not is_conn_consistent:
        msg_conn = (
            f"DB Connection mismatch:\n"
            f"  cfg.dbname   = {repr(cfg_conn.dbname)}, cache.dbname = {repr(cache_conn.dbname)}\n"
            f"  cfg.user     = {repr(cfg_conn.user)}, cache.user = {repr(cache_conn.user)}\n"
            f"  cfg.host     = {repr(cfg_conn.host)}, cache.host = {repr(cache_conn.host)}\n"
            f"  cfg.port     = {repr(cfg_conn.port)}, cache.port = {repr(cache_conn.port)}"
        )
        model.log_event(Relevance.DEBUG, msg_conn)
        
    if not is_query_consistent:
        msg_query = (
            f"DB Query mismatch:\n"
            f"  cfg.bid    = {repr(cfg_query.bid)}, cache.bid = {repr(cache_query.bid)}\n"
            f"  cfg.status = {repr(cfg_query.status)}, cache.status = {repr(cache_query.status)}\n"
            f"  cfg.ec     = {repr(cfg_query.ec)}, cache.ec = {repr(cache_query.ec)}"
        )
        model.log_event(Relevance.DEBUG, msg_query)

    return is_cache_consistent


def _load_from_db(model: ReversiLogisticModel) -> RegabDataSet:
    """Extract dataset from the database safely."""
    conn_params = model.cfg.regab_data_set.regab_db_connection
    rc = RegabDBConnection(conn_params.dbname, conn_params.user, conn_params.host)
    
    model.log_event(Relevance.DEBUG, f"Regab Database connection {(conn_params.dbname, conn_params.user, conn_params.host)} established successfully.")

    try:
        query_params = model.cfg.regab_data_set
        rds = regab_extract_data_set_from_db(rc, query_params.bid, query_params.status, query_params.ec)
        model.log_event(Relevance.DEBUG, f"Extracted {len(rds.df_mogv):,} positions from the database.")
        return rds
    finally:
        rc.close()
        model.log_event(Relevance.DEBUG, "Regab Database connection closed successfully.")

