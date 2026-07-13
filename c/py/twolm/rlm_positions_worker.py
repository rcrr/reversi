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

from twolm.rlm_abstract_worker import  ReversiLogisticModelWorker

from twolm.types import *
from twolm.board import *
from twolm.regab import *
from twolm.binio import *

from twolm.cache_manager import *



__all__ = ['RLMPositionsWorker']




class RLMPositionsWorker(ReversiLogisticModelWorker):

    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(Relevance.INFO, "Loading game positions...")

        cache_file_path = model.get_cache_file_full_path_for_next_level()

        logger = model.log_event

        # Step 0: Is cache allowed? (Example logic)
        is_cache_allowed = model.cfg.use_cache 

        # Define the injected strategies (Business Logic)
        def compute() -> RegabDataSet:
            return _load_from_db(model)

        def validate(cached_rds: RegabDataSet) -> bool:
            return _is_cache_consistent(model, cached_rds)

        # Execute the abstracted pipeline
        rds = cache_manager_load_or_compute(
            cache_path=cache_file_path,
            is_allowed=is_cache_allowed,
            load_fn=regab_load_data_set_from_file,
            store_fn=regab_store_data_set_to_file,
            validate_fn=validate,
            compute_fn=compute,
            logger_fn=logger
        )

        # Post-processing (unrelated to caching logic)
        positions, game_values = rds.generate_positions_and_game_values()
        model.positions = positions
        model.game_values = game_values
        
        model.log_event(Relevance.INFO, f"Model attributes positions and game_values have been set.")
        return
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(Relevance.INFO, f"Clearing game positions, setting model attribute rds to None.")
        model.rds = None
        return

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

def _is_cache_available(p: Path) -> bool:
    if p.exists():
        return verify_sha3_256_sidecar(p)
    return False

def _is_cache_consistent(model: ReversiLogisticModel, rds: RegabDataSet) -> bool:
    m = model.cfg.regab_data_set.regab_db_connection
    c = rds.rc
    cfg_data_c   = (m.dbname, m.user, m.host, m.port)
    cache_data_c = (c.dbname, c.user, c.host, int(c.port))
    is_db_conn_consistent = cfg_data_c == cache_data_c
    msg_conn = (f"is_db_conn_consistent = {is_db_conn_consistent}\n"
                f"  cfg.dbname = {repr(m.dbname)}, cache.dbname = {repr(c.dbname)}\n"
                f"  cfg.user = {repr(m.user)}, cache.user = {repr(c.user)}\n"
                f"  cfg.host = {repr(m.host)}, cache.host = {repr(c.host)}\n"
                f"  cfg.port = {repr(m.port)}, cache.port = {repr(c.port)}\n")
    m = model.cfg.regab_data_set
    c = rds
    cfg_data_q   = (m.bid, m.status, m.ec)
    cache_data_q = (c.bid, c.status, c.ec)
    is_db_query_consistent = cfg_data_q == cache_data_q
    msg_query = (f"is_db_query_consistent = {is_db_query_consistent}"
                 f"  cfg.bid = {repr(m.bid)}, cache.bid = {repr(c.bid)}"
                 f"  cfg.status = {repr(m.status)}, cache.status = {repr(c.status)}"
                 f"  cfg.ec = {repr(m.ec)}, cache.ec = {repr(c.ec)}")
    is_cache_consistent = is_db_conn_consistent and is_db_query_consistent
    model.log_event(Relevance.DEBUG, f"is_cache_consistent = {is_cache_consistent}.")
    if not is_db_conn_consistent:
        model.log_event(Relevance.DEBUG, msg_conn)
    if not is_db_query_consistent:
        model.log_event(Relevance.DEBUG, msg_query)
    return is_cache_consistent

def _load_from_db(model: ReversiLogisticModel) -> RegabDataSet:
    cp = model.cfg.regab_data_set.regab_db_connection
    rc = RegabDBConnection(cp.dbname, cp.user, cp.host)
    model.log_event(Relevance.DEBUG, f"Regab Database connection {cp.dbname, cp.user, cp.host} established succesfully.")

    cp = model.cfg.regab_data_set
    rds = regab_extract_data_set_fron_db(rc, cp.bid, cp.status, cp.ec)
    model.log_event(Relevance.DEBUG, f"Extracted {len(rds.df_mogv):,} positions from the database.")
    
    rc.close()
    model.log_event(Relevance.DEBUG, f"Regab Database connection closed succesfully.")

    return rds


