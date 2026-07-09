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

from twolm.board import *
from twolm.regab import *
from twolm.binio import *



__all__ = ['RLMPositionsWorker']



class RLMPositionsWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Loading game positions...")

        rds: RegabDataSet = None
        
        cache_file_path = model.cfg.base_dir / model.get_cache_file_path_for_next_level()
        ca = _is_cache_available(cache_file_path)
        model.log_event(model.Relevance.INFO, f"Cache file path: '{cache_file_path}', exists={ca}.")
        
        if ca:
            rds = regab_load_data_set_from_file(cache_file_path)
            cc = _is_cache_consistent(model, rds)
            model.log_event(model.Relevance.INFO, f"Cache file loaded: '{cache_file_path}', is_cache_consistent={cc}.")
            if not cc:
                rds = None
                checksum_file_path = cache_file_path.with_suffix(cache_file_path.suffix + ".SHA3-256")
                cache_file_path.unlink(missing_ok=True)
                model.log_event(model.Relevance.INFO, f"Cache file {cache_file_path} deleted.")
                checksum_file_path.unlink(missing_ok=True)
                model.log_event(model.Relevance.INFO, f"Cache file checksum {checksum_file_path} deleted.")

        if not rds:
            model.log_event(model.Relevance.INFO, f"Loading data from database...")
            rds = _load_from_db(model)
            model.log_event(model.Relevance.INFO, f"Game positions loaded. Count: {len(rds.df_mogv):,}")
            regab_store_data_set_to_file(rds, cache_file_path)
            model.log_event(model.Relevance.INFO, f"Cache file {cache_file_path} written.")

            positions, game_values = rds.generate_positions_and_game_values()
            model.positions = positions
            model.log_event(model.Relevance.INFO, f"Model attribute positions has been set.")
            model.game_values = game_values
            model.log_event(model.Relevance.INFO, f"Model attribute game_values has been set.")
        return
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, f"Clearing game positions, setting model attribute rds to None.")
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
    model.log_event(model.Relevance.DEBUG, f"is_cache_consistent = {is_cache_consistent}.")
    if not is_db_conn_consistent:
        model.log_event(model.Relevance.DEBUG, msg_conn)
    if not is_db_query_consistent:
        model.log_event(model.Relevance.DEBUG, msg_query)
    return is_cache_consistent

def _load_from_db(model: ReversiLogisticModel) -> RegabDataSet:
    cp = model.cfg.regab_data_set.regab_db_connection
    rc = RegabDBConnection(cp.dbname, cp.user, cp.host)
    model.log_event(model.Relevance.DEBUG, f"Regab Database connection {cp.dbname, cp.user, cp.host} established succesfully.")

    cp = model.cfg.regab_data_set
    rds = regab_extract_data_set_fron_db(rc, cp.bid, cp.status, cp.ec)
    model.log_event(model.Relevance.DEBUG, f"Extracted {len(rds.df_mogv):,} positions from the database.")
    
    rc.close()
    model.log_event(model.Relevance.DEBUG, f"Regab Database connection closed succesfully.")

    return rds


