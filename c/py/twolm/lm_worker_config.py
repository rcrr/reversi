#
# lm_worker_positions.py
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


# twolm/lm_worker_config.py
from __future__ import annotations

from typing import TYPE_CHECKING, Callable
from pathlib import Path
import os
import json5
import hashlib

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.rlm_config_schemas import ReversiLogisticModelConfig

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['lm_worker_config']


def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, f"Loading configuration file '{ctx.config_file_path}' ...")
    cfg = _load(ctx)
    ctx.log_event(Relevance.INFO, f"Configuration file {ctx.config_file_path} loaded.")
    _validate(ctx, cfg)
    ctx.log_event(Relevance.INFO, f"Configuration file {ctx.config_file_path} validated.")
    _store_to_file(ctx, cfg)
    ctx.log_event(Relevance.INFO, f"Model name: {cfg.name}")
    ctx.log_event(Relevance.INFO, f"Model description: {cfg.description}")
    ctx.log_event(Relevance.INFO, f"Model base_dir: {cfg.base_dir}")
    ctx.use_cache = cfg.use_cache
    ctx.cfg = cfg

def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing configuration...")
    ctx.cfg = None
    ctx.use_cache = None

def _load(ctx: "RLMContext") -> ReversiLogisticModelConfig:
    with ctx.config_file_path.open("r", encoding="utf-8") as f:
        config_raw_data = json5.load(f)
        ctx.log_event(Relevance.DEBUG, f"JSON file '{f}' read.")

    if ctx.base_dir_override is not None:
        config_raw_data['base_dir'] = ctx.base_dir_override
        ctx.log_event(Relevance.DEBUG, f"Value for base_dir changed to '{ctx.base_dir_override}'.")

    cfg = ReversiLogisticModelConfig(**config_raw_data)
    return cfg

def _validate(ctx: "RLMContext", cfg: ReversiLogisticModelConfig) -> None:
    _validate_base_dir(ctx, cfg)

def _validate_base_dir(ctx: "RLMContext", cfg: ReversiLogisticModelConfig) -> None:
    if not cfg.base_dir.exists():
        error_message = f"The base dir: '{cfg.base_dir}' doesn't exist."
        ctx.log_event(Relevance.ERROR, error_message)
        raise RuntimeError(error_message)
    if not cfg.base_dir.is_dir():
        error_message = f"The base dir: '{cfg.base_dir}' is not a directory."
        ctx.log_event(Relevance.ERROR, error_message)
        raise RuntimeError(error_message)    
    is_writable = os.access(cfg.base_dir, os.W_OK)
    if not is_writable:
        error_message = f"The base dir: '{cfg.base_dir}' is not writable."
        ctx.log_event(Relevance.ERROR, error_message)
        raise RuntimeError(error_message)    

def _store_to_file(ctx: "RLMContext", cfg: ReversiLogisticModelConfig) -> None:
    config_json = cfg.model_dump_json(indent=4)
    current_checksum = hashlib.sha3_256(config_json.encode()).hexdigest()
    
    cache_file_path = cfg.base_dir / ctx.get_cache_file_path_for_next_level()
    checksum_file_path = cache_file_path.with_name(cache_file_path.name + ".SHA3-256")
    
    ctx.log_event(Relevance.DEBUG, f"File: {cache_file_path}, current_checksum = {current_checksum}")
    
    has_to_write = False
    
    if not cache_file_path.exists():
        has_to_write = True
    else:
        if not checksum_file_path.exists():
            msg = f"The checksum file {checksum_file_path} is missing."
            ctx.log_event(Relevance.ERROR, msg)
            raise RuntimeError(msg)
        with open(checksum_file_path, 'r') as checksum_file:
            stored_checksum = checksum_file.read().strip()
        ctx.log_event(Relevance.DEBUG, f"File: {cache_file_path}, stored_checksum = {stored_checksum}")

        on_disk_recomputed_sha3_256_hash = hashlib.sha3_256()    
        with open(cache_file_path, 'rb') as f:
            while chunk := f.read(8192):
                on_disk_recomputed_sha3_256_hash.update(chunk)
        on_disk_recomputed_checksum = on_disk_recomputed_sha3_256_hash.hexdigest()
        ctx.log_event(Relevance.DEBUG, f"File: {cache_file_path}, on_disk_recomputed_checksum = {on_disk_recomputed_checksum}")
        
        if on_disk_recomputed_checksum != stored_checksum:
            msg = f"The checksum file on disk {checksum_file_path} is not matching."
            ctx.log_event(Relevance.ERROR, msg)
            raise RuntimeError(f"The checksum file on disk {checksum_file_path} is not matching.")    

        if current_checksum != stored_checksum:
            has_to_write = True

    if has_to_write:
        cache_file_path.write_text(config_json, encoding="utf-8")
        with open(checksum_file_path, 'w') as checksum_file:
            checksum_file.write(current_checksum)
        ctx.log_event(Relevance.INFO, f"File '{cache_file_path}' and checksum written to disk.")
    else:
        ctx.log_event(Relevance.INFO, f"Existing file '{cache_file_path}' and checksum are up to date.")


def lm_worker_config() -> Worker:
    """Factory function that returns the CONFIG worker instance."""
    return Worker("CONFIG", _up, _down)
