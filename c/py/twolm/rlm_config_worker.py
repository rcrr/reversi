#
# rlm_config_worker.py
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
# Reversi Logistic Model Config Worker
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.rlm_abstract_worker import ReversiLogisticModelWorker

from twolm.domain import *

from pydantic import (BaseModel, Field, NonNegativeInt, field_validator, field_serializer,
                      ConfigDict)

from typing import List, Annotated, Optional

import os
import json5
import hashlib
from pathlib import Path


__all__ = ['RLMConfigWorker']

#
# Pydantic configuration classes.
#

class RegabDBConnectionConfig(BaseModel):
    """
    Configuration for connecting to the Regab database.
    """
    dbname: str
    user: str
    host: str
    port: int = 5432
    password: Optional[str] = None

StatusString = Annotated[str, Field(pattern=r"^[A-Z]{3}$")]

class RegabDataSetConfig(BaseModel):
    """
    Configuration for a Regab data set, including the database connection and filtering criteria.
    """
    regab_db_connection: RegabDBConnectionConfig
    bid: List[NonNegativeInt]
    status: List[StatusString]
    ec: int = Field(..., ge=0, le=60)

class PatternConfig(BaseModel):
    """
    Configuration for a pattern, including its name and mask.
    """
    name: str
    mask: SquareSet = Field(..., description="Pattern mask in HEX format, no 0x prefix, just 16 digits.")
 
    model_config = ConfigDict(arbitrary_types_allowed=True)

    @field_validator("mask", mode="before")
    @classmethod
    def parse_hex_to_square_set(cls, h: str) -> SquareSet:
        """
        Converts a hex string (e.g., '0x0000000000000107') into a SquareSet.
        """
        return SquareSet.new_from_hex(h)
 
    @field_serializer("mask")
    def serialize_mask_to_hex(self, mask: SquareSet) -> str:
        return f"{mask:016X}"

class PatternSetConfig(BaseModel):
    """
    Configuration for a set of patterns, including the name and a list of individual pattern configurations.
    """
    name: str
    patterns: List[PatternConfig] = []

class ReversiLogisticModelConfig(BaseModel):
    """
    Configuration for the Reversi Logistic Model, including general settings,
    data set configuration.
    """
    name: str
    description: str
    base_dir: Path
    regab_data_set: RegabDataSetConfig
    pattern_set: PatternSetConfig

#
# Pydantic - End.
#

class RLMConfigWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, f"Loading configuration file '{model.config_file_path}' ...")
        cfg = _load(model)
        model.log_event(model.Relevance.INFO, f"Configuration file {model.config_file_path} loaded.")
        _validate(model, cfg)
        model.log_event(model.Relevance.INFO, f"Configuration file {model.config_file_path} validated.")
        _store_to_file(model, cfg)
        model.log_event(model.Relevance.INFO, f"Model name: {cfg.name}")
        model.log_event(model.Relevance.INFO, f"Model description: {cfg.description}")
        model.log_event(model.Relevance.INFO, f"Model base_dir: {cfg.base_dir}")
        model.cfg = cfg
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Clearing configuration...")
        model.cfg = None

#
#
#

def _load(model: ReversiLogisticModel) -> ReversiLogisticModelConfig:

    with model.config_file_path.open("r", encoding="utf-8") as f:
        config_raw_data = json5.load(f)
        model.log_event(model.Relevance.DEBUG, f"JSON file '{f}' read.")

    if model.base_dir_override is not None:
        config_raw_data['base_dir'] = model.base_dir_override
        model.log_event(model.Relevance.DEBUG, f"Value for base_dir changed to '{model.base_dir_override}'.")

    cfg = ReversiLogisticModelConfig(**config_raw_data)
    return cfg

def _validate(model: ReversiLogisticModel,
              cfg: ReversiLogisticModelConfig):
    _validate_base_dir(model, cfg)

def _validate_base_dir(model: ReversiLogisticModel,
                       cfg: ReversiLogisticModelConfig):
    if not cfg.base_dir.exists():
        error_message = f"The base dir: '{cfg.base_dir}' doesn't exist."
        model.log_event(model.Relevance.ERROR, error_message)
        raise RuntimeError(error_message)
    if not cfg.base_dir.is_dir():
        error_message = f"The base dir: '{cfg.base_dir}' is not a directory."
        model.log_event(model.Relevance.ERROR, error_message)
        raise RuntimeError(error_message)    
    is_writable = os.access(cfg.base_dir, os.W_OK)
    if not is_writable:
        error_message = f"The base dir: '{cfg.base_dir}' is not writable."
        model.log_event(model.Relevance.ERROR, error_message)
        raise RuntimeError(error_message)    

def _store_to_file(model: ReversiLogisticModel,
                   cfg: ReversiLogisticModelConfig):

    sha3_256_hash = hashlib.sha3_256()
    config_json = cfg.model_dump_json(indent=4)
    current_checksum = hashlib.sha3_256(config_json.encode()).hexdigest()
    
    file_path = cfg.base_dir / model.get_cache_file_path_for_next_level()
    checksum_file_path = file_path.with_name(file_path.name + ".SHA3-256")
    
    model.log_event(model.Relevance.DEBUG, f"File: {file_path}, current_checksum = {current_checksum}")
    
    has_to_write = False
    
    if not file_path.exists():
        has_to_write = True
    else:
        if not checksum_file_path.exists():
            model.log_event(model.Relevance.ERROR, f"The checksum file {checksum_file_path} is missing.")
            raise RuntimeError(f"The checksum file {checksum_file_path} is missing.")    
        with open(checksum_file_path, 'r') as checksum_file:
            stored_checksum = checksum_file.read().strip()
        model.log_event(model.Relevance.DEBUG, f"File: {file_path}, stored_checksum = {stored_checksum}")

        on_disk_recomputed_sha3_256_hash = hashlib.sha3_256()    
        with open(file_path, 'rb') as f:
            while chunk := f.read(8192):
                on_disk_recomputed_sha3_256_hash.update(chunk)
        on_disk_recomputed_checksum = on_disk_recomputed_sha3_256_hash.hexdigest()
        model.log_event(model.Relevance.DEBUG, f"File: {file_path}, on_disk_recomputed_checksum = {on_disk_recomputed_checksum}")
        
        if on_disk_recomputed_checksum != stored_checksum:
            model.log_event(model.Relevance.ERROR, f"The checksum file on disk {checksum_file_path} is not matching.")
            raise RuntimeError(f"The checksum file on disk {checksum_file_path} is not matching.")    

        if current_checksum != stored_checksum:
            has_to_write = True

    if has_to_write:
        file_path.write_text(config_json, encoding="utf-8")
        with open(checksum_file_path, 'w') as checksum_file:
            checksum_file.write(current_checksum)
        model.log_event(model.Relevance.INFO, f"File '{file_path}' and checksum written to disk.")


