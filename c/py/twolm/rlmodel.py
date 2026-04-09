#
# rlmodel.py
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

"""
rlmodel module
"""

from __future__ import annotations

from twolm.domain import *
from twolm.rdata import *

import json5

from pydantic import (BaseModel, Field, PositiveInt, NonNegativeInt, DirectoryPath,
                      field_validator, computed_field, ConfigDict)
from typing import List, Optional, Annotated

from pathlib import Path

import numpy as np


__all__ = ['ReversiLogisticModel']


class RegabDBConnection(BaseModel):
    dbname: str
    user: str
    host: str
    port: int = 5432
    password: Optional[str] = None

class PatternConfig(BaseModel):
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

class PatternSetConfig(BaseModel):
    name: str
    patterns: List[PatternConfig] = []

StatusString = Annotated[str, Field(pattern=r"^[A-Z]{3}$")]

class RegabDataSetConfig(BaseModel):
    regab_db_connection: RegabDBConnection
    bid: List[NonNegativeInt]
    status: List[StatusString]
    ec: int = Field(..., ge=0, le=60)

class RegabDataSetCachedConfig(BaseModel):
    regab_data_set: RegabDataSetConfig
    filename: Path

class RegabDataSetConfig(BaseModel):
    regab_data_set_cached: RegabDataSetCachedConfig

class RegabIndexedDataSetConfig(BaseModel):
    regab_data_set_cached: RegabDataSetCachedConfig
    pattern_set: PatternSetConfig
    has_indexes: bool
    has_findexes: bool
    has_pindexes: bool
    has_lookup: bool
    has_revmap: bool
    
class RegabIndexedDataSetCachedConfig(BaseModel):
    regab_indexed_data_set: RegabIndexedDataSetConfig
    filename: Path

class ReversiLogisticModelConfig(BaseModel):
    name: str
    description: str
    base_dir: Path
    project_dir: Path
    regab_indexed_data_set_cached: RegabIndexedDataSetCachedConfig
    
    @computed_field
    @property
    def full_project_dir(self) -> Path:
        return self.base_dir / self.project_dir

class ReversiLogisticModel:
    """
    """
    def __init__(self,
                 config_file_path: str,
                 base_dir_override: str | None =None):
        """
        Initializes a ReversiLogisticModel instance by
        loading and validating JSON configuration from a file.
        """
        if not isinstance(config_file_path, str):
            raise TypeError('Argument config_file_path is not an instance of str')
        
        cfp = Path(config_file_path)

        if not cfp.exists():
            raise FileNotFoundError(f"No such file: '{config_file_path}'")
        if not cfp.is_file():
            raise FileNotFoundError(f"File path is not a file: '{config_file_path}'")

        with cfp.open("r", encoding="utf-8") as f:
            config_raw_data = json5.load(f)

        if base_dir_override is not None:
            config_raw_data['base_dir'] = base_dir_override

        self.cfg = ReversiLogisticModelConfig(**config_raw_data)
