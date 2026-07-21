#
# rlm_config_schemas.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
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

from __future__ import annotations

from pydantic import (BaseModel, Field, PositiveInt, NonNegativeInt,
                      field_validator, field_serializer,
                      ConfigDict, model_validator)

from typing import List, Annotated, Optional, Any

from pathlib import Path

from twolm.board import (Bitboard,
                         bitboard_from_hex_str)



__all__ = ['ReversiLogisticModelConfig']



class RLMBaseConfig(BaseModel):
    properties: dict[str, Any] = Field(default_factory=dict)
    
class RegabDBConnectionConfig(RLMBaseConfig):
    """
    Configuration for connecting to the Regab database.
    """
    dbname: str
    user: str
    host: str
    port: int = 5432
    password: Optional[str] = None

StatusString = Annotated[str, Field(pattern=r"^[A-Z]{3}$")]

class RegabDataSetConfig(RLMBaseConfig):
    """
    Configuration for a Regab data set, including the database connection and filtering criteria.
    """
    regab_db_connection: RegabDBConnectionConfig
    bid: List[NonNegativeInt]
    status: List[StatusString]
    ec: int = Field(..., ge=0, le=60)

class PatternConfig(RLMBaseConfig):
    """
    Configuration for a pattern, including its name and mask.
    """
    name: str
    mask: Bitboard = Field(..., description="A 64-bit hex string (16 hex digits), case-insensitive.")
 
    model_config = ConfigDict(arbitrary_types_allowed=True)

    @field_validator("mask", mode="before")
    @classmethod
    def parse_hex_to_bitboard(cls, h: str) -> Bitboard:
        """
        Converts a hex string (e.g., '0x0000000000000107') into a Bitboard.
        """
        return bitboard_from_hex_str(h)
 
    @field_serializer("mask")
    def serialize_mask_to_hex(self, mask: Bitboard) -> str:
        return f"{mask:016X}"

class PatternSetConfig(RLMBaseConfig):
    """
    Configuration for a set of patterns, including the name and a list of individual pattern configurations.
    """
    name: str
    patterns: List[PatternConfig] = []

class MobilityFeatureConfig(RLMBaseConfig):
    """
    Configuration for a mobility feature.
    """
    name: str
    mask: Bitboard = Field(..., description="Legal moves mask in HEX format, no 0x prefix, just 16 digits.")
    amask: Bitboard = Field(..., description="Anti legal moves mask in HEX format, no 0x prefix, just 16 digits.")
 
    model_config = ConfigDict(arbitrary_types_allowed=True) 

    @field_validator("mask", "amask", mode="before")
    @classmethod
    def parse_hex_to_bitboard(cls, h: str) -> Bitboard:
        """
        Converts a hex string (e.g., '0x0000000000000107') into a Bitboard.
        """
        return bitboard_from_hex_str(h)

    @field_serializer("mask", "amask")
    def serialize_mask_to_hex(self, mask: Bitboard) -> str:
        return f"{mask:016X}"
    
class MobilitySetConfig(RLMBaseConfig):
    """
    Configuration for a set of mobility features, including the name and a list of individual configurations.
    """
    name: str
    mobility_features: List[MobilityFeatureConfig] = []

class FeatureSetConfig(RLMBaseConfig):
    """
    Configuration for the set of features.
    """
    name: Optional[str] = 'NoNameFeatureSet'
    intercept: Optional[bool] = False
    mobility_set: Optional[MobilitySetConfig] = None
    pattern_set: Optional[PatternSetConfig] = None
    
class StatModelConfig(BaseModel):
    """
    Configuration for a statistical model, including the frequency cut-off threshold.
    """
    frequency_cut_off: PositiveInt = 1
    logit_clipping: float = Field(default=0.05, gt=0, lt=0.5)
    ridge_regularization: float = Field(default=0.00, ge=0.00)

class ReversiLogisticModelConfig(RLMBaseConfig):
    """
    Configuration for the Reversi Logistic Model, including general settings,
    data set configuration.
    """
    name: str
    description: str
    base_dir: Path
    use_cache: bool = True
    regab_data_set: RegabDataSetConfig
    feature_set: FeatureSetConfig
    stat_model: StatModelConfig
    
