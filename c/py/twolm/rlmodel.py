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
import sys
import logging

from pydantic import (BaseModel, Field, PositiveInt, NonNegativeInt, DirectoryPath,
                      field_validator, computed_field, ConfigDict)
from typing import List, Optional, Annotated

from pathlib import Path

import numpy as np


__all__ = ['ReversiLogisticModel']

logging.basicConfig(stream=sys.stdout, level=logging.INFO, format='%(message)s')
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)
logger.propagate = False 
console_handler = logging.StreamHandler(sys.stdout)
console_handler.setLevel(logging.INFO)
logger.addHandler(console_handler)

class RegabDBConnectionConfig(BaseModel):
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
    regab_db_connection: RegabDBConnectionConfig
    bid: List[NonNegativeInt]
    status: List[StatusString]
    ec: int = Field(..., ge=0, le=60)

class RegabDataSetCachedConfig(BaseModel):
    regab_data_set: RegabDataSetConfig
    filename: Path
    purge: bool = False

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
    purge: bool = False

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

        self.rds = None
        self.rids = None

    def load_regab_data_set_from_db(self) -> None:
        """
        Loads the regab data set from the database as defined by the config file.
        Then, saves the data set to the cache file, again as defined by the config file.
        """
        cfg_rdsc = (self.cfg
                    .regab_indexed_data_set_cached
                    .regab_indexed_data_set
                    .regab_data_set_cached)
        
        cfg_rds = cfg_rdsc.regab_data_set
        
        cfg_conn = cfg_rds.regab_db_connection
        
        rc = RegabDBConnection(cfg_conn.dbname,
                               cfg_conn.user,
                               cfg_conn.host,
                               str(cfg_conn.port),
                               cfg_conn.password)
        
        rds = RegabDataSet.extract_from_db(rc,
                                           cfg_rds.bid,
                                           cfg_rds.status,
                                           cfg_rds.ec)

        rc.close()

        full_path_filename = self.cfg.full_project_dir / cfg_rdsc.filename
        rds.store_to_file(full_path_filename)
        
        self.rds = rds

    def load_regab_data_set_from_file(self) -> None:
        """
        Loads the regab data set from the cache file as defined by the config file.
        """
        cfg_rdsc = (self.cfg
                    .regab_indexed_data_set_cached
                    .regab_indexed_data_set
                    .regab_data_set_cached)
        
        cfg_rds = cfg_rdsc.regab_data_set
        
        full_path_filename = str(self.cfg.full_project_dir / cfg_rdsc.filename)
        rds = RegabDataSet.load_from_file(full_path_filename)

        if not cfg_rds.bid == rds.bid:
            raise ValueError(f"The value of cfg_rds.bid {cfg_rds.bid} and rds.bid {rds.bid} differ.")
        if not cfg_rds.status == rds.status:
            raise ValueError(f"The value of cfg_rds.status {cfg_rds.status} and rds.status {rds.status} differ.")
        if not cfg_rds.ec == rds.ec:
            raise ValueError(f"The value of cfg_rds.ec {cfg_rds.ec} and rds.ec {rds.ec} differ.")        
        
        self.rds = rds

    def load_regab_data_set(self) -> None:
        """
        Loads the regab data set from the cache file if it exists, or from db otherwise,
        as defined by the config file.
        """
        cfg_rdsc = (self.cfg
                    .regab_indexed_data_set_cached
                    .regab_indexed_data_set
                    .regab_data_set_cached)
        
        full_path_filename = self.cfg.full_project_dir / cfg_rdsc.filename
        if full_path_filename.exists():
            self.load_regab_data_set_from_file()
        else:
            self.load_regab_data_set_from_db()

    def purge_regab_data_set_cache(self) -> None:
        """
        Removes the regab data set cache file.
        """
        cfg_rdsc = (self.cfg
                    .regab_indexed_data_set_cached
                    .regab_indexed_data_set
                    .regab_data_set_cached)
        
        logger.debug(f"Entering method purge_regab_data_set_cache().")
        if not cfg_rdsc.purge:
            logger.debug(f"The config file doesn't prescribe the purge action.")
            return
        logger.debug(f"The config file prescribes the purge action.")
        full_path_filename = self.cfg.full_project_dir / cfg_rdsc.filename
        logger.debug(f"File to be purged: {full_path_filename}")
        if full_path_filename.exists():
            checksum = full_path_filename.with_name(full_path_filename.name + ".SHA3-256")
            logger.debug(f"Purging regab data set cache file.")
            full_path_filename.unlink()
            logger.debug(f"Purging regab data set cache checksum file.")
            checksum.unlink(missing_ok=True)
            logger.debug(f"Files have been deleted.")
        else:
            logger.debug(f"The file doesn't exist.")
        

        
    def load_regab_indexed_data_set(self) -> None:
        """
        Loads the regab indexed data set.
        """
        logger.debug(f"Entering method load_regab_indexed_data_set().")
        if self.rids is not None:
            logger.debug(f"Attribute rids is already computed, returning.")
            return
        else:
            logger.debug(f"Attribute rids is None, loading the data.")
            cfg_ridsc = self.cfg.regab_indexed_data_set_cached
            cfg_rids = cfg_ridsc.regab_indexed_data_set
            logger.debug(f"cfg_ridsc = {cfg_ridsc}")
            logger.debug(f"cfg_rids = {cfg_rids}")

            are_indexes_required = cfg_rids.has_indexes
            are_findexes_required = cfg_rids.has_findexes
            are_pindexes_required = cfg_rids.has_pindexes
            is_lookup_required = cfg_rids.has_lookup
            is_revmap_required = cfg_rids.has_revmap
        
            full_path_filename = self.cfg.full_project_dir / cfg_ridsc.filename
            logger.debug(f"Looking for cache file: {full_path_filename}")
            if full_path_filename.exists():
                logger.debug(f"Loading cache file ...")
                rids = RegabIndexedDataSet.load_from_file(full_path_filename)
            else:
                logger.debug(f"Cache file not found, rids object has to be created.")
                if self.rds is None:
                    logger.debug(f"Loading regab data set, from cache file or from database.")
                    self.load_regab_data_set()
                logger.debug(f"Regab data set has been loaded.")
                cfg_pset = cfg_rids.pattern_set
                patterns = [Pattern(elt.name, elt.mask) for elt in cfg_pset.patterns]
                pset = PatternSet(cfg_pset.name, patterns)
                logger.debug(f"Pattern set has been created.")
                rids = RegabIndexedDataSet(self.rds, pset)
                logger.debug(f"Object rids has been assembled.")

            is_file_modified = False
            logger.debug(f"Checking what index or lookup is missing.")
            if are_indexes_required and rids.indexes is None:
                rids.compute_indexes()
                is_file_modified = True
            if are_findexes_required and rids.findexes is None:
                rids.flatten_indexes()
                is_file_modified = True
            if are_pindexes_required and rids.pindexes is None:
                rids.compute_principal_indexes()
                is_file_modified = True
            if is_lookup_required and rids.lookup is None:
                rids.compute_lookup()
                is_file_modified = True
            if is_revmap_required and rids.revmap is None:
                rids.compute_revmap()
                is_file_modified = True
            logger.debug(f"All requested indexes, lookup table, and reverse map have been computed.")
            if rids.indexes is not None and are_indexes_required is False:
                logger.debug(f"Removing indexes.")
                rids.indexes = None
            if rids.findexes is not None and are_findexes_required is False:
                logger.debug(f"Removing findexes.")
                rids.findexes = None
            if rids.pindexes is not None and are_pindexes_required is False:
                logger.debug(f"Removing pindexes.")
                rids.pindexes = None
            if rids.lookup is not None and is_lookup_required is False:
                logger.debug(f"Removing lookup.")
                rids.lookup = None
            if rids.revmap is not None and is_revmap_required is False:
                logger.debug(f"Removing revmap.")
                rids.revmap = None
                
            if is_file_modified:
                rids.store_to_file(full_path_filename)
                logger.debug(f"Cache file {full_path_filename} has been written.")
                self.purge_regab_data_set_cache()
                
            self.rids = rids
