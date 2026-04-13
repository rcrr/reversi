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

from itertools import accumulate


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

class StatModelConfig(BaseModel):
    frequency_cut_off: PositiveInt = 1
    
class ReversiLogisticModelConfig(BaseModel):
    name: str
    description: str
    base_dir: Path
    project_dir: Path
    regab_indexed_data_set_cached: RegabIndexedDataSetCachedConfig
    stat_model: StatModelConfig
    
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
        self.pattern_w_ranges = None
        self.iwmap_pattern_offset = None
        self.iwmap = None
        self.wmap = None
        self.wmap_fallback = None

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

    def compute_wmaps(self) -> None:
        """
        Computes the following data structures given the cut_off hyper-parameter:
            pattern_w_ranges: npt.NDArray[np.int64], shape(P, 3)
                Lists for each pattern, ordered by pid (pattern index):
                - fallback the index value of w used for the fallback (cut-off) configurations.
                - w_min the first index value of w assigned to the pattern ( equal to fallback when fallback is not -1).
                - w_max the last index value of w assigned to the pattern.
            iwmap_pattern_offset: npt.NDArray[np.uint32], shape(P + 1,)
                Gives the position of a pattern in the iwmap array.
                iwmap_pattern_offset[pid] gives the position of configuration zero for the pattern identified by pid.
            iwmap: npt.NDArray[np.int64], shape(K,)
                Assigns to each possible configuration the index of w. -1 when the configuration is not found
                in the dataset.
                Given pid, and pattern_conf_id the formula:
                    iwmap_pattern_offset[pid + pattern_conf_id] returns the index of w related to (pid, pattern_conf_id)
            wmap: npt.NDArray[np.int64], shape(W, 3)
                Given the w index returns the pattern index, the configuration index, and the frequency configuration
                pid, pattern_conf_id, pattern_conf_frequency = wmap[w_index]
            wmap_fallback: npt.NDArray[np.int64], shape(F, 3)
                Contains the entries of wmap being excluded by the cut-off. It has:
                - pid: pattern id
                - pattern_conf_id: index of the pattern configuration excluded by the cut-off rule
                - pattern_conf_frequency: the frequency of the patetrn conficuration within the dataset
        Where:
            P is the count of patterns defined by the model.
            K is the count of all pattern configurations (K = sum([p.n_configurations for p in patterns])).
            W is the count of all model weights.
            F is the count of all cut-off configurations.
        """

        logger.debug(f"Analyzing patterns defined by the model.")

        cut_off = self.cfg.stat_model.frequency_cut_off
        logger.debug(f"cut_off = {cut_off} ...  (Frequency cut-off value set by the statistical model)")

        patterns = self.rids.pset.patterns
        P = len(patterns)
        logger.debug(f"P = {P} ...  (count of patterns defined by the model)")

        p_num_configurations = [p.n_configurations for p in patterns]
        logger.debug(f"p_num_configurations = {p_num_configurations} ... (number of combinatorial configurations for each pattern)")
        iwmap_pattern_offset = np.array([0] + list(accumulate(p_num_configurations)), dtype=np.uint32)
        K = iwmap_pattern_offset[P]
        logger.debug(f"K = {K} ... (length of the iwmap array, equal to iwmap_pattern_offset[P])")

        iwmap = np.full(K, -1, dtype=np.int64)
        pindexes = self.rids.pindexes
        N = pindexes.shape[0]
        next_w = 0
        p_w_ranges = []

        # Pre-allocate the inverse map after the loop starts or use a list to collect parts
        # Since we don't know the final 'next_w' until the end, 
        # collecting blocks and stacking them at the end is the most efficient way.
        wmap_blocks = []
        fallback_data = [] # To store [pid, config_id, frequency] for each fallback entry
        
        for pid, p in enumerate(patterns):
            cols = self.rids.get_pattern_columns(pid)
            if len(cols) != p.n_instances:
                raise ValueError(f"Pattern {p.name}, pid = {pid}: len(cols) = {len(cols)} is not correct.")
            logger.debug(f"Analyzing pattern {p.name}, pid = {pid}, cols = {cols}")
            p_pindexes = pindexes[:, cols]
            flat_p_pindexes = p_pindexes.ravel()
            unique_values, counts = np.unique(flat_p_pindexes, return_counts=True)
            stats = np.column_stack((unique_values, counts))
            if sum(counts) != len(cols) * N:
                raise ValueError(f"Pattern {p.name}, sum(counts) != len(cols) * N ... invariance violated.")
            mask = stats[:, 1] < cut_off
            stats_filtered_below = stats[mask]
            stats_filtered_above = stats[~mask]
            if len(stats_filtered_below) + len(stats_filtered_above) != len(stats):
                raise ValueError(f"Pattern {p.name}, len(stats_filtered_below) + len(stats_filtered_above) != len(stats) ... invariance violated.")

            w_indices = np.empty(len(stats), dtype=np.int64)
            if len(stats_filtered_below) > 0:
                fallback = next_w
                w_indices[mask] = fallback
                next_w += 1
                fallback_pattern_configuration_is_present = True
                logger.debug(f"  Configurations below freq. cut-off = {len(stats_filtered_below)}, occurrences = {stats_filtered_below[:, 1].sum()}")
            else:
                fallback = -1
                fallback_pattern_configuration_is_present = False

            # Handle the "above cutoff" group (mapped to a progressive sequence)
            n_above = (~mask).sum()
            if n_above > 0:
                # Generate a sequence starting from the current next_w
                w_indices[~mask] = np.arange(next_w, next_w + n_above)
                # Increment next_w by the number of unique configurations added
                next_w += n_above

            # Add the new column to 'stats' and refresh the filtered views
            stats = np.column_stack((stats, w_indices))

            p_iwmap = iwmap[iwmap_pattern_offset[pid]:iwmap_pattern_offset[pid + 1]]
            # MAPPING:
            # stats[:, 0] contains the unique configurations (the "keys" or indices)
            # stats[:, 2] contains the assigned w_index values
            p_iwmap[stats[:, 0].astype(np.int64)] = stats[:, 2]
            logger.debug(f"Pattern {p.name}: p_iwmap populated with {len(stats)} unique configurations.")
            
            # Identify the range of w_indices for the current pattern
            # Using w_indices (from your previous step) to find min and max
            w_min = w_indices.min() if len(w_indices) > 0 else -1
            w_max = w_indices.max() if len(w_indices) > 0 else -1
            p_w_ranges.append({
                'pid': pid,
                'fallback': fallback,
                'w_min': w_min,
                'w_max': w_max
            })

            # The number of unique w assigned to this pattern is (w_max - w_min + 1)
            num_w_pattern = w_max - w_min + 1
            pattern_w_mapping = np.empty((num_w_pattern, 3), dtype=np.int64)
            # Column 0: Always the current Pattern ID
            pattern_w_mapping[:, 0] = pid
            # Fill the second column with Configuration IDs
            # By default, we use -1 (which covers the fallback case)
            pattern_w_mapping[:, 1] = -1

            # Handle Fallback Frequency
            if fallback_pattern_configuration_is_present:
                # Sum of frequencies for all configurations below cutoff
                fallback_freq = stats_filtered_below[:, 1].sum()
                
                # Assign to the specific row in the block
                # Usually fallback is at w_min in your current logic
                fallback_rel_idx = fallback - w_min
                pattern_w_mapping[fallback_rel_idx, 2] = fallback_freq
                
                # Collect detailed fallback entries for the separate table
                # stats_filtered_below has [config_id, frequency, w_index]
                for row in stats_filtered_below:
                    fallback_data.append([pid, row[0], row[1]])

            
            # For configurations above cutoff, we map the w_index to its config_id
            # mask was defined as: stats[:, 1] < cut_off
            above_mask = ~mask 
            if above_mask.any():
                # Get the w_indices and config_ids for the 'above' group
                w_above = stats[above_mask, 2]
                config_above = stats[above_mask, 0]
                freq_above = stats[above_mask, 1]
                # The index in pattern_w_mapping is (w - w_min)
                relative_indices = (w_above - w_min).astype(np.int64)
                pattern_w_mapping[relative_indices, 1] = config_above
                pattern_w_mapping[relative_indices, 2] = freq_above
            wmap_blocks.append(pattern_w_mapping)
        
        # Combine all blocks into the final inverse mapping table
        # Index of this array is 'w', value is [PID, CONFIG_ID]
        wmap = np.vstack(wmap_blocks)
        
        # Final fallback details table: [PID, CONFIG_ID, FREQ]
        # This contains every single config_id that was merged into a fallback w
        fallback_details_table = np.array(fallback_data, dtype=np.int64)      
    
        self.pattern_w_ranges = np.array([[m['fallback'], m['w_min'], m['w_max']] for m in p_w_ranges], dtype=np.int64)
        self.iwmap_pattern_offset = iwmap_pattern_offset
        self.iwmap = iwmap
        self.wmap = wmap
        self.wmap_fallback = fallback_details_table

        logger.debug(f"Table wmap created. Shape: {wmap.shape} (Total weights assigned: {next_w})")
        
