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

This module provides the ReversiLogisticModel class, which handles the logistic regression model for the Reversi game.
It manages the loading and processing of datasets, computes necessary mappings and matrices, and prepares the model for training and evaluation.
"""

from __future__ import annotations

from twolm.domain import *
from twolm.rdata import *

import struct
import json5
import hashlib
import sys
import logging

from pydantic import (BaseModel, Field, PositiveInt, NonNegativeInt, DirectoryPath,
                      field_validator, field_serializer, computed_field, ConfigDict)

from typing import List, Optional, Annotated, Self, Callable

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
    """
    Configuration for connecting to the Regab database.
    """
    dbname: str
    user: str
    host: str
    port: int = 5432
    password: Optional[str] = None

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

StatusString = Annotated[str, Field(pattern=r"^[A-Z]{3}$")]
"""
Annotation for a status string, which must be exactly three uppercase letters.
"""

class RegabDataSetConfig(BaseModel):
    """
    Configuration for a Regab data set, including the database connection and filtering criteria.
    """
    regab_db_connection: RegabDBConnectionConfig
    bid: List[NonNegativeInt]
    status: List[StatusString]
    ec: int = Field(..., ge=0, le=60)

class RegabDataSetCachedConfig(BaseModel):
    """
    Configuration for caching a Regab data set, including the data set configuration
    and the cache file details.
    """
    regab_data_set: RegabDataSetConfig
    filename: Path
    purge: bool = False

class RegabIndexedDataSetConfig(BaseModel):
    """
    Configuration for an indexed Regab data set, including the cached data set configuration
    and flags for various indexes and lookups.
    """
    regab_data_set_cached: RegabDataSetCachedConfig
    pattern_set: PatternSetConfig
    has_indexes: bool
    has_findexes: bool
    has_pindexes: bool
    has_lookup: bool
    has_revmap: bool
    
class RegabIndexedDataSetCachedConfig(BaseModel):
    """
    Configuration for caching an indexed Regab data set,
    including the indexed data set configuration and the cache file details.
    """
    regab_indexed_data_set: RegabIndexedDataSetConfig
    filename: Path
    purge: bool = False

class StatModelConfig(BaseModel):
    """
    Configuration for a statistical model, including the frequency cut-off threshold.
    """
    frequency_cut_off: PositiveInt = 1
    
class ReversiLogisticModelConfig(BaseModel):
    """
    Configuration for the Reversi logistic model, including general settings,
    data set configurations, and statistical model settings.
    """
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
    This class is designed to handle the logistic regression model for the Reversi game.
    It manages the loading and processing of datasets, computes necessary mappings and matrices, and prepares the model for training and evaluation.

    Attributes:
        cfg (ReversiLogisticModelConfig): Configuration object containing all necessary parameters for the model.
        rids (Optional[RegabIndexedDataSet]): Loaded regab indexed data set.
        pattern_w_ranges (Optional[npt.NDArray[np.int64]]): Array listing for each pattern the fallback, w_min, and w_max values.
        iwmap_pattern_offset (Optional[npt.NDArray[np.uint32]]): Array giving the position of a pattern in the iwmap array.
        iwmap (Optional[npt.NDArray[np.int64]]): Array assigning to each possible configuration the index of w.
        wmap (Optional[npt.NDArray[np.int64]]): Array mapping w index to pattern index, configuration index, and frequency.
        wmap_fallback (Optional[npt.NDArray[np.int64]]): Array containing entries of wmap excluded by the cut-off.
        X (Optional[npt.NDArray[np.uint32]]): Design matrix for the logistic regression model.

    Methods:
        __init__(self, cfg: ReversiLogisticModelConfig) -> None: Initializes the model with the provided configuration.
        from_json_path(cls, config_file_path: str | Path, base_dir_override: str | None = None) -> ReversiLogisticModel: Initializes the model with JSON configuration from a file.
        load_regab_data_set_from_db(self) -> None: Loads the regab data set from the database and saves it to a cache file.
        load_regab_data_set_from_file(self) -> None: Loads the regab data set from a cache file.
        load_regab_data_set(self) -> None: Loads the regab data set from a cache file if it exists, or from the database otherwise.
        purge_regab_data_set_cache(self) -> None: Removes the regab data set cache file if the purge flag is set.
        load_regab_indexed_data_set(self) -> None: Loads the regab indexed data set, computing necessary indexes and lookups if needed.
        compute_wmaps(self) -> None: Computes the pattern_w_ranges, iwmap_pattern_offset, iwmap, wmap, and wmap_fallback arrays based on the frequency cut-off.
        compute_design_matrix(self) -> None: Computes the design matrix X for the logistic regression model.
        write_core_object_data(self, fw: Callable[[bytes], None]) -> None: Writes the core data of the ReversiLogisticModel instance to a binary file using the provided writer function.
        store_to_file(self, filename: str | Path) -> None: Saves the ReversiLogisticModel instance to a binary file and calculates the SHA3-256 checksum.
        load_from_file(cls, filename: str | Path, checksum: bool = True) -> ReversiLogisticModel: Loads a ReversiLogisticModel instance from a binary file.
    """
    def __init__(self, cfg: ReversiLogisticModelConfig):
        """
        Initializes an instance of the ReversiLogisticModel class with the provided configuration.

        Parameters
        ----------
        cfg : ReversiLogisticModelConfig
            Configuration object containing all necessary parameters for the model.

        Raises
        ------
        TypeError
            If the argument `cfg` is not an instance of `ReversiLogisticModelConfig`.

        Notes
        -----
        - The method initializes the model with the provided configuration.
        - It sets up the logger and initializes several attributes to `None`.
        """
        if not isinstance(cfg, ReversiLogisticModelConfig):
            raise TypeError('Argument cfg is not an instance of ReversiLogisticModelConfig')
        

        self.cfg = cfg
        self.fqcn: str = f"{self.__class__.__module__}.{self.__class__.__qualname__}"
        self.rids = None
        self.pattern_w_ranges = None
        self.iwmap_pattern_offset = None
        self.iwmap = None
        self.wmap = None
        self.wmap_fallback = None
        self.X = None

    @classmethod
    def from_json_path(cls,
                       config_file_path: str | Path,
                       base_dir_override: str | None =None) -> ReversiLogisticModel:
        """
        Initializes the model with JSON configuration from a file.
        Validates the configuration file path and loads the configuration data, optionally overriding the base directory.

        The definition of the structure of the JSON config model is realized by adopting the Pydantic library.
        The root class is ReversiLogisticModelConfig.

        Parameters
        ----------
        config_file_path: str
            The name as string or Path of the JSON file read as the configuration for the model.
        base_dir_override: str | None =None
            Eventually overrides the base directory. It is used for unit testing.

        Raises
        ------
        TypeError
            If the parameter config_file_path is not a string or a Path object.
        FileNotFoundError
            If the config file is not found.
        """
        if not isinstance(config_file_path, (str, Path)):
            raise TypeError('Argument config_file_path is not an instance of str or Path')
        
        cfp = Path(config_file_path)

        if not cfp.exists():
            raise FileNotFoundError(f"No such file: '{config_file_path}'")
        if not cfp.is_file():
            raise FileNotFoundError(f"File path is not a file: '{config_file_path}'")

        with cfp.open("r", encoding="utf-8") as f:
            config_raw_data = json5.load(f)

        if base_dir_override is not None:
            config_raw_data['base_dir'] = base_dir_override

        cfg = ReversiLogisticModelConfig(**config_raw_data)

        return cls(cfg=cfg)
        

    def load_regab_data_set_from_db(self) -> None:
        """
        Loads the regab data set from the database as defined by the config file.
        Then, saves the data set to the cache file, again as defined by the config file.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        Exception
            If there is an error during the database connection or data extraction.
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

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        ValueError
            If there is a mismatch between the configuration settings (bid, status, ec) and the loaded data set.
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
        Loads the regab data set from the cache file if it exists, or from the database otherwise,
        as defined by the config file.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        ValueError
            If there is a mismatch between the configuration settings (bid, status, ec) and the loaded data set.
        Exception
            If there is an error during the database connection or data extraction.
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
        Removes the regab data set cache file if the purge flag is set in the configuration.
        This method checks if the purge flag is enabled and deletes the cache file along with its checksum file if they exist.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        None
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
        This method loads the regab indexed data set from a cache file if it exists.
        If the cache file does not exist, it creates the regab indexed data set by loading the regab data set
        and computing the necessary indexes and lookups based on the configuration settings.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        ValueError
            If there is a mismatch between the configuration settings and the loaded data set.
        Exception
            If there is an error during the creation of the regab indexed data set.
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

        The method performs the following steps:
            1. Initializes the cut-off value from the configuration.
            2. Retrieves the list of patterns and their respective number of configurations.
            3. Computes the offset for each pattern in the iwmap array using the cumulative sum of the number of configurations.
            4. Initializes the iwmap array with -1, indicating that no configuration is initially mapped.
            5. Iterates over each pattern to compute the unique configurations and their frequencies.
            6. Determines the fallback configuration for each pattern if any configuration's frequency is below the cut-off.
            7. Assigns unique w indices to configurations with frequencies above the cut-off.
            8. Updates the iwmap array with the computed w indices.
            9. Constructs the pattern_w_ranges array with fallback, w_min, and w_max values for each pattern.
            10. Constructs the wmap array mapping each w index to the corresponding pattern index, configuration index, and frequency.
            11. Constructs the wmap_fallback array containing configurations excluded by the cut-off.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        ValueError
            If there is a mismatch between the expected number of instances and the actual number of instances for a pattern.
            If the sum of counts does not match the expected total number of instances.
            If the sum of filtered below and above configurations does not match the total number of configurations.
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
        
    def compute_design_matrix(self) -> None:
        """
        Computes the design matrix for the logistic regression model. The design matrix is used to represent the input features
        in a compressed form, where each element corresponds to the index of the weight vector w that is different from zero and
        is always equal to one. This allows the computation of the linear predictor as:

            linear_predictor = np.sum(w[X], axis=1)

        The design matrix X is also used in the gradient method to compute the backward pass:

            grad = np.bincount(X.ravel(), weights=np.repeat(dyh_rn, P), minlength=N)

        where:
            N = len(w)
            P = X.shape[1]
            dyh_rn = yh * (1. - yh) * (yh - y)
            yh = sigmoid(linear_predictor)

        The method performs the following steps:
            1. Checks if the design matrix X is already computed. If so, it returns immediately.
            2. Loads the regab indexed data set if it is not already loaded.
            3. Computes the principal indexes if they are not already computed.
            4. Computes the iwmap table if it is not already computed.
            5. Retrieves the principal indexes and the number of instances per pattern.
            6. Computes the column offsets for each pattern.
            7. Adjusts the principal indexes with the column offsets.
            8. Validates that the adjusted principal indexes do not refer to invalid values in the iwmap array.
            9. Initializes the design matrix X with the appropriate shape.
            10. Populates the design matrix X by taking the corresponding values from the iwmap array.

        Parameters
        ----------
        None

        Returns
        -------
        None

        Raises
        ------
        ValueError
            If the principal indexes refer to an invalid value in the iwmap array.
            If there is a mismatch between the indexes and valid values in the iwmap array.
        """
        logger.debug(f"Method compute design matrix.")

        if self.X is not None:
            logger.debug(f"The design matrix is already computed, returning.")
            return

        if self.rids is None:
            logger.debug(f"The regab indexed data set is missing, loading it.")
            self.load_regab_indexed_data_set()

        if self.rids.pindexes is None:
            logger.debug(f"The principal indexes are missing, computing them.")
            self.rids.compute_principal_indexes()
            
        if self.iwmap_pattern_offset is None or self.iwmap is None:
            logger.debug(f"The iwmap table is missing, computing it.")
            self.compute_wmaps()

        pindexes = self.rids.pindexes
        
        n_instances_per_pattern = [p.n_instances for p in self.rids.pset.patterns]
        col_offsets = np.repeat(self.iwmap_pattern_offset[:-1], n_instances_per_pattern)
        pindexes_with_offsets = pindexes + col_offsets

        if np.any(self.iwmap[pindexes_with_offsets] == -1):
            raise ValueError(f"It should never happen, pindexes is referring an invalid value.")

        if not pindexes.size == np.count_nonzero(self.iwmap[pindexes_with_offsets] >= 0):
            raise ValueError(f"It should never happen, ismatch between indexes and valid values.")
        
        self.X = np.empty(pindexes.shape, dtype=np.uint32)
        np.take(self.iwmap, pindexes_with_offsets, out=self.X)
        
        logger.debug(f"Design matrix has been computed.")

    def write_core_object_data(self, fw: Callable[[bytes], None]) -> None:
        """
        Writes the core data of the ReversiLogisticModel instance to a binary file using the provided writer function.

        Parameters
        ----------
        fw : Callable[[bytes], None]
            A function that takes bytes as input and writes them to a file.

        Returns
        -------
        None
        """
        # Write the configuration as JSON5 string
        cfg_json5 = json5.dumps(self.cfg.model_dump(), indent=4, default=str)
        cfg_json5_bytes = cfg_json5.encode('utf-8')
        fw(struct.pack('I', len(cfg_json5_bytes)))
        fw(cfg_json5_bytes)

        # First writes the information if the attributes are populated or are None.
        # 1 means populated, 0 means None.
        flag_rids = 0
        flag_pattern_w_ranges = 0
        flag_iwmap_pattern_offset = 0
        flag_iwmap = 0
        flag_wmap = 0
        flag_wmap_fallback = 0
        flag_X = 0

        if self.rids is not None:
            flag_rids = 1
        if self.pattern_w_ranges is not None:
            flag_pattern_w_ranges = 1
        if self.iwmap_pattern_offset is not None:
            flag_iwmap_pattern_offset = 1
        if self.iwmap is not None:
            flag_iwmap = 1
        if self.wmap is not None:
            flag_wmap = 1
        if self.wmap_fallback is not None:
            flag_wmap_fallback = 1
        if self.X is not None:
            flag_X = 1

        fw(struct.pack('I', flag_rids))
        fw(struct.pack('I', flag_pattern_w_ranges))
        fw(struct.pack('I', flag_iwmap_pattern_offset))
        fw(struct.pack('I', flag_iwmap))
        fw(struct.pack('I', flag_wmap))
        fw(struct.pack('I', flag_wmap_fallback))
        fw(struct.pack('I', flag_X))
        
        if flag_rids == 1:
            self.rids.write_core_object_data(fw)

        if flag_pattern_w_ranges == 1:
            rows, cols = self.pattern_w_ranges.shape
            fw(struct.pack('QQ', rows, cols))
            fw(self.pattern_w_ranges.tobytes())

        if flag_iwmap_pattern_offset == 1:
            num_elements = self.iwmap_pattern_offset.size
            fw(struct.pack('Q', num_elements))
            fw(self.iwmap_pattern_offset.tobytes())

        if flag_iwmap == 1:
            num_elements = self.iwmap.size
            fw(struct.pack('Q', num_elements))
            fw(self.iwmap.tobytes())

        if flag_wmap == 1:
            rows, cols = self.wmap.shape
            fw(struct.pack('QQ', rows, cols))
            fw(self.wmap.tobytes())

        if flag_wmap_fallback == 1:
            rows, cols = self.wmap_fallback.shape
            fw(struct.pack('QQ', rows, cols))
            fw(self.wmap_fallback.tobytes())

        if flag_X == 1:
            rows, cols = self.X.shape
            fw(struct.pack('QQ', rows, cols))
            fw(self.X.tobytes())

    def store_to_file(self, filename: str | Path) -> None:
        """
        Saves the ReversiLogisticModel instance to a binary file and calculates the SHA3-256 checksum.

        Parameters
        ----------
        filename : str | Path
            The name of the file in which to save the data.

        Returns
        -------
        None
        """
        if not isinstance(filename, (str, Path)):
            raise TypeError('Argument filename is not an instance of str or Path')

        filename = Path(filename)

        # Create a SHA3-256 hash object
        sha3_256_hash = hashlib.sha3_256()

        with open(filename, 'wb') as f:
            fw = fun_builder_write_and_hash(f, sha3_256_hash)
            fw((self.fqcn + '\n').encode('utf-8'))
            self.write_core_object_data(fw)

        # Calculate the SHA3-256 checksum
        checksum = sha3_256_hash.hexdigest()

        # Write the checksum to a separate file with the same name and ".SHA3-256" suffix
        checksum_filename = filename.with_name(filename.name + ".SHA3-256")
        with open(checksum_filename, 'w') as checksum_file:
            checksum_file.write(checksum)

    @classmethod
    def load_from_file(cls: type[Self], filename: str | Path, checksum: bool = True) -> Self:
        """
        Loads a ReversiLogisticModel instance from a binary file.

        Parameters
        ----------
        filename : str | Path
            The name of the file from which to load the data.
        checksum : bool, optional
            Whether to verify the SHA3-256 checksum of the file. Default is True.

        Returns
        -------
        ReversiLogisticModel
            An instance of ReversiLogisticModel containing the loaded data.

        Raises
        ------
        FileNotFoundError
            If the checksum file is not found when checksum verification is enabled.
        ValueError
            If the calculated checksum does not match the stored checksum.
        EOFError
            If data is malformed or truncated.

        Notes
        -----
        - The method reads the fully qualified class name and verifies it matches the expected one.
        - It reads the configuration as a JSON5 string and loads it using ReversiLogisticModelConfig.
        - It reads and loads the rds and rids attributes using the appropriate methods.
        - It reads and loads other attributes (pattern_w_ranges, iwmap_pattern_offset, iwmap, wmap, wmap_fallback, X) if present.
        - It creates a new instance of ReversiLogisticModel without calling the constructor (__new__) and assigns the loaded attributes to it.
        - Finally, it returns the created instance of ReversiLogisticModel.
        """
        if not isinstance(filename, (str, Path)):
            raise TypeError('Argument filename is not an instance of str or Path')

        filename = Path(filename)
        
        if checksum:
            verify_checksum(filename)

        with open(filename, 'rb') as f:
            # Read the fully qualified class name.
            fqcn = f.readline().decode('utf-8').strip()
            expected_fqcn = 'twolm.rlmodel.ReversiLogisticModel'
            if fqcn != expected_fqcn:
                raise ValueError(f"The read fqcn {fqcn} does not match the expected one {expected_fqcn}.")

            # Read the configuration as JSON5 string
            cfg_len = struct.unpack('I', f.read(4))[0]
            cfg_json5_bytes = f.read(cfg_len)
            cfg_json5 = cfg_json5_bytes.decode('utf-8')
            cfg = ReversiLogisticModelConfig(**json5.loads(cfg_json5))

            model = ReversiLogisticModel(cfg)

            # 1. Read the flags (4 byte per flag, using 'I' as per write logic)
            # Total expected: 7 * 4 bytes = 28 bytes
            flag_count = 7
            flag_bytes = 4 * flag_count
            flags_raw = f.read(flag_bytes)
            if len(flags_raw) < flag_bytes:
                raise EOFError("Failed to read header flags: File is truncated or corrupted.")
            
            # Unpack the 5 unsigned integers
            flags = struct.unpack('IIIIIII', flags_raw)
            f_rids, f_pattern_w_ranges, f_iwmap_pattern_offset, f_iwmap, f_wmap, f_wmap_fallback, f_X = flags

            if f_rids == 1:
                model.rids = RegabIndexedDataSet.read_core_object_data(f)
            else:
                model.rids = None

            if f_pattern_w_ranges == 1:
                dims = f.read(16) # QQ
                if len(dims) < 16: raise EOFError("Truncated dimensions for 'pattern_w_ranges'")
                rows, cols = struct.unpack('QQ', dims)
                data = np.fromfile(f, dtype=np.int64, count=rows*cols)
                model.pattern_w_ranges = data.reshape((rows, cols)).copy()
            else:
                model.pattern_w_ranges = None

            if f_iwmap_pattern_offset == 1:
                dims = f.read(8) # Q
                if len(dims) < 8: raise EOFError("Truncated dimensions for 'iwmap_pattern_offset'")
                num_elements = struct.unpack('Q', dims)[0]
                data = np.fromfile(f, dtype=np.uint32, count=num_elements)
                model.iwmap_pattern_offset = data.copy()
            else:
                model.iwmap_pattern_offset = None

            if f_iwmap == 1:
                dims = f.read(8) # Q
                if len(dims) < 8: raise EOFError("Truncated dimensions for 'iwmap'")
                num_elements = struct.unpack('Q', dims)[0]
                data = np.fromfile(f, dtype=np.int64, count=num_elements)
                model.iwmap = data.copy()
            else:
                model.iwmap = None

            if f_wmap == 1:
                dims = f.read(16) # QQ
                if len(dims) < 16: raise EOFError("Truncated dimensions for 'wmap'")
                rows, cols = struct.unpack('QQ', dims)
                data = np.fromfile(f, dtype=np.int64, count=rows*cols)
                model.wmap = data.reshape((rows, cols)).copy()
            else:
                model.wmap = None

            if f_wmap_fallback == 1:
                dims = f.read(16) # QQ
                if len(dims) < 16: raise EOFError("Truncated dimensions for 'wmap_fallback'")
                rows, cols = struct.unpack('QQ', dims)
                data = np.fromfile(f, dtype=np.int64, count=rows*cols)
                model.wmap_fallback = data.reshape((rows, cols)).copy()
            else:
                model.wmap_fallback = None

            if f_X == 1:
                dims = f.read(16) # QQ
                if len(dims) < 16: raise EOFError("Truncated dimensions for 'X'")
                rows, cols = struct.unpack('QQ', dims)
                data = np.fromfile(f, dtype=np.uint32, count=rows*cols)
                model.X = data.reshape((rows, cols)).copy()
            else:
                model.X = None

            return model
