#
# rlm_wmaps.py
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

# twolm/rlm_wmaps.py
from __future__ import annotations

from typing import TYPE_CHECKING
from pathlib import Path
from itertools import accumulate

import numpy as np
from pydantic import validate_call, ConfigDict

from twolm import binio

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['ReversiLogisticModelWMaps',
           'rlm_wmaps_store_to_file',
           'rlm_wmaps_load_from_file',
           'rlm_wmaps_compute',
           'rlm_wmaps_is_cache_consistent']


class ReversiLogisticModelWMaps:
    """
    Represents the computed Weight Maps (WMaps) for the model.
    Holds the arrays mapping feature configurations to the weight vector W.

    Attributes
    ----------
    feature_set_hash : str
        The hash of the feature set used to compute the indexes.
    cut_off : int
        The frequency cut-off threshold used to filter rare configurations.
    w_ranges : np.ndarray
        shape(F, 3). Lists for each feature, ordered by fid (feature index):
        - fallback: the index value of w used for the fallback (cut-off) configurations.
        - w_min: the first index value of w assigned to the feature (equal to fallback when fallback is not -1).
        - w_max: the last index value of w assigned to the feature.
    iwmap_feature_offset : np.ndarray
        shape(F + 1,). Gives the position of a feature in the iwmap array.
        iwmap_feature_offset[fid] gives the position of configuration zero for the feature identified by fid.
    iwmap : np.ndarray
        shape(K,). Assigns to each possible configuration the index of w. -1 when the configuration is not found
        in the dataset.
        Given fid, and feature_conf_id the formula:
        iwmap[iwmap_feature_offset[fid] + feature_conf_id] returns the index of w related to (fid, feature_conf_id).
    wmap : np.ndarray
        shape(W, 3). Given the w index returns the feature index, the configuration index, and the frequency configuration.
        fid, feature_conf_id, feature_conf_frequency = wmap[w_index]
        The value -1 is used to identify weight assigned to fall back configurations. One per feature, if required.
    wmap_fallback : np.ndarray
        shape(F_cut, 3). Contains the entries of wmap being excluded by the cut-off. It has:
        - fid: feature id
        - feature_conf_id: index of the feature configuration excluded by the cut-off rule
        - feature_conf_frequency: the frequency of the feature configuration within the dataset
    w : np.ndarray
        shape(W,). The model weights vector, initialized to zeros.

    Where:
        F is the count of features defined by the model (Intercept, Mobilities, Patterns).
        K is the count of all feature configurations (K = sum([f.n_configurations for f in features])).
        W is the count of all model weights.
        F_cut is the count of all cut-off configurations.
    """
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self,
                 feature_set_hash: str,
                 cut_off: int,
                 w_ranges: np.ndarray,
                 iwmap_feature_offset: np.ndarray,
                 iwmap: np.ndarray,
                 wmap: np.ndarray,
                 wmap_fallback: np.ndarray,
                 w: np.ndarray):
        self.feature_set_hash = feature_set_hash
        self.cut_off = cut_off
        self.w_ranges = w_ranges
        self.iwmap_feature_offset = iwmap_feature_offset
        self.iwmap = iwmap
        self.wmap = wmap
        self.wmap_fallback = wmap_fallback
        self.w = w


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_wmaps_store_to_file(wmaps: ReversiLogisticModelWMaps, filename: str | Path) -> None:
    """Saves the WMaps instance to a binary file."""
    filename = Path(filename)
    description = "ReversiLogisticModelWMaps binary data file"
    version = 1

    with binio.BinaryWriter(filename) as w:
        w.write_header(description, version)
        w.write_string(wmaps.feature_set_hash)
        w.write_i32(wmaps.cut_off)
        w.write_array(wmaps.w_ranges)
        w.write_array(wmaps.iwmap_feature_offset)
        w.write_array(wmaps.iwmap)
        w.write_array(wmaps.wmap)
        w.write_array(wmaps.wmap_fallback)
        w.write_array(wmaps.w)


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_wmaps_load_from_file(filename: str | Path, checksum: bool = True) -> ReversiLogisticModelWMaps:
    """Loads a WMaps instance from a binary file."""
    if checksum:
        if not binio.verify_sha3_256_sidecar(filename):
            raise RuntimeError("The checksum file signature doesn't match with current file.")

    expected_description = "ReversiLogisticModelWMaps binary data file"
    expected_version = 1

    with binio.BinaryReader(filename) as r:
        description, version = r.read_header()
        if description != expected_description:
            raise RuntimeError(f"The file is not a proper {expected_description}.")
        if version != expected_version:
            raise RuntimeError(f"File version mismatch: found {version}, expected {expected_version}")

        feature_set_hash = r.read_string()
        cut_off = r.read_i32()
        
        w_ranges = r.read_array()
        iwmap_feature_offset = r.read_array()
        iwmap = r.read_array()
        wmap = r.read_array()
        wmap_fallback = r.read_array()
        w = r.read_array()

    return ReversiLogisticModelWMaps(
        feature_set_hash=feature_set_hash,
        cut_off=cut_off,
        w_ranges=w_ranges,
        iwmap_feature_offset=iwmap_feature_offset,
        iwmap=iwmap,
        wmap=wmap,
        wmap_fallback=wmap_fallback,
        w=w
    )


def rlm_wmaps_compute(ctx: "RLMContext") -> ReversiLogisticModelWMaps:
    """
    Computes WMaps based on feature frequencies and the cut-off threshold.

    The method performs the following steps:
        1. Initializes the cut-off value from the configuration.
        2. Retrieves the list of features and their respective number of configurations.
        3. Computes the offset for each feature in the iwmap array using the cumulative sum of the number of configurations.
        4. Initializes the iwmap array with -1, indicating that no configuration is initially mapped.
        5. Iterates over each feature to compute the unique configurations and their frequencies.
        6. Determines the fallback configuration for each feature if any configuration's frequency is below the cut-off.
        7. Assigns unique w indices to configurations with frequencies above the cut-off.
        8. Updates the iwmap array with the computed w indices.
        9. Constructs the w_ranges array with fallback, w_min, and w_max values for each feature.
        10. Constructs the wmap array mapping each w index to the corresponding feature index, configuration index, and frequency.
        11. Constructs the wmap_fallback array containing configurations excluded by the cut-off.
        12. Initializes the weights vector w with zeros, having length equal to the total count of assigned weights (W).
    """
    cut_off = ctx.cfg.stat_model.frequency_cut_off
    features = ctx.feature_set.features
    
    # Extract configurations and explicitly cast to uint32 to prevent uint8 overflow 
    # from the underlying feature objects.
    f_num_configurations = np.array([f.n_configurations for f in features], dtype=np.uint32)
    
    # Compute the offset array using numpy's cumulative sum
    iwmap_feature_offset = np.zeros(len(features) + 1, dtype=np.uint32)
    iwmap_feature_offset[1:] = np.cumsum(f_num_configurations)
    
    K = iwmap_feature_offset[-1]
    
    iwmap = np.full(K, -1, dtype=np.int64)
    pindexes = ctx.rlm_indexes.indexes
    N = pindexes.shape[0]
    next_w = 0
    f_w_ranges = []
    wmap_blocks = []
    fallback_data = []
    
    col_offset = 0
    for fid, f in enumerate(features):
        # Extract the columns for this specific feature
        cols = list(range(col_offset, col_offset + f.n_instances))
        f_pindexes = pindexes[:, cols]
        flat_f_pindexes = f_pindexes.ravel()
        
        unique_values, counts = np.unique(flat_f_pindexes, return_counts=True)
        stats = np.column_stack((unique_values, counts))
        
        mask = stats[:, 1] < cut_off
        stats_filtered_below = stats[mask]
        stats_filtered_above = stats[~mask]
        
        w_indices = np.empty(len(stats), dtype=np.int64)
        if len(stats_filtered_below) > 0:
            fallback = next_w
            w_indices[mask] = fallback
            next_w += 1
            fallback_feature_configuration_is_present = True
        else:
            fallback = -1
            fallback_feature_configuration_is_present = False
            
        n_above = (~mask).sum()
        if n_above > 0:
            w_indices[~mask] = np.arange(next_w, next_w + n_above)
            next_w += n_above
            
        stats = np.column_stack((stats, w_indices))
        
        f_iwmap = iwmap[iwmap_feature_offset[fid]:iwmap_feature_offset[fid + 1]]
        f_iwmap[stats[:, 0].astype(np.int64)] = stats[:, 2]
        
        w_min = w_indices.min() if len(w_indices) > 0 else -1
        w_max = w_indices.max() if len(w_indices) > 0 else -1
        f_w_ranges.append({
            'fallback': fallback,
            'w_min': w_min,
            'w_max': w_max
        })
        
        num_w_feature = w_max - w_min + 1
        feature_w_mapping = np.empty((num_w_feature, 3), dtype=np.int64)
        feature_w_mapping[:, 0] = fid
        feature_w_mapping[:, 1] = -1
        
        if fallback_feature_configuration_is_present:
            fallback_freq = stats_filtered_below[:, 1].sum()
            fallback_rel_idx = fallback - w_min
            feature_w_mapping[fallback_rel_idx, 2] = fallback_freq
            
            for row in stats_filtered_below:
                fallback_data.append([fid, row[0], row[1]])
                
        above_mask = ~mask 
        if above_mask.any():
            w_above = stats[above_mask, 2]
            config_above = stats[above_mask, 0]
            freq_above = stats[above_mask, 1]
            relative_indices = (w_above - w_min).astype(np.int64)
            feature_w_mapping[relative_indices, 1] = config_above
            feature_w_mapping[relative_indices, 2] = freq_above
        wmap_blocks.append(feature_w_mapping)
        
        col_offset += f.n_instances
        
    wmap = np.vstack(wmap_blocks)
    fallback_details_table = np.array(fallback_data, dtype=np.int64)      
    if fallback_details_table.size == 0:
        fallback_details_table = fallback_details_table.reshape(0, 3)
    
    feature_w_ranges_arr = np.array([[m['fallback'], m['w_min'], m['w_max']] for m in f_w_ranges], dtype=np.int64)
    w = np.zeros(next_w, dtype=np.float32)
    
    return ReversiLogisticModelWMaps(
        feature_set_hash=ctx.feature_set.hash,
        cut_off=cut_off,
        w_ranges=feature_w_ranges_arr,
        iwmap_feature_offset=iwmap_feature_offset,
        iwmap=iwmap,
        wmap=wmap,
        wmap_fallback=fallback_details_table,
        w=w
    )


def rlm_wmaps_is_cache_consistent(ctx: "RLMContext", wmaps: ReversiLogisticModelWMaps) -> bool:
    """Validates the cache by checking feature_set_hash and cut_off."""
    is_hash_consistent = wmaps.feature_set_hash == ctx.feature_set.hash
    is_cut_off_consistent = wmaps.cut_off == ctx.cfg.stat_model.frequency_cut_off
    return is_hash_consistent and is_cut_off_consistent
