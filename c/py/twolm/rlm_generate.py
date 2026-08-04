#
# rlm_generate.py
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

# twolm/rlm_generate.py
from __future__ import annotations

import hashlib
import numpy as np

from typing import TYPE_CHECKING
from pathlib import Path
from pydantic import validate_call, ConfigDict

from twolm import binio
from twolm.state_machine import Relevance

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['ReversiLogisticModelDenseWeights',
           'rlm_generate_store_to_file',
           'rlm_generate_load_from_file',
           'compute_w_dense',
           'rlm_generate_is_cache_consistent']



class ReversiLogisticModelDenseWeights:
    """Wrapper for the dense weight vector and its metadata."""
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self, w_checksum: str, w_dense: np.ndarray):
        self.w_checksum = w_checksum
        self.w_dense = w_dense

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def compute_sha3_256_hash(self) -> str:
        hasher = hashlib.sha3_256()
        hasher.update(self.w_dense.tobytes())
        sha3_256_hash = hasher.hexdigest()
        return sha3_256_hash


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_generate_store_to_file(dw: ReversiLogisticModelDenseWeights, filename: str | Path) -> None:
    filename = Path(filename)
    with binio.BinaryWriter(filename) as w:
        w.write_header("RGLM Dense Weights binary data file", 1)
        w.write_string(dw.w_checksum)
        w.write_array(dw.w_dense)
    return


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_generate_load_from_file(filename: str | Path, checksum: bool = True) -> ReversiLogisticModelDenseWeights:
    if checksum:
        if not binio.verify_sha3_256_sidecar(filename):
            raise RuntimeError("Checksum mismatch.")
            
    with binio.BinaryReader(filename) as r:
        description, version = r.read_header()
        if description != "RGLM Dense Weights binary data file":
            raise RuntimeError(f"Wrong file format: {description}")
        w_checksum = r.read_string()
        w_dense = r.read_array()
        
    return ReversiLogisticModelDenseWeights(w_checksum, w_dense)


def rlm_generate_is_cache_consistent(ctx: "RLMContext", dw: ReversiLogisticModelDenseWeights) -> bool:
    return dw.w_checksum == ctx.w_checksum


def compute_w_dense(ctx: "RLMContext") -> ReversiLogisticModelDenseWeights:
    """
    Computes the dense weight vector (w_dense) of size K (all theoretical configurations).
    Unseen configurations and fallbacks are handled according to feature type:
    - Intercept: Direct mapping.
    - Pattern (Categorical): Symmetric configurations inherit the weight of their Principal Index. 
      If the Principal is unseen/fallback, they inherit the weighted mean of the pattern's weights.
    - Mobility (Ordinal): Internal missing values are linearly interpolated, external ones clamped.
    """
    features = ctx.feature_set.features
    feature_w_ranges = ctx.feature_w_ranges
    iwmap = ctx.iwmap
    iwmap_feature_offset = ctx.iwmap_feature_offset
    wmap = ctx.wmap
    w = ctx.w
    w_checksum = ctx.w_checksum
    
    K = iwmap_feature_offset[-1]
    w_dense = np.zeros(K, dtype=np.float32)

    for fid, f in enumerate(features):
        start_idx = iwmap_feature_offset[fid]
        end_idx = iwmap_feature_offset[fid + 1]
        f_iwmap = iwmap[start_idx:end_idx]
        
        fallback, w_min, w_max = feature_w_ranges[fid]
        f_w = w[w_min : w_max + 1]
        
        # Extract frequencies for this feature from wmap
        # wmap columns: [fid, config_id, frequency]
        f_wmap_mask = wmap[:, 0] == fid
        f_wmap = wmap[f_wmap_mask]
        
        if f.category == 0:  # Intercept
            w_dense[start_idx:end_idx] = f_w[0]
                
        elif f.category == 1:  # Mobility
            # To find the true "above cutoff" configurations, we must look at wmap, 
            # because iwmap points rare configs to the fallback index.
            f_wmap_mask = wmap[:, 0] == fid
            f_wmap = wmap[f_wmap_mask]
            
            # True configs are those with config_id >= 0 in wmap
            true_configs_mask = f_wmap[:, 1] >= 0
            known_configs = f_wmap[true_configs_mask, 1].astype(np.int64)
            
            if len(known_configs) > 0:
                # Get the specific weights for these true configs via iwmap
                w_indices = f_iwmap[known_configs]
                known_weights = w[w_indices]

                # Interpolate over the full range of possible configurations (0 to n_configurations-1)
                # np.interp automatically clamps external values to the nearest known boundary.
                all_configs = np.arange(f.n_configurations)
                w_dense[start_idx : start_idx + f.n_configurations] = np.interp(
                    all_configs, known_configs, known_weights
                ).astype(np.float32)
            else:
                ctx.log_event(Relevance.WARN, f"Mobility {f.name} (id:{fid}) has no seen configurations. Weights are zero.")
                
        elif f.category == 2:  # Pattern
            # We only want configurations that are NOT fallback (config_id >= 0)
            # and that were actually assigned a weight (seen in dataset, above cutoff)
            true_configs_mask = f_wmap[:, 1] >= 0
            true_freqs = f_wmap[true_configs_mask, 2]
            
            # Get the weights for these true configs via iwmap
            true_config_ids = f_wmap[true_configs_mask, 1].astype(np.int64)
            w_indices = f_iwmap[true_config_ids]
            true_weights = w[w_indices]
            
            if len(true_weights) > 0 and np.sum(true_freqs) > 0:
                weighted_mean = np.dot(true_weights, true_freqs) / np.sum(true_freqs)
            else:
                weighted_mean = 0.0
                
            ctx.log_event(Relevance.INFO, f"Pattern {f.name} (id:{fid}) weighted mean is {weighted_mean:.4f}.")
            
            if fallback != -1:
                fallback_w_val = w[fallback]
                ctx.log_event(Relevance.INFO, f"Pattern {f.name} (id:{fid}) fallback weight is {fallback_w_val:.4f}.")

            # --- Populate w_dense propagating Principal Index weights ---
            
            # 1. Get all theoretical configuration IDs for this pattern (MUST BE uint32 for Pydantic validation)
            all_configs = np.arange(f.n_configurations, dtype=np.uint32)
            
            # 2. Map every config to its Principal Index
            principal_configs = f.ref.convert_to_principal_index(all_configs)
            
            # 3. Find the w_idx for each of these principal configurations
            # Cast back to int64 for safe indexing into f_iwmap
            principal_w_indices = f_iwmap[principal_configs.astype(np.int64)]
            
            # 4. Initialize the dense weights for this feature with the weighted_mean
            f_w_dense = np.full(f.n_configurations, weighted_mean, dtype=np.float32)
            
            # 5. Where the principal configuration was actually computed (not fallback, not unseen)
            is_computed = (principal_w_indices >= 0) & (principal_w_indices != fallback)
            
            # 6. Propagate the principal weight to all symmetric configurations
            f_w_dense[is_computed] = w[principal_w_indices[is_computed].astype(np.int64)]
            
            w_dense[start_idx:end_idx] = f_w_dense
            
        else:  # Undefined feature
            raise RuntimeError(f"The feature category is unknown, f.category = {f.category}")

    return ReversiLogisticModelDenseWeights(w_checksum, w_dense)
