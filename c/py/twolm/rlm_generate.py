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

from typing import TYPE_CHECKING
from pathlib import Path
import numpy as np
from pydantic import validate_call, ConfigDict

from twolm import binio
from twolm.enums import Relevance

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
    def __init__(self, feature_set_hash: str, w_dense: np.ndarray):
        self.feature_set_hash = feature_set_hash
        self.w_dense = w_dense


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_generate_store_to_file(dw: ReversiLogisticModelDenseWeights, filename: str | Path) -> None:
    filename = Path(filename)
    with binio.BinaryWriter(filename) as w:
        w.write_header("RGLM Dense Weights binary data file", 1)
        w.write_string(dw.feature_set_hash)
        w.write_array(dw.w_dense)

@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_generate_load_from_file(filename: str | Path, checksum: bool = True) -> ReversiLogisticModelDenseWeights:
    if checksum:
        if not binio.verify_sha3_256_sidecar(filename):
            raise RuntimeError("Checksum mismatch.")
            
    with binio.BinaryReader(filename) as r:
        description, version = r.read_header()
        if description != "RGLM Dense Weights binary data file":
            raise RuntimeError(f"Wrong file format: {description}")
        feature_set_hash = r.read_string()
        w_dense = r.read_array()
        
    return ReversiLogisticModelDenseWeights(feature_set_hash, w_dense)

def rlm_generate_is_cache_consistent(ctx: "RLMContext", dw: ReversiLogisticModelDenseWeights) -> bool:
    return dw.feature_set_hash == ctx.feature_set.hash


def compute_w_dense(ctx: "RLMContext") -> ReversiLogisticModelDenseWeights:
    """
    Computes the dense weight vector (w_dense) of size K (all theoretical configurations).
    Unseen configurations and fallbacks are handled according to feature type:
    - Intercept: Direct mapping.
    - Pattern (Categorical): Replaced with the weighted mean of the pattern's weights.
    - Mobility (Ordinal): Internal missing values are linearly interpolated, external ones clamped.
    """
    features = ctx.feature_set.features
    w_ranges = ctx.feature_w_ranges
    iwmap = ctx.iwmap
    iwmap_feature_offset = ctx.iwmap_feature_offset
    wmap = ctx.wmap
    w = ctx.w
    
    K = iwmap_feature_offset[-1]
    w_dense = np.zeros(K, dtype=np.float32)
    
    # Calculate mean of Z for Intercept sanity check
    z_mean = np.mean(ctx.z) if ctx.z is not None else 0.5

    for fid, f in enumerate(features):
        start_idx = iwmap_feature_offset[fid]
        end_idx = iwmap_feature_offset[fid + 1]
        f_iwmap = iwmap[start_idx:end_idx]
        
        fallback, w_min, w_max = w_ranges[fid]
        f_w = w[w_min : w_max + 1]
        
        # Extract frequencies for this feature from wmap
        # wmap columns: [fid, config_id, frequency]
        f_wmap_mask = wmap[:, 0] == fid
        f_wmap = wmap[f_wmap_mask]
        
        # Sanity checks for Patterns
        if f.category == 2:  # Pattern
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
                
            if not np.isclose(weighted_mean, 0.0, atol=0.1):
                ctx.log_event(Relevance.WARN, f"Pattern {f.name} (id:{fid}) weighted mean is {weighted_mean:.4f} (expected ~0.0). Intercept might not be capturing the full mean.")
            
            if fallback != -1:
                fallback_w_val = w[fallback]
                if not np.isclose(fallback_w_val, weighted_mean, atol=0.5):
                    ctx.log_event(Relevance.WARN, f"Pattern {f.name} (id:{fid}) fallback weight is {fallback_w_val:.4f}, far from weighted mean {weighted_mean:.4f}.")

            # Populate w_dense for Pattern
            seen_mask = f_iwmap >= 0
            w_dense[start_idx:end_idx][seen_mask] = w[f_iwmap[seen_mask]]
            w_dense[start_idx:end_idx][~seen_mask] = weighted_mean
            if fallback != -1:
                w_dense[start_idx:end_idx][f_iwmap == fallback] = weighted_mean

        elif f.category == 0:  # Intercept
            w_dense[start_idx:end_idx] = f_w[0]
            if not np.isclose(f_w[0], z_mean, atol=0.1):
                ctx.log_event(Relevance.WARN, f"Intercept weight is {f_w[0]:.4f}, but mean of Z is {z_mean:.4f}.")
                
        elif f.category == 1:  # Mobility
            # Find which configurations were actually seen and have a direct weight
            seen_mask = f_iwmap >= 0
            known_configs = np.where(seen_mask)[0]
            known_weights = w[f_iwmap[seen_mask]]
            
            if len(known_configs) > 0:
                # Fill known values
                w_dense[start_idx + known_configs] = known_weights
                
                # Interpolate missing internal values
                if len(known_configs) > 1:
                    all_configs = np.arange(f.n_configurations)
                    w_dense[start_idx : start_idx + f.n_configurations] = np.interp(
                        all_configs, known_configs, known_weights
                    ).astype(np.float32) # Ensure float32 output from interp
                else:
                    # Only one config seen, clamp everything to it
                    w_dense[start_idx : start_idx + f.n_configurations] = known_weights[0]
            else:
                ctx.log_event(Relevance.WARN, f"Mobility {f.name} (id:{fid}) has no seen configurations. Weights are zero.")

    return ReversiLogisticModelDenseWeights(ctx.feature_set.hash, w_dense)

