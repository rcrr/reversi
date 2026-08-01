#
# rlm_save.py
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

# twolm/rlm_save.py
from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Dict
from pathlib import Path
import numpy as np

from twolm.binio import BinaryWriter, BinaryReader
from twolm.feature import Feature, FeatureSet
from twolm.mobility import Mobility, MobilitySet
from twolm.pattern import Pattern, PatternSet

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['ModelWeights', 'read_model_weights_file', 'write_model_weights_file']



@dataclass
class ModelWeights:
    version: int
    name: str
    ec: int
    logit_clipping: float
    opt_info: Dict[str, Any]
    val_metrics: Dict[str, Any]
    feature_set: FeatureSet
    iwmap_feature_offset: np.ndarray
    w_dense: np.ndarray


def write_model_weights_file(ctx: "RLMContext", path: Path, compressed: bool = True) -> None:
    """
    Writes the model weights and metadata to a binary file using binio.
    """
    with BinaryWriter(path, compressed=compressed) as w:
        # 1. Header & Metadata
        w.write_header("RGLM_BINARY_WEIGHTS", version=1)
        w.write_string(ctx.cfg.name)
        w.write_u32(ctx.cfg.regab_data_set.ec)
        w.write_f32(ctx.cfg.stat_model.logit_clipping)

        # 2. Optimization Info
        opt_info = ctx.opt_info or {}
        w.write_string(opt_info.get('reason', 'N/A'))
        w.write_u32(opt_info.get('iters', 0))
        w.write_f64(opt_info.get('f', 0.0))

        # 3. Validation Metrics
        vld = ctx.vld_metrics or {}
        w.write_u32(vld.get('vld_samples', 0))
        w.write_f32(vld.get('vld_rmse_y', 0.0))
        w.write_f32(vld.get('vld_mae_y', 0.0))
        w.write_f32(vld.get('vld_loss', 0.0))

        # 4. FeatureSet
        fs = ctx.feature_set
        w.write_string(fs.name)
        w.write_u8(1 if fs.intercept else 0)

        # 5. MobilitySet
        if fs.mset:
            w.write_u8(1)
            w.write_string(fs.mset.name)
            w.write_u32(len(fs.mset.mobilities))
            for m in fs.mset.mobilities:
                w.write_string(m.name)
                w.write_u64(int(m.mask))
                w.write_u64(int(m.amask))
        else:
            w.write_u8(0)

        # 6. PatternSet
        if fs.pset:
            w.write_u8(1)
            w.write_string(fs.pset.name)
            w.write_u32(len(fs.pset.patterns))
            for p in fs.pset.patterns:
                w.write_string(p.name)
                w.write_u64(int(p.mask))
        else:
            w.write_u8(0)

        # 7. Core Inference Data
        w.write_array(ctx.iwmap_feature_offset)
        w.write_array(ctx.w_dense)


def read_model_weights_file(path: Path, compressed: bool = True) -> ModelWeights:
    """
    Reads a binary model weights file and returns a ModelWeights dataclass.
    """
    with BinaryReader(path, compressed=compressed) as r:
        header = r.read_header()
        # We could assert header.description == "RGLM_BINARY_WEIGHTS" here
        version = header.version
        
        name = r.read_string()
        ec = r.read_u32()
        logit_clipping = r.read_f32()

        opt_reason = r.read_string()
        opt_iters = r.read_u32()
        opt_f = r.read_f64()
        opt_info = {'reason': opt_reason, 'iters': opt_iters, 'f': opt_f}

        vld_samples = r.read_u32()
        vld_rmse_y = r.read_f32()
        vld_mae_y = r.read_f32()
        vld_loss = r.read_f32()
        val_metrics = {
            'vld_samples': vld_samples,
            'vld_rmse_y': vld_rmse_y,
            'vld_mae_y': vld_mae_y,
            'vld_loss': vld_loss
        }

        # Reconstruct FeatureSet
        fs_name = r.read_string()
        has_intercept = r.read_u8()
        intercept = Feature.new_intercept() if has_intercept else None

        has_mset = r.read_u8()
        mset = None
        if has_mset:
            mset_name = r.read_string()
            n_mob = r.read_u32()
            mobilities = []
            for _ in range(n_mob):
                m_name = r.read_string()
                m_mask = r.read_u64()
                m_amask = r.read_u64()
                mobilities.append(Mobility(m_name, m_mask, m_amask))
            mset = MobilitySet(mset_name, mobilities)

        has_pset = r.read_u8()
        pset = None
        if has_pset:
            pset_name = r.read_string()
            n_pat = r.read_u32()
            patterns = []
            for _ in range(n_pat):
                p_name = r.read_string()
                p_mask = r.read_u64()
                patterns.append(Pattern(p_name, p_mask))
            pset = PatternSet(pset_name, patterns)

        feature_set = FeatureSet(fs_name, intercept, mset, pset)

        # Core data
        iwmap_feature_offset = r.read_array()
        w_dense = r.read_array()

        return ModelWeights(
            version=version,
            name=name,
            ec=ec,
            logit_clipping=logit_clipping,
            opt_info=opt_info,
            val_metrics=val_metrics,
            feature_set=feature_set,
            iwmap_feature_offset=iwmap_feature_offset,
            w_dense=w_dense
        )
