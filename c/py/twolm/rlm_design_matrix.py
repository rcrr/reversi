#
# rlm_design_matrix.py
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

# twolm/rlm_design_matrix.py
from __future__ import annotations

from typing import TYPE_CHECKING
from pathlib import Path

import numpy as np
from pydantic import validate_call, ConfigDict

from twolm import binio

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['ReversiLogisticModelDesignMatrix',
           'rlm_design_matrix_store_to_file',
           'rlm_design_matrix_load_from_file',
           'rlm_design_matrix_compute',
           'rlm_design_matrix_is_cache_consistent']


class ReversiLogisticModelDesignMatrix:
    """Represents the computed Design Matrix (X)."""
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self, feature_set_hash: str, cut_off: int, X: np.ndarray):
        self.feature_set_hash = feature_set_hash
        self.cut_off = cut_off
        self.X = X


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_design_matrix_store_to_file(dm: ReversiLogisticModelDesignMatrix, filename: str | Path) -> None:
    """Saves the Design Matrix instance to a binary file."""
    filename = Path(filename)
    description = "ReversiLogisticModelDesignMatrix binary data file"
    version = 1

    with binio.BinaryWriter(filename) as w:
        w.write_header(description, version)
        w.write_string(dm.feature_set_hash)
        w.write_i32(dm.cut_off)
        w.write_array(dm.X)


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_design_matrix_load_from_file(filename: str | Path, checksum: bool = True) -> ReversiLogisticModelDesignMatrix:
    """Loads a Design Matrix instance from a binary file."""
    if checksum:
        if not binio.verify_sha3_256_sidecar(filename):
            raise RuntimeError("The checksum file signature doesn't match with current file.")

    expected_description = "ReversiLogisticModelDesignMatrix binary data file"
    expected_version = 1

    with binio.BinaryReader(filename) as r:
        description, version = r.read_header()
        if description != expected_description:
            raise RuntimeError(f"The file is not a proper {expected_description}.")
        if version != expected_version:
            raise RuntimeError(f"File version mismatch: found {version}, expected {expected_version}")

        feature_set_hash = r.read_string()
        cut_off = r.read_i32()
        X = r.read_array()

    return ReversiLogisticModelDesignMatrix(
        feature_set_hash=feature_set_hash,
        cut_off=cut_off,
        X=X
    )


def rlm_design_matrix_compute(ctx: "RLMContext") -> ReversiLogisticModelDesignMatrix:
    """Computes the Design Matrix X from principal indexes and iwmap."""
    pindexes = ctx.rlm_indexes.indexes
    
    n_instances_per_feature = [f.n_instances for f in ctx.feature_set.features]
    col_offsets = np.repeat(ctx.iwmap_feature_offset[:-1], n_instances_per_feature)
    pindexes_with_offsets = pindexes + col_offsets

    if np.any(ctx.iwmap[pindexes_with_offsets] == -1):
        raise ValueError("It should never happen, pindexes is referring an invalid value.")

    if not pindexes.size == np.count_nonzero(ctx.iwmap[pindexes_with_offsets] >= 0):
        raise ValueError("It should never happen, mismatch between indexes and valid values.")
    
    X = np.empty(pindexes.shape, dtype=np.uint32)
    np.take(ctx.iwmap, pindexes_with_offsets, out=X)
    
    return ReversiLogisticModelDesignMatrix(
        feature_set_hash=ctx.feature_set.hash,
        cut_off=ctx.cfg.stat_model.frequency_cut_off,
        X=X
    )


def rlm_design_matrix_is_cache_consistent(ctx: "RLMContext", dm: ReversiLogisticModelDesignMatrix) -> bool:
    """Validates the cache by checking feature_set_hash and cut_off."""
    is_hash_consistent = dm.feature_set_hash == ctx.feature_set.hash
    is_cut_off_consistent = dm.cut_off == ctx.cfg.stat_model.frequency_cut_off
    return is_hash_consistent and is_cut_off_consistent
