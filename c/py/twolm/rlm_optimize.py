#
# rglm.py
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

# twolm/rlm_optimize.py
from __future__ import annotations

import numpy as np

from pathlib import Path
from typing import List, Tuple, Dict, Any, TYPE_CHECKING

from twolm import binio

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['save_optimization_checkpoint',
           'load_optimization_checkpoint',
           'is_optimization_cache_consistent']



#: Unlike the other layers, where the cache—when present—is organized using a class and a data object,
#: the OPTIMIZE layer cache (called a checkpoint) consists of a dictionary.

def save_optimization_checkpoint(filepath: Path, 
                                 max_iters: int, 
                                 m: int, 
                                 converged: bool, 
                                 reason: str, 
                                 iters: int, 
                                 f: float, 
                                 g_norm: float, 
                                 w: np.ndarray, 
                                 sl: List[np.ndarray], 
                                 yl: List[np.ndarray], 
                                 rho: List[float]) -> None:
    """Saves the L-BFGS state and metadata to a binary checkpoint file."""
    with binio.BinaryWriter(filepath) as bw:
        bw.write_header("RGLM Optimization Checkpoint", 1)
        bw.write_i32(max_iters)
        bw.write_i32(m)
        bw.write_i32(1 if converged else 0)
        bw.write_string(reason)
        bw.write_i32(iters)
        bw.write_f32(f)
        bw.write_f32(g_norm)
        
        bw.write_i32(len(sl))
        bw.write_array(w)
        
        # Atomically serialize the L-BFGS history vectors chronologically.
        # zip() pairs each displacement vector (s) with its corresponding 
        # gradient change (y) and scaling factor (rho) to write them 
        # sequentially into the binary file for a seamless warm start.
        for s, y, r in zip(sl, yl, rho):
            bw.write_array(s)
            bw.write_array(y)
            bw.write_f64(r)


def load_optimization_checkpoint(filepath: Path) -> Dict[str, Any]:
    """
    Loads the L-BFGS state from a binary checkpoint file.
    """
    with binio.BinaryReader(filepath) as br:
        # 1. Read header properly!
        description, version = br.read_header()
        if description != "RGLM Optimization Checkpoint":
            raise RuntimeError(f"The file is not a proper optimization checkpoint. Found: {description}")
        if version != 1:
            raise RuntimeError(f"Checkpoint version mismatch: found {version}, expected 1")
        
        # 2. Read the metadata
        max_iters = br.read_i32()
        m = br.read_i32()
        converged = bool(br.read_i32())
        reason = br.read_string()
        iters = br.read_i32()
        f = br.read_f32()
        g_norm = br.read_f32()
        
        # 3. Read the L-BFGS memory state
        len_mem = br.read_i32()
        w = br.read_array()
        
        sl, yl, rho = [], [], []
        for _ in range(len_mem):
            sl.append(br.read_array())
            yl.append(br.read_array())
            rho.append(br.read_f64())
            
    return {
        'max_iters': max_iters,
        'm': m,
        'converged': converged,
        'reason': reason,
        'iters': iters,
        'f': f,
        'g_norm': g_norm,
        'w': w,
        'sl': sl,
        'yl': yl,
        'rho': rho
    }


def is_optimization_cache_consistent(ctx: "RLMContext", checkpoint: Dict[str, Any]) -> bool:
    """Validates the checkpoint against current configuration."""
    if checkpoint['max_iters'] != ctx.cfg.optimization.max_iters:
        return False
    if checkpoint['m'] != ctx.cfg.optimization.m:
        return False
    return True
