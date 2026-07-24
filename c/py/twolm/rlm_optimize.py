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

from pathlib import Path
from typing import List, Tuple, Dict, Any, TYPE_CHECKING
import numpy as np

from twolm import binio

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['save_optimization_checkpoint',
           'load_optimization_checkpoint',
           'is_optimization_cache_consistent']



def save_optimization_checkpoint(filepath: Path, 
                                 start_max_iters: int, 
                                 start_m: int, 
                                 is_converged: bool, 
                                 stop_reason: str, 
                                 iterations_done: int, 
                                 final_f: float, 
                                 final_g_norm: float, 
                                 w: np.ndarray, 
                                 sl: List[np.ndarray], 
                                 yl: List[np.ndarray], 
                                 rho: List[float]) -> None:
    """Saves the L-BFGS state and metadata to a binary checkpoint file."""
    with binio.BinaryWriter(filepath) as bw:
        bw.write_header("RGLM Optimization Checkpoint", 1)
        bw.write_i32(start_max_iters)
        bw.write_i32(start_m)
        bw.write_i32(1 if is_converged else 0)
        bw.write_string(stop_reason)
        bw.write_i32(iterations_done)
        bw.write_f32(final_f)
        bw.write_f32(final_g_norm)
        
        bw.write_i32(len(sl))
        bw.write_array(w)
        
        for s, y, r in zip(sl, yl, rho):
            bw.write_array(s)
            bw.write_array(y)
            bw.write_f64(r)


def load_optimization_checkpoint(filepath: Path) -> Dict[str, Any]:
    """Loads the L-BFGS state from a binary checkpoint file."""
    with binio.BinaryReader(filepath) as br:
        # 1. Read header properly!
        description, version = br.read_header()
        if description != "RGLM Optimization Checkpoint":
            raise RuntimeError(f"The file is not a proper optimization checkpoint. Found: {description}")
        if version != 1:
            raise RuntimeError(f"Checkpoint version mismatch: found {version}, expected 1")
        
        # 2. Read the metadata
        start_max_iters = br.read_i32()
        start_m = br.read_i32()
        is_converged = bool(br.read_i32())
        stop_reason = br.read_string()
        iterations_done = br.read_i32()
        final_f = br.read_f32()
        final_g_norm = br.read_f32()
        
        # 3. Read the L-BFGS memory state
        len_mem = br.read_i32()
        w = br.read_array()
        
        sl, yl, rho = [], [], []
        for _ in range(len_mem):
            sl.append(br.read_array())
            yl.append(br.read_array())
            rho.append(br.read_f64())
            
    return {
        'start_max_iters': start_max_iters,
        'start_m': start_m,
        'is_converged': is_converged,
        'stop_reason': stop_reason,
        'iterations_done': iterations_done,
        'final_f': final_f,
        'final_g_norm': final_g_norm,
        'w': w,
        'sl': sl,
        'yl': yl,
        'rho': rho
    }


def is_optimization_cache_consistent(ctx: "RLMContext", checkpoint: Dict[str, Any]) -> bool:
    """Validates the checkpoint against current configuration."""
    if checkpoint['start_max_iters'] != ctx.cfg.optimization.max_iters:
        return False
    if checkpoint['start_m'] != ctx.cfg.optimization.m:
        return False
    return True
