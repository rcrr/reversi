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

import math
import numpy as np

from pathlib import Path
from typing import List, Tuple, Dict, Any, TYPE_CHECKING

from twolm import binio
from twolm.state_machine import Relevance

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['optimization_info_dict',
           'save_optimization_checkpoint',
           'load_optimization_checkpoint',
           'is_optimization_cache_consistent',
           'is_optimization_status_converged']



#: Unlike the other layers, where the cache—when present—is organized using a class and a data object,
#: the OPTIMIZE layer cache (called a checkpoint) consists of a dictionary.

def save_optimization_checkpoint(filepath: Path,
                                 design_matrix_checksum: str,
                                 zed_checksum: str,
                                 ridge_regularization: float,
                                 min_grad_value: float,
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
        bw.write_string(design_matrix_checksum)
        bw.write_string(zed_checksum)
        bw.write_f32(ridge_regularization)
        bw.write_f32(min_grad_value)
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
        # Read header properly!
        description, version = br.read_header()
        if description != "RGLM Optimization Checkpoint":
            raise RuntimeError(f"The file is not a proper optimization checkpoint. Found: {description}")
        if version != 1:
            raise RuntimeError(f"Checkpoint version mismatch: found {version}, expected 1")

        # Read the checksum from earlier steps
        design_matrix_checksum = br.read_string()
        zed_checksum = br.read_string()
        ridge_regularization = br.read_f32()
        min_grad_value = br.read_f32()
        
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
        'design_matrix_checksum': design_matrix_checksum,
        'zed_checksum': zed_checksum,
        'ridge_regularization': ridge_regularization,
        'min_grad_value': min_grad_value,
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
    """
    Validates the checkpoint against current configuration.
    The two gates are:
      - ctx.design_matrix_checksum
        It ensures that the game positions, feature set, frequency clipping are all
        unchanged. The actual JSON config and database extraction are consistent with
        the reloaded data from the checpoint file cache.
      - ctx.zed_checksum
        It ensures that the game values and logit clipping are consistent as well.
    """

    if ctx.design_matrix_checksum != checkpoint['design_matrix_checksum']:
        ctx.log_event(Relevance.WARN, f"The definition of the Design Matrix has changed.")
        ctx.log_event(Relevance.WARN, f"ctx.design_matrix_checksum:           {ctx.design_matrix_checksum}.")
        ctx.log_event(Relevance.WARN, f"checkpoint['design_matrix_checksum']: {checkpoint['design_matrix_checksum']}.")
        return False
    
    if ctx.zed_checksum != checkpoint['zed_checksum']:
        ctx.log_event(Relevance.WARN, f"The definition of the Z array has changed.")
        ctx.log_event(Relevance.WARN, f"ctx.zed_checksum:           {ctx.zed_checksum}.")
        ctx.log_event(Relevance.WARN, f"checkpoint['zed_checksum']: {checkpoint['zed_checksum']}.")
        return False
    
    ctx.log_event(Relevance.INFO, f"The definition of the Design Matrix and Z array are unchanged.")
    return True


def optimization_info_dict(lbfgs_info: dict) -> dict:
    """
    Filters unused entries given by the lbfgs function.
    Keeps the ones saved into the logistic_model context.
    """
    info = {
        'converged': lbfgs_info['converged'],
        'reason':    lbfgs_info['reason'],
        'iters':     lbfgs_info['iters'],
        'f':         lbfgs_info['f'],
        'g_norm':    lbfgs_info['g_norm']
    }
    return info


def is_optimization_status_converged(ctx: "RLMContext", checkpoint: Dict[str, Any]) -> bool:
    """
    Validates the checkpoint convergence status against current configuration requirements.
    """
    ridge_regularization = ctx.cfg.stat_model.ridge_regularization
    min_grad = ctx.cfg.optimization.min_grad
    ctx.log_event(Relevance.TRACE, f"Parameter ctx.cfg.stat_model.ridge_regularization = {ridge_regularization:.4e}")
    ctx.log_event(Relevance.TRACE, f"Parameter ctx.cfg.optimization.min_grad = ({min_grad[0]:.4e}, {min_grad[1]})")
    min_grad_value, _ = min_grad

    cp_ridge_regularization = checkpoint['ridge_regularization']
    cp_min_grad_value = checkpoint['min_grad_value']
    ctx.log_event(Relevance.TRACE, f"Parameter checkpoint['ridge_regularization'] = {cp_ridge_regularization:.4e}")
    ctx.log_event(Relevance.TRACE, f"Parameter checkpoint['min_grad_value'] = {cp_min_grad_value:.4e}")

    rr_is_consistent = math.isclose(ridge_regularization, cp_ridge_regularization, rel_tol=1e-5, abs_tol=1e-8)
    mg_is_consistent = math.isclose(min_grad_value, cp_min_grad_value, rel_tol=1e-3, abs_tol=1e-8)
    ctx.log_event(Relevance.TRACE, f"Parameter rr_is_consistent = {rr_is_consistent}")
    ctx.log_event(Relevance.TRACE, f"Parameter mg_is_consistent = {mg_is_consistent}")

    if not rr_is_consistent:
        ctx.log_event(Relevance.DEBUG, f"Configuration parameter ridge_regularization has been changed.")
        return False

    cp_g_norm = checkpoint['g_norm']
    ctx.log_event(Relevance.TRACE, f"Parameter checkpoint['g_norm'] = {cp_g_norm:.4e}")

    if cp_g_norm < min_grad_value:
        ctx.log_event(Relevance.INFO, f"Obtained gradient norm is lower than required, convergence is reached.")
        return True
    else:
        ctx.log_event(Relevance.INFO, f"Obtained gradient norm is greater than required, convergence is not reached.")
        return False
