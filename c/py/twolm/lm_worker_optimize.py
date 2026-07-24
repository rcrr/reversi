#
# lm_worker_optimize.py
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

# twolm/lm_worker_optimize.py
from __future__ import annotations

from typing import TYPE_CHECKING
from pathlib import Path

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.opt_lbfgs import lbfgs
from twolm.rlm_optimize import (save_optimization_checkpoint, 
                                load_optimization_checkpoint, 
                                is_optimization_cache_consistent)

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_optimize']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Starting OPTIMIZE worker...")
    
    opt_cfg = ctx.cfg.optimization
    checkpoint_path = ctx.cfg.base_dir / "checkpoint_optimize.dat"
    
    w_init = ctx.w
    initial_sl, initial_yl, initial_rho = None, None, None

    # 1. Check for existing checkpoint
    if checkpoint_path.exists() and ctx.use_cache:
        try:
            cp = load_optimization_checkpoint(checkpoint_path)
            
            if not is_optimization_cache_consistent(ctx, cp):
                ctx.log_event(Relevance.WARN, "Checkpoint config mismatch. Starting from scratch.")
            elif cp['is_converged']:
                ctx.log_event(Relevance.INFO, f"Checkpoint found. Optimization already converged ({cp['stop_reason']}). Loading final weights.")
                ctx.w = cp['w']
                return # Skip optimization entirely!
            elif cp['iterations_done'] >= cp['start_max_iters']:
                # MAX ITERS REACHED: Treated as a valid completion!
                ctx.log_event(Relevance.INFO, f"Checkpoint found. Reached max iterations ({cp['iterations_done']}/{cp['start_max_iters']}). Proceeding with current weights.")
                ctx.w = cp['w']
                return
            elif cp['iterations_done'] < cp['start_max_iters']:
                ctx.log_event(Relevance.INFO, f"Checkpoint found. Resuming optimization from iteration {cp['iterations_done']}.")
                w_init = cp['w']
                initial_sl = cp['sl']
                initial_yl = cp['yl']
                initial_rho = cp['rho']
            else:
                ctx.log_event(Relevance.WARN, "Checkpoint exists but state is unrecognized. Restarting from scratch.")
        except Exception as e:
            ctx.log_event(Relevance.WARN, f"Failed to load checkpoint ({e}). Starting from scratch.")

    # 2. Setup wrappers
    def logger_fn(msg: str):
        ctx.log_event(Relevance.INFO, msg)
        
    def save_fn(w, sl, yl, rho, k, f, g_norm):
        save_optimization_checkpoint(
            filepath=checkpoint_path,
            start_max_iters=opt_cfg.max_iters,
            start_m=opt_cfg.m,
            is_converged=False,
            stop_reason="INCOMPLETE",
            iterations_done=k + 1,
            final_f=f,
            final_g_norm=g_norm,
            w=w,
            sl=sl,
            yl=yl,
            rho=rho
        )
        ctx.log_event(Relevance.DEBUG, f"Intermediate checkpoint saved at iteration {k+1}.")

    # 3. Run L-BFGS
    w_opt, info = lbfgs(
        fg=ctx.fg,
        x0=w_init,
        c1=opt_cfg.c1,
        c2=opt_cfg.c2,
        min_grad=opt_cfg.min_grad,
        min_p_fun_decrease=opt_cfg.min_p_fun_decrease,
        max_iters=opt_cfg.max_iters,
        m=opt_cfg.m,
        log_every_n=opt_cfg.log_every_n,
        logger=logger_fn,
        save_every_n=opt_cfg.save_every_n,
        save_fn=save_fn,
        initial_sl=initial_sl,
        initial_yl=initial_yl,
        initial_rho=initial_rho
    )
    
    # 4. Update context and save final checkpoint
    ctx.w = w_opt
    ctx.log_event(Relevance.INFO, f"Optimization finished. Reason: {info['reason']}. Final Loss: {info['f']:.8e}")
    
    save_optimization_checkpoint(
        filepath=checkpoint_path,
        start_max_iters=opt_cfg.max_iters,
        start_m=opt_cfg.m,
        is_converged=info['converged'],
        stop_reason=info['reason'],
        iterations_done=info['iters'],
        final_f=info['f'],
        final_g_norm=info['g_norm'],
        w=info['w'],
        sl=info['sl'],
        yl=info['yl'],
        rho=info['rho']
    )

def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing optimization attributes...")
    ctx.fg = None

def lm_worker_optimize() -> Worker:
    """Factory function that returns the OPTIMIZE worker instance."""
    return Worker("OPTIMIZE", _up, _down)
