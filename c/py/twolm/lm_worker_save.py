#
# lm_worker_save.py
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

# twolm/lm_worker_save.py
from __future__ import annotations

from typing import TYPE_CHECKING
from pathlib import Path

from twolm.state_machine import Worker, Relevance
from twolm.rlm_save import write_model_weights_file

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext



__all__ = ['lm_worker_save']



def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Saving model weights to disk...")
    
    # Assume ctx.cfg.output holds the filename (e.g., "model.dat")
    # and ctx.cfg.base_dir holds the directory.
    # If ctx.cfg.output is already a full path, we can just use it directly.
    output_path = ctx.cfg.base_dir / ctx.cfg.output
    
    write_model_weights_file(ctx, output_path, compressed=True)
    ctx.log_event(Relevance.INFO, f"Model weights saved to {output_path}")

def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing save attributes...")
    # Nothing to clear from context, as we just read from it.

def lm_worker_save() -> Worker:
    """Factory function that returns the SAVE worker instance."""
    return Worker("SAVE", _up, _down)
