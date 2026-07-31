#
# lm_worker_analytics.py
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

# twolm/lm_worker_analytics.py
from __future__ import annotations

from typing import TYPE_CHECKING
from pathlib import Path

from twolm.state_machine import Worker
from twolm.enums import Relevance
from twolm.rlm_analytics import compute_training_metrics, format_analytics_report

if TYPE_CHECKING:
    from twolm.logistic_model import RLMContext

__all__ = ['lm_worker_analytics']


def _up(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Generating model analytics report...")
    
    # 1. Compute training metrics
    train_metrics = compute_training_metrics(ctx)
    val_metrics = ctx.vld_metrics
    
    # 2. Format the console report
    console_report = format_analytics_report(ctx, train_metrics, val_metrics, detailed=False)
    
    # 3. Log the report line by line so it shows up in the StateMachine logs
    for line in console_report.strip().split('\n'):
        ctx.log_event(Relevance.INFO, line)
    
    # 4. Save the complete report to a text file
    file_report = format_analytics_report(ctx, train_metrics, val_metrics, detailed=True)
    report_path = ctx.cfg.base_dir / "model_analytics_report.txt"
    with open(report_path, "w", encoding="utf-8") as f:
        f.write(file_report)
        
    ctx.log_event(Relevance.INFO, f"Analytics report saved to {report_path}")
    
    # Store in context if needed by the SAVE worker later
    ctx.analytics_report = console_report

def _down(ctx: "RLMContext") -> None:
    ctx.log_event(Relevance.INFO, "Clearing analytics attributes...")
    ctx.analytics_report = None


def lm_worker_analytics() -> Worker:
    """Factory function that returns the ANALYTICS worker instance."""
    return Worker("ANALYTICS", _up, _down)
