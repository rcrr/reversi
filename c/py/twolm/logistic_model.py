#
# logistic_model.py
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


# twolm/logistic_model.py
from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Any, Callable

from twolm.state_machine import StateMachine, Worker, Context
from twolm.enums import Verbosity, Relevance

# We import the helpers from rlmwf.py for now, to avoid duplicating logic
from twolm.rlmwf import check_config_file_path, check_base_dir_override

# Import worker factories
from twolm.lm_worker_config import lm_worker_config
from twolm.lm_worker_positions import lm_worker_positions
from twolm.lm_worker_features import lm_worker_features
from twolm.lm_worker_indexes import lm_worker_indexes



__all__ = ['RLMContext', 'LogisticModel']



@dataclass
class RLMContext(Context):
    """
    Context for the Reversi Logistic Model pipeline.
    Holds all data artifacts and configuration state shared across workers.
    """
    config_file_path: Path
    base_dir_override: Path | None = None
    use_cache: bool | None = None
    
    # Data artifacts populated by the workers
    cfg: Any = None
    positions: Any = None
    game_values: Any = None
    feature_set: Any = None
    rlm_indexes: Any = None
    wmaps: Any = None
    design_matrix: Any = None
    zed: Any = None
    gradient: Any = None
    loss: Any = None
    
    # Injected by StateMachine
    log_event: Callable = lambda *args: None
    get_cache_file_path_for_next_level: Callable = lambda: None
    get_cache_file_full_path_for_next_level: Callable = lambda: None


# --- Level 0: CREATED (Kept here as it's just a dummy blocker) ---

def _created_up(ctx: RLMContext) -> None:
    ctx.log_event(Relevance.ERROR, "Executing CREATED up worker should never happen.")
    raise RuntimeError("Executing created step should never happen.")
        
def _created_down(ctx: RLMContext) -> None:
    ctx.log_event(Relevance.ERROR, "Executing CREATED down worker should never happen.")
    raise RuntimeError("Cleaning created step should never happen.")


class LogisticModel:
    """
    Facade for the Reversi Logistic Model pipeline.
    Orchestrates the state machine and the shared context.
    """

    def __init__(self,
                 config_file_path: str | Path,
                 verbosity: Verbosity = Verbosity.STANDARD,
                 base_dir_override: str | Path | None = None):
        
        # Validate paths using existing helpers
        config_file_path = check_config_file_path(config_file_path)
        if base_dir_override is not None:
            base_dir_override = check_base_dir_override(base_dir_override)

        # Initialize the shared context
        self.context = RLMContext(
            config_file_path=config_file_path,
            base_dir_override=base_dir_override
        )

        # Build the pipeline using the factory functions
        self.workers: List[Worker] = [
            Worker("CREATED", _created_up, _created_down),
            lm_worker_config(),
            lm_worker_positions(),
            lm_worker_features(),
            lm_worker_indexes(),
            # Next workers will be added here
        ]

        # Initialize the generic State Machine
        self.sm = StateMachine(
            workers=self.workers,
            context=self.context,
            verbosity=verbosity
        )

    # --- Facade methods to interact with the State Machine ---

    @property
    def current_step(self) -> int:
        return self.sm.current_step

    @property
    def current_worker_name(self) -> str:
        return self.sm.current_worker_name

    @property
    def verbosity(self) -> Verbosity:
        return self.sm.verbosity

    @verbosity.setter
    def verbosity(self, value: Verbosity) -> None:
        self.sm.verbosity = value
    
    def move_to_step(self, target_step: int | str) -> None:
        """Move the pipeline to the specified step."""
        self.sm.move_to_step(target_step)

    def show_event_log(self) -> None:
        self.sm.show_event_log()

    def show_history_of_moves(self) -> None:
        self.sm.show_history_of_moves()

    def export_history_of_moves_as_csv(self, filename: str | Path) -> None:
        self.sm.export_history_of_moves_as_csv(filename)
