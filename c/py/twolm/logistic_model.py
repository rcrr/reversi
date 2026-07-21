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
from twolm.base_model import BaseModel
from twolm.enums import Verbosity, Relevance

# Import worker factories
from twolm.lm_worker_config import lm_worker_config
from twolm.lm_worker_positions import lm_worker_positions
from twolm.lm_worker_features import lm_worker_features
from twolm.lm_worker_indexes import lm_worker_indexes
from twolm.lm_worker_wmaps import lm_worker_wmaps



__all__ = ['RLMContext', 'LogisticModel']



#
# Helper methods.
#

def _check_config_file_path(config_file_path: Path | str) -> Path:
    if not isinstance(config_file_path, (str, Path)):
        raise TypeError('Argument config_file_path is not an instance of str or Path')
    cfp = Path(config_file_path)
    if not cfp.exists():
        raise FileNotFoundError(f"No such file: '{config_file_path}'")
    if not cfp.is_file():
        raise FileNotFoundError(f"File path is not a file: '{config_file_path}'")
    return cfp

def _check_base_dir_override(base_dir_override: Path | str) -> Path:
    if not isinstance(base_dir_override, (str, Path)):
        raise TypeError('Argument base_dir_override is not an instance of str or Path')
    bdo = Path(base_dir_override)
    if not bdo.exists():
        raise FileNotFoundError(f"No such file: '{base_dir_override}'")
    if not bdo.is_dir():
        raise NotADirectoryError(f"File path is not a directory: '{base_dir_override}'")
    return bdo

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

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
    feature_w_ranges: Any = None
    iwmap_feature_offset: Any = None
    iwmap: Any = None
    wmap: Any = None
    wmap_fallback: Any = None
    w: Any = None
    
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


class LogisticModel(BaseModel):
    """
    Facade for the Reversi Logistic Model pipeline.
    Orchestrates the state machine and the shared context.
    """

    def __init__(self,
                 config_file_path: str | Path,
                 verbosity: Verbosity = Verbosity.STANDARD,
                 base_dir_override: str | Path | None = None):
        
        # 1. Validate domain specific paths
        config_file_path = _check_config_file_path(config_file_path)
        if base_dir_override is not None:
            base_dir_override = _check_base_dir_override(base_dir_override)

        # 2. Initialize the shared context
        context = RLMContext(
            config_file_path=config_file_path,
            base_dir_override=base_dir_override
        )

        # 3. Build the pipeline using the factory functions
        workers: List[Worker] = [
            Worker("CREATED", _created_up, _created_down),
            lm_worker_config(),
            lm_worker_positions(),
            lm_worker_features(),
            lm_worker_indexes(),
            lm_worker_wmaps(),
            # Next workers will be added here
        ]

        # 4. Call the superclass constructor with the assembled parts
        super().__init__(workers=workers, context=context, verbosity=verbosity)
