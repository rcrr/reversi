#
# rlm_config_worker.py
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

#
# Reversi Logistic Model Config Worker
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.rlm_abstract_worker import ReversiLogisticModelWorker

from pydantic import (BaseModel, computed_field)

import json5
from pathlib import Path


__all__ = ['RLMConfigWorker']

#
# Pydantic configuration classes.
#

class ReversiLogisticModelConfig(BaseModel):
    """
    Configuration for the Reversi logistic model, including general settings,
    data set configurations, and statistical model settings.
    """
    name: str
    description: str
    base_dir: Path
    project_dir: Path
    #regab_indexed_data_set_cached: RegabIndexedDataSetCachedConfig
    #stat_model: StatModelConfig
    
    @computed_field
    @property
    def full_project_dir(self) -> Path:
        return self.base_dir / self.project_dir

#
# Pydantic - End.
#

class RLMConfigWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Loading configuration file...")
        cfg = _load(model)
        model.log_event(model.Relevance.INFO, f"Configuration file {model.config_file_path} loaded.")
        _validate(model, cfg)
        model.log_event(model.Relevance.INFO, f"Configuration file {model.config_file_path} validated.")
        model.cfg = cfg
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Clearing configuration...")
        model.cfg = None

#
#
#

def _load(model: ReversiLogisticModel) -> ReversiLogisticModelConfig:

    with model.config_file_path.open("r", encoding="utf-8") as f:
        config_raw_data = json5.load(f)
        model.log_event(model.Relevance.DEBUG, f"JSON file '{f}' read.")

    if model.base_dir_override is not None:
        config_raw_data['base_dir'] = model.base_dir_override
        model.log_event(model.Relevance.DEBUG, f"Value for base_dir changed to '{model.base_dir_override}'.")

    cfg = ReversiLogisticModelConfig(**config_raw_data)
    model.log_event(model.Relevance.DEBUG, f"Configuration loaded.")
    return cfg

def _validate(model: ReversiLogisticModel,
              cfg: ReversiLogisticModelConfig):
    _validate_full_project_dir(model, cfg)

def _validate_full_project_dir(model: ReversiLogisticModel,
                               cfg: ReversiLogisticModelConfig):
    if not cfg.full_project_dir.exists():
        model.log_event(model.Relevance.ERROR, f"The full project dir: '{cfg.full_project_dir}' doesn't exist.")
        raise RuntimeError(f"The full project dir: '{cfg.full_project_dir}' doesn't exist.")
        
