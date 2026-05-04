#
# rlmwf.py
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
# Reversi Logistic Model Workflow -> rlmwf
#

from __future__ import annotations

from enum import IntEnum
from datetime import datetime, timezone, timedelta
from dataclasses import dataclass
from contextlib import contextmanager

from typing import List, TypedDict, Any
from pathlib import Path

import csv

from twolm.rlm_created_worker import RLMCreatedWorker
from twolm.rlm_config_worker import RLMConfigWorker
from twolm.rlm_positions_worker import RLMPositionsWorker
from twolm.rlm_patterns_worker import RLMPatternsWorker
from twolm.rlm_features_worker import RLMFeaturesWorker
from twolm.rlm_indexes_worker import RLMIndexesWorker
from twolm.rlm_wmaps_worker import RLMWmapsWorker
from twolm.rlm_design_matrix_worker import RLMDesignMatrixWorker
from twolm.rlm_zed_worker import RLMZedWorker
from twolm.rlm_gradient_worker import RLMGradientWorker

__all__ = ['ReversiLogisticModel']


class ReversiLogisticModel:
        
    class Verbosity(IntEnum):
        HIGH      = 0
        MODERATE  = 1
        STANDARD  = 2
        LOW       = 3
        NONE      = 4

    class Relevance(IntEnum):
        TRACE     = 0
        DEBUG     = 1
        INFO      = 2
        WARN      = 3
        ERROR     = 4        
    
    class Level(IntEnum):
        CREATED    = ( 0, RLMCreatedWorker(), "Just created.")
        CONFIG     = ( 1, RLMConfigWorker(), "Configuration has been loaded and validated.")
        POSITIONS  = ( 2, RLMPositionsWorker(), "Game positions have been loaded.")
        PATTERNS   = ( 3, RLMPatternsWorker(), "The pattern set has been loaded.")
        FEATURES   = ( 4, RLMFeaturesWorker(), "The feature set has been loaded.")
        INDEXES    = ( 5, RLMIndexesWorker(), "Indexes for pattern configurations have been computed.")
        WMAPS      = ( 6, RLMWmapsWorker(), "Weight maps have been computed.")
        DESIGN_MTR = ( 7, RLMDesignMatrixWorker(), "The design matrix (X) has been computed.")
        ZED        = ( 8, RLMZedWorker(), "Zed array calculated from the game values.")
        GRADIENT   = ( 9, RLMGradientWorker(), "The loss function and the gradient array are generated.")
        OPTIMIZING = (10, None, "The model is ready for the optimization.")

        def __new__(cls, value, worker, description):
            obj = int.__new__(cls, value)
            obj._value_ = value
            obj.worker = worker
            obj.description = description
            return obj
    
    levels = list(Level)
    levels_names = [l.name for l in Level]
    levels_lookup = {l.name: l.value for l in Level}
    levels_descriptions = [l.description for l in Level]

    @dataclass
    class LevelMove:
        level: ReversiLogisticModel.Level
        direction: str
        start: datetime
        finish: datetime
        duration: timedelta

    @contextmanager
    def _history_timer(self, level: Level, direction: str):
        start = datetime.now(timezone.utc)
        try:
            yield
        finally:
            finish = datetime.now(timezone.utc)
            self.history.append(self.LevelMove(
                level=level, 
                direction=direction, 
                start=start, 
                finish=finish, 
                duration=finish - start
            ))

    class LogEntry(TypedDict):
        time: datetime
        level: ReversiLogisticModel.Level
        relevance: ReversiLogisticModel.Relevance
        message: str
    
    def __init__(self,
                 config_file_path: str | Path,
                 verbosity: Verbosity = Verbosity.STANDARD,
                 base_dir_override: str | Path | None = None):
        config_file_path = check_config_file_path(config_file_path)
        if base_dir_override is not None:
            base_dir_override = check_base_dir_override(base_dir_override)
        self.current_level = self.Level.CREATED
        self.logs: List[LogEntry] = []
        self.history: List[ReversiLogisticModel.LevelMove] = []
        self.verbosity = verbosity
        self.config_file_path = config_file_path
        self.base_dir_override = base_dir_override
        self.cfg = None
        self.rds = None
        self.pset = None
        self.log_event(self.Relevance.DEBUG, "ReversiLogisticModel initialized.")
        
    def log_event(self, relevance: Relevance, message: str) -> None:
        time = datetime.now(timezone.utc)
        event = {
            "time": time,
            "level": self.current_level,
            "relevance": relevance,
            "message": message
        }
        self.logs.append(event)
        if relevance >= self.verbosity:
            timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
            print(f"[{timestamp}] [{self.current_level:02}][{self.current_level.name:10}] [{relevance}:{self.verbosity}] {message}")
        
    def show_event_log(self) -> None:
        print(f"\n--- --- --- --- --- - EVENT LOG - --- --- --- --- ---")
        print(f"______________________________________________________")
        print(f"TIMESTAMP           | LEVEL      | R | MESSAGE")
        for entry in self.logs:
            time = entry['time']
            timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
            print(f"{timestamp} | {entry['level'].name:10} | {entry['relevance']} | {entry['message']}")

    def show_history_of_moves(self) -> None:
        print(f"{'Level':<15} | {'Dir':<5} | {'Start (UTC)':<26} | {'Dur (s)':<14}")
        print("-" * 70)
    
        for entry in self.history:
            name = getattr(entry.level, 'name', str(entry.level))
            start_str = entry.start.strftime("%Y-%m-%d %H:%M:%S.%f")
            dur = entry.duration.total_seconds()
            print(f"{name:<15} | {entry.direction:<5} | {start_str:<12} | {dur:>15.6f}")

    def export_history_of_moves_as_csv(self, filename: str | Path) -> None:
        keys = ["level", "level_name", "direction", "start_utc", "finish_utc", "duration_sec"]
    
        with open(filename, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=keys)
            writer.writeheader()
        
            for entry in self.history:
                writer.writerow({
                    "level": entry.level.value,
                    "level_name": entry.level.name,
                    "direction": entry.direction,
                    "start_utc": entry.start.isoformat(),
                    "finish_utc": entry.finish.isoformat(),
                    "duration_sec": entry.duration.total_seconds()
                })

    def move_to_level(self, target_level: Level | str | int) -> None:
        if not isinstance(target_level, (self.Level, str, int)):
            raise TypeError(f"Argument target_level must be Level or string.")
        if isinstance(target_level, str):
            try:
                target_level = self.Level[target_level.upper()]
            except KeyError:
                raise ValueError(f"Argument target_level '{target_level}' is not a valid Level.")
        if isinstance(target_level, int):
            try:
                target_level = self.Level(target_level)
            except KeyError:
                raise ValueError(f"Argument target_level '{target_level}' is not a valid Level.")
        
        if target_level == self.current_level:
            return

        if target_level > self.current_level:
            steps = range(self.current_level + 1, target_level + 1)
            direction = 'up'
        else:
            steps = range(self.current_level, target_level, -1)
            direction = 'down'

        for lv_val in steps:
            lvl_obj = self.Level(lv_val)
            with self._history_timer(lvl_obj, direction):
                self._run_step(lvl_obj, direction)
            
            if direction == 'up':
                self.current_level = lvl_obj
            else:
                self.current_level = self.Level(lv_val - 1)

    def _run_step(self, level: Level, direction: str):
        if not level.worker:
            self.log_event(self.Relevance.WARN, f"No worker defined for level {level.name}")
            return
        try:
            worker_name = level.worker.__class__.__name__
            self.log_event(self.Relevance.INFO, f"Starting worker {worker_name}, {direction.upper()} step for {level.name}")            
            method = getattr(level.worker, direction)
            method(self)
            self.log_event(self.Relevance.INFO, f"Completed {direction.upper()} step for {level.name}")
        except Exception as e:
            self.log_event(self.Relevance.ERROR, f"Error in {level.name} during {direction}: {str(e)}")
            raise

    def get_cache_file_path_for_next_level(self) -> Path:
        level = self.Level(self.current_level + 1)
        base_file_name = 'rlmwf'
        suffix = 'dat'
        level_string = f"{level:02}_{level.name}"
        file_name = f"{base_file_name}_{level_string}.{suffix}"
        fp = Path(file_name)
        return fp

#
# Helper methods.
#

def check_config_file_path(config_file_path: Path | str) -> Path:
    if not isinstance(config_file_path, (str, Path)):
        raise TypeError('Argument config_file_path is not an instance of str or Path')
    cfp = Path(config_file_path)
    if not cfp.exists():
        raise FileNotFoundError(f"No such file: '{config_file_path}'")
    if not cfp.is_file():
        raise FileNotFoundError(f"File path is not a file: '{config_file_path}'")
    return cfp

def check_base_dir_override(base_dir_override: Path | str) -> Path:
    if not isinstance(base_dir_override, (str, Path)):
        raise TypeError('Argument base_dir_override is not an instance of str or Path')
    bdo = Path(base_dir_override)
    if not bdo.exists():
        raise FileNotFoundError(f"No such file: '{base_dir_override}'")
    if not bdo.is_dir():
        raise FileNotFoundError(f"File path is not a directory: '{base_dir_override}'")
    return bdo
