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

from typing import List
from pathlib import Path

import csv


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
        CREATED    = (0, "Just created.")
        CONFIG     = (1, "Configuration has been loaded and validated.")
        POSITIONS  = (2, "Game positions have been loaded.")
        PATTERNS   = (3, "The pattern set has been loaded.")
        INDEXES    = (4, "Indexes for pattern configurations have been computed.")
        WMAPS      = (5, "Weight maps have been computed.")
        DESIGN_MTR = (6, "The design matrix (X) has been computed.")
        ZED        = (7, "Zed array calculated from the game values.")
        GRADIENT   = (8, "The loss function and the gradient array are generated.")
        OPTIMIZING = (9, "The model is ready for the optimization.")

        def __new__(cls, value, description):
            obj = int.__new__(cls, value)
            obj._value_ = value
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
            
    def __init__(self):

        self.current_level = self.Level.CREATED
        self.logs = []
        self.history: List[ReversiLogisticModel.LevelMove] = []
        self.verbosity = self.Verbosity.STANDARD
        self.cfg = None
        
        self.log_event(self.Relevance.DEBUG, "ReversiLogisticModel initialized.")
        
    def log_event(self, relevance: Relevance, message: str) -> None:
        timestamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S")
        event = {
            "timestamp": timestamp,
            "level": self.current_level.name,
            "relevance": relevance,
            "message": message
        }
        self.logs.append(event)
        if relevance >= self.verbosity:
            print(f"[{timestamp}] [{self.current_level.name}] [{relevance}:{self.verbosity}] {message}")
        
    def show_event_log(self) -> None:
        print(f"\n--- --- --- --- EVENT LOG --- --- --- ---")
        print(f"TIMESTAMP           | LEVEL      | R | MESSAGE")
        for entry in self.logs:
            print(f"{entry['timestamp']} | {entry['level']:10} | {entry['relevance']} | {entry['message']}")

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

    def _run_step(self, level, direction):
        method_name = f"_{direction}_{level.name.lower()}"
        method = getattr(self, method_name, None)
        if method:
            self.log_event(self.Relevance.INFO, f"Executing {method_name}...")
            method()
            self.log_event(self.Relevance.INFO, f"Completed execution of {method_name}.")
        
    def _up_created(self):
        raise RuntimeError(f"Executing created step should never happen.")

    def _down_created(self):
        raise RuntimeError(f"Cleaning created step should never happen.")
        
    def _up_config(self):
        # .... add the domain code ...
        return

    def _down_config(self):
        # .... add the domain code ...
        return
        
    def _up_positions(self):
        # .... add the domain code ...
        return

    def _down_positions(self):
        # .... add the domain code ...
        return
        
    def _up_patterns(self):
        # .... add the domain code ...
        return

    def _down_patterns(self):
        # .... add the domain code ...
        return
        
    def _up_indexes(self):
        # .... add the domain code ...
        return

    def _down_indexes(self):
        # .... add the domain code ...
        return
        
    def _up_wmaps(self):
        # .... add the domain code ...
        return

    def _down_wmaps(self):
        # .... add the domain code ...
        return
        
    def _up_design_mtr(self):
        # .... add the domain code ...
        return

    def _down_design_mtr(self):
        # .... add the domain code ...
        return
        
    def _up_zed(self):
        # .... add the domain code ...
        return

    def _down_zed(self):
        # .... add the domain code ...
        return
        
    def _up_gradient(self):
        # .... add the domain code ...
        return

    def _down_gradient(self):
        # .... add the domain code ...
        return
        
    def _up_optimizing(self):
        # .... add the domain code ...
        return

    def _down_optimizing(self):
        # .... add the domain code ...
        return
