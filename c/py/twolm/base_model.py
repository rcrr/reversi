#
# base_model.py
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

# twolm/base_model.py
from __future__ import annotations

from pathlib import Path
from typing import List

from twolm.state_machine import StateMachine, Worker, Context
from twolm.enums import Verbosity


class BaseModel:
    """
    Base class for pipeline facades.
    Encapsulates the generic delegation to the StateMachine, avoiding boilerplate
    in concrete domain-specific model classes (e.g., LogisticModel, DeepNetworkModel).
    """

    def __init__(self, workers: List[Worker], context: Context, verbosity: Verbosity = Verbosity.STANDARD):
        self.context = context
        self.sm = StateMachine(
            workers=workers,
            context=context,
            verbosity=verbosity
        )

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
