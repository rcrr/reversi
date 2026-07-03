#
# rlm_indexes_worker.py
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
# Reversi Logistic Model Indexes Worker
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.rlm_abstract_worker import ReversiLogisticModelWorker

from twolm.board import *
from twolm.pattern import *

import numpy as np



__all__ = ['RLMIndexesWorker']



class RLMIndexesWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Computing indexes for the pattern configurations...")
        model.indexes = _compute_indexes(model)
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Clearing indexes...")
        model.indexes = None

###########################################################################################################

def _compute_indexes(model: ReversiLogisticModel) -> IndexArray:
    positions: pd.DataFrame = model.rds.positions
    mover = positions['mover'].to_numpy().view(Bitboard)
    opponent = positions['opponent'].to_numpy().view(Bitboard)
    positions = make_position(mover, opponent)
    indexes = model.feature_set.compute_indexes(positions)
    return indexes
