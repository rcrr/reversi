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
    N = len(positions)
    print()
    print(f"N = {N}")
    
    fset = model.feature_set
    I = sum([f.n_instances for f in fset.features])
    print()
    fset.print_summary()
    print(f"I = {I}")

    if fset.intercept:
        print(f"Adding intercept indexes ...")
        intercept_indexes = np.ones((N, 1), dtype=Index)
        print(f"")
        print(f"type(intercept_indexes) = {type(intercept_indexes)}")
        print(f"intercept_indexes: shape = {intercept_indexes.shape}, dtype = {intercept_indexes.dtype}")
        print(f"{intercept_indexes}")

    if fset.mset:
        print(f"Adding mobility indexes ...")
        mobility_indexes = fset.mset.compute_indexes(mover, opponent)
        print(f"")
        print(f"type(mobility_indexes) = {type(mobility_indexes)}")
        print(f"mobility_indexes: shape = {mobility_indexes.shape}, dtype = {mobility_indexes.dtype}")
        print(f"{mobility_indexes}")

    if fset.pset:
        print(f"Adding pattern indexes ...")    
        pattern_indexes = fset.pset.compute_principal_indexes(mover, opponent)
        print(f"")
        print(f"type(pattern_indexes) = {type(pattern_indexes)}")
        print(f"pattern_indexes: shape = {pattern_indexes.shape}, dtype = {pattern_indexes.dtype}")
        print(f"{pattern_indexes}")

    active_indexes = [x for x in [intercept_indexes, mobility_indexes, pattern_indexes] if x is not None]

    if not active_indexes:
        raise ValueError("All indexes are None!")

    total_cols = sum(x.shape[1] for x in active_indexes)
    if total_cols != I:
        raise ValueError("The number of columns in the index matrix is not consistent with n_instances in the feature set.")

    combined_indexes = np.empty((N, I), dtype=Index)
    current_col = 0
    for x in active_indexes:
        cols = x.shape[1]
        combined_indexes[:, current_col : current_col + cols] = x
        current_col += cols
    print(f"")
    print(f"type(combined_indexes) = {type(combined_indexes)}")
    print(f"combined_indexes: shape = {combined_indexes.shape}, dtype = {combined_indexes.dtype}")
    print(f"{combined_indexes}")

    return combined_indexes
