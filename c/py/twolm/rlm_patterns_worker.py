#
# rlm_patterns_worker.py
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
# Reversi Logistic Model Patterns Worker
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.domain import *
    
from twolm.rlm_abstract_worker import  ReversiLogisticModelWorker

__all__ = ['RLMPatternsWorker']

class RLMPatternsWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Loading board patterns as defined by the model...")
        model.pset = _load_patterns(model)
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Clearing patterns...")
        model.pset = None

##################################################################################################

def _load_patterns(model: ReversiLogisticModel) -> PatternSet:
    cfg_pset = model.cfg.pattern_set
    pset_name = cfg_pset.name
    model.log_event(model.Relevance.DEBUG, f"Pattern set name: '{pset_name}', patterns:")
    patterns = [Pattern(elt.name, elt.mask) for elt in cfg_pset.patterns]
    for i, p in enumerate(patterns):
        model.log_event(model.Relevance.DEBUG, f"  -{i:02}- Pattern: name={p.name}, mask={p.mask:016X}")
    pset = PatternSet(cfg_pset.name, patterns)
    model.log_event(model.Relevance.DEBUG, f"Pattern set object created, hash = '{pset.hash}'.")
    return pset
