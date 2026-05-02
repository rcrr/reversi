#
# rlm_positions_worker.py
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
# Reversi Logistic Model Positions Worker
#

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from twolm.rlm_abstract_worker import  ReversiLogisticModelWorker

__all__ = ['RLMPositionsWorker']

class RLMPositionsWorker(ReversiLogisticModelWorker):
    
    def up(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Loading game positions...")
        _read_cache(model)
        
    def down(self, model: ReversiLogisticModel) -> None:
        model.log_event(model.Relevance.INFO, "Clearing game positions...")

#
# Steps ...
#
# -0- cerca il file di CACHE
# -1- se esiste lo verifica con il checksum ...
# -1.1- carica i dati di HEADER e li confronta con quelli di CFG
# -1.2- se sono UGUALI carica il file.
# -1.3- se sono DIVERSI _INVALIDA_LA_CACHE_ e prepara la DBCONN
# -1.4- esegue la query.
# -1.5- eventualmente calcola dei dati derivati ...
# -2- se _INVALIDA_CACHE_ is True ... scrive il file di CACHE su disco
# -3- FINE WORK ELEMENT

def _read_cache(model: ReversiLogisticModel) -> None:
    pass
