#
# feature.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
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

from __future__ import annotations

from twolm.board import *

import numpy as np

from abc import ABC, abstractmethod

__all__ = ['Feature', 'mobilities']

class Feature(ABC):
    @property
    @abstractmethod
    def n_configurations(self):
        """Abstract getter"""
        pass

    @n_configurations.setter
    @abstractmethod
    def n_configurations(self, value):
        """Abstract setter"""
        pass
    
    @abstractmethod
    def compute_indexes(self, movers: npt.NDArray[np.uint64], opponents: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint32]:
        pass

###############################################################################################################################

class Intercept(Feature):
    
    def __init__(self):
        self._n_configurations = 1
    
    def compute_indexes(self, movers: npt.NDArray[np.uint64], opponents: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint32]:
        pass

    @property
    def n_configurations(self):
        return self._n_configurations

    @n_configurations.setter
    def n_configurations(self, value):
        raise RuntimeError("The property n_configurations cannot be set.")

###############################################################################################################################

class Mobility(Feature):
    
    def compute_indexes(self, movers: npt.NDArray[np.uint64], opponents: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint32]:
        pass


###############################################################################################################################

class CornerMobility(Feature):
    
    def compute_indexes(self, movers: npt.NDArray[np.uint64], opponents: npt.NDArray[np.uint64]) -> npt.NDArray[np.uint32]:
        pass


###############################################################################################################################
from functools import reduce
import pandas as pd

def mobilities(lms: np.ndarray) -> np.ndarray:

    mobs_def = {
        'corners': 'A1',
        'c_squares': 'B1',
        'x_squares': 'B2',
        'a_squares': 'C1',
        'r2b_squares': 'C2',
        'r2a_squares': 'D2',
        'r3b_squares': 'C3',
        'r3a_squares': 'D3',
    }

    get_uint64 = lambda sq: Bitboard(1) << square_from_str(sq)
    mobs_sq_mask = {key: get_uint64(val) for key, val in mobs_def.items()}
    mobs_8_masks = {key: bitboard_transformations(val) for key, val in mobs_sq_mask.items()}
    mobs_1_mask = {
        key: np.bitwise_or.reduce(bitboard_transformations(val))
        for key, val in mobs_sq_mask.items()
    }
    mobs = {'full': 0xFFFFFFFFFFFFFFFF, **mobs_1_mask}
    if False:
        for key, val in mobs.items():
            print(f"")
            print(f"key = {key}")
            print(f"mask = {val:016X}")
            bitboard_print(val)

    # Ora in mobs ho un dizionario k:v dove k è la stringa 'name' e v e' il mask della mobility.
    df = pd.DataFrame(list(mobs.items()), columns=['name', 'mask'])
    df['name'] = df['name'].astype(str)
    df['mask'] = df['mask'].astype(np.uint64)
    
    #print(f"df =\n{df.to_string(formatters={'mask': lambda x: f'{x:016X}'})}")

    masks_array = df['mask'].to_numpy()
    
    # 1. Usiamo il bitwise AND tra l'input lms e le maschere.
    # Sfruttiamo il broadcasting: 
    # lms[:, None] trasforma lms in (N, 1)
    # masks_array è (9,)
    # Il risultato sarà (N, 9)
    intersections = lms[:, np.newaxis] & masks_array

    # 2. Contiamo i bit accesi (popcount) per ogni intersezione.
    # In Python/NumPy, il modo più veloce per il bit_count su uint64:
    v_bit_count = np.vectorize(lambda x: int(x).bit_count())
    mobs_values = v_bit_count(intersections).astype(np.int32)
    
    # 3. Creiamo un DataFrame temporaneo per le statistiche
    # Usiamo i nomi delle maschere definiti nel tuo df originale
    mobs_df = pd.DataFrame(mobs_values, columns=df['name'].values)
    #print("\nTabella Mobilità (Bit Count):")
    #print(mobs_df.head()) # Mostra le prime righe

    if False:
        # 4. Calcoliamo le statistiche trasponendo per avere le maschere sulle righe
        stats_df = mobs_df.agg(['min', 'max', 'mean', 'std', 'var']).T
    
        # Rinominiamo le colonne per chiarezza come richiesto
        stats_df.columns = ['MIN', 'MAX', 'AVERAGE', 'STD', 'VARIANCE']
    
        print("\n--- STATISTICHE MOBILITÀ ---")
        # Formattazione per la stampa: 2 decimali per i valori calcolati
        print(stats_df.to_string(formatters={
            'AVERAGE': '{:,.2f}'.format,
            'STD': '{:,.2f}'.format,
            'VARIANCE': '{:,.2f}'.format
        }))
    
    return mobs_df


###############################################################################################################################
