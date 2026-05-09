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

from twolm.domain import *

import numpy as np

from abc import ABC, abstractmethod

__all__ = ['Feature', 'legal_moves', 'vectorized_legal_moves', 'mobilities']

class Feature(ABC):
    
    @abstractmethod
    def quack(self, length: float):
        pass

###############################################################################################################################

class Intercept(Feature):
    
    def quack(self, length: float):
        pass


###############################################################################################################################

class Mobility(Feature):
    
    def quack(self, length: float):
        pass


###############################################################################################################################

class CornerMobility(Feature):
    
    def quack(self, length: float):
        pass


###############################################################################################################################
from functools import reduce
import operator
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

    get_uint64 = lambda sq: np.uint64(1) << Square.new_from_str(sq)
    mobs_sq_mask = {key: get_uint64(val) for key, val in mobs_def.items()}
    mobs_8_masks = {key: SquareSet.trxs(val) for key, val in mobs_sq_mask.items()}
    mobs_1_mask = {
        key: np.bitwise_or.reduce(SquareSet.trxs(val), axis=1)[0]
        for key, val in mobs_sq_mask.items()
    }
    mobs = {'full': 0xFFFFFFFFFFFFFFFF, **mobs_1_mask}
    if False:
        for key, val in mobs.items():
            print(f"")
            print(f"key = {key}")
            print(f"mask = {val:016X}")
            SquareSet(val).log()

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

def legal_moves(mover: np.uint64,
                opponent: np.uint64) -> np.uint64:
    empties = ~(mover | opponent)
    return _kogge_stone_lms(mover, opponent, empties)

_all_squares                 = np.uint64(0xFFFFFFFFFFFFFFFF)
_all_squares_except_column_a = np.uint64(0xFEFEFEFEFEFEFEFE)
_all_squares_except_column_h = np.uint64(0x7F7F7F7F7F7F7F7F)

_mask_l = np.array([
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares,
    _all_squares_except_column_a
], dtype=np.uint64)

_mask_r = np.array([
    _all_squares_except_column_h,
    _all_squares_except_column_a,
    _all_squares,
    _all_squares_except_column_h
], dtype=np.uint64)

_slide_1 = np.array([1, 7, 8, 9], dtype=np.uint8)
_slide_2 = _slide_1 * 2
_slide_4 = _slide_1 * 4

def _kogge_stone_lms(generator,
                     propagator,
                     blocker):
    

    result = np.uint64(0x0000000000000000)

    for i in range(4):
        g = generator
        p = propagator & _mask_l[i]

        g |= p & (g << _slide_1[i])
        p &= p << _slide_1[i]

        g |= p & (g << _slide_2[i])
        p &= p << _slide_2[i]

        g |= p & (g << _slide_4[i])

        g &= ~generator
        g = blocker & _mask_l[i] & (g << _slide_1[i])

        result |= g

        g = generator
        p = propagator & _mask_r[i]

        g |= p & (g >> _slide_1[i])
        p &= p >> _slide_1[i]

        g |= p & (g >> _slide_2[i])
        p &= p >> _slide_2[i]

        g |= p & (g >> _slide_4[i])

        g &= ~generator
        g = blocker & _mask_r[i] & (g >> _slide_1[i])

        result |= g

    return result

########################################################################################################

import numpy as np
from numba import njit, prange

# Constants for bitboard manipulation
ALL_SQUARES = np.uint64(0xFFFFFFFFFFFFFFFF)
EXCEPT_COL_A = np.uint64(0xFEFEFEFEFEFEFEFE)
EXCEPT_COL_H = np.uint64(0x7F7F7F7F7F7F7F7F)

# Pre-calculating masks and slides to avoid overhead inside the JIT function
MASK_L = np.array([EXCEPT_COL_A, EXCEPT_COL_H, ALL_SQUARES, EXCEPT_COL_A], dtype=np.uint64)
MASK_R = np.array([EXCEPT_COL_H, EXCEPT_COL_A, ALL_SQUARES, EXCEPT_COL_H], dtype=np.uint64)
SLIDE_1 = np.array([1, 7, 8, 9], dtype=np.uint8)
SLIDE_2 = SLIDE_1 * 2
SLIDE_4 = SLIDE_1 * 4

@njit(parallel=True, cache=True)
def vectorized_legal_moves(movers: np.ndarray, opponents: np.ndarray) -> np.ndarray:
    """
    Vectorized calculation of Reversi legal moves using Kogge-Stone algorithm.
    Optimized for large arrays (10M+ elements) using Numba JIT and multi-threading.
    """
    n = movers.shape[0]
    results = np.empty(n, dtype=np.uint64)

    # Parallel loop across all board instances
    for i in prange(n):
        mover = np.uint64(movers[i])
        opponent = np.uint64(opponents[i])
        empties = np.uint64(~(mover | opponent))
        
        legal_result = np.uint64(0)
        
        # Iterate through the 4 main directions (each handles both sides: Left/Right, Up/Down, etc.)
        for d in range(4):
            # --- Directional Forward Shift (Left-style) ---
            g = mover
            p = opponent & MASK_L[d]

            g |= p & (g << SLIDE_1[d])
            p &= p << SLIDE_1[d]
            g |= p & (g << SLIDE_2[d])
            p &= p << SLIDE_2[d]
            g |= p & (g << SLIDE_4[d])

            # Resulting moves must be in an empty square and not be the generator itself
            g_final = empties & MASK_L[d] & ((g & ~mover) << SLIDE_1[d])
            legal_result |= g_final

            # --- Directional Backward Shift (Right-style) ---
            g = mover
            p = opponent & MASK_R[d]

            g |= p & (g >> SLIDE_1[d])
            p &= p >> SLIDE_1[d]
            g |= p & (g >> SLIDE_2[d])
            p &= p >> SLIDE_2[d]
            g |= p & (g >> SLIDE_4[d])

            g_final = empties & MASK_R[d] & ((g & ~mover) >> SLIDE_1[d])
            legal_result |= g_final
            
        results[i] = legal_result
        
    return results
