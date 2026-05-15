#
# mobility.py
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

from typing import TypeAlias

from twolm.domain import *

import numpy as np
from numba import njit, prange


__all__ = ['legal_moves']

Bitboard: TypeAlias = np.uint64

# Constants for bitboard manipulation
_all_squares                 = Bitboard(0xFFFFFFFFFFFFFFFF)
_all_squares_except_column_a = Bitboard(0xFEFEFEFEFEFEFEFE)
_all_squares_except_column_h = Bitboard(0x7F7F7F7F7F7F7F7F)

# Pre-calculating masks and slides to avoid overhead inside the JIT function
_mask_l = np.array([
    _all_squares_except_column_a,
    _all_squares_except_column_h,
    _all_squares,
    _all_squares_except_column_a
], dtype=Bitboard)
_mask_r = np.array([
    _all_squares_except_column_h,
    _all_squares_except_column_a,
    _all_squares,
    _all_squares_except_column_h
], dtype=Bitboard)
_slide_1 = np.array([1, 7, 8, 9], dtype=np.uint8)
_slide_2 = _slide_1 * 2
_slide_4 = _slide_1 * 4

@njit(parallel=True, cache=True)
def vectorized_legal_moves(movers: np.ndarray, opponents: np.ndarray) -> np.ndarray:
    """
    Vectorized calculation of Reversi legal moves using Kogge-Stone algorithm.
    Optimized for large arrays (10M+ elements) using Numba JIT and multi-threading.
    """
    n = movers.shape[0]
    results = np.empty(n, dtype=Bitboard)

    # Parallel loop across all board instances
    for i in prange(n):
        mover = Bitboard(movers[i])
        opponent = Bitboard(opponents[i])
        empties = Bitboard(~(mover | opponent))
        
        legal_result = Bitboard(0)
        
        # Iterate through the 4 main directions (each handles both sides: Left/Right, Up/Down, etc.)
        for d in range(4):
            # --- Directional Forward Shift (Left-style) ---
            g = mover
            p = opponent & _mask_l[d]

            g |= p & (g << _slide_1[d])
            p &= p << _slide_1[d]
            g |= p & (g << _slide_2[d])
            p &= p << _slide_2[d]
            g |= p & (g << _slide_4[d])

            # Resulting moves must be in an empty square and not be the generator itself
            g_final = empties & _mask_l[d] & ((g & ~mover) << _slide_1[d])
            legal_result |= g_final

            # --- Directional Backward Shift (Right-style) ---
            g = mover
            p = opponent & _mask_r[d]

            g |= p & (g >> _slide_1[d])
            p &= p >> _slide_1[d]
            g |= p & (g >> _slide_2[d])
            p &= p >> _slide_2[d]
            g |= p & (g >> _slide_4[d])

            g_final = empties & _mask_r[d] & ((g & ~mover) >> _slide_1[d])
            legal_result |= g_final
            
        results[i] = legal_result
        
    return results

def legal_moves(mover: Bitboard | npt.NDArray[Bitboard],
                opponent: Bitboard | npt.NDArray[Bitboard]) -> Bitboard | npt.NDArray[Bitboard]:
    """
    Calculate Reversi legal moves for a single or multiple board configurations.
    
    Parameters:
    mover (Bitboard or npt.NDArray[Bitboard]): Bitboard(s) representing the current player's pieces.
    opponent (Bitboard or  npt.NDArray[Bitboard]): Bitboard(s) representing the opponent's pieces.
    
    Returns:
    Bitboard or npt.NDArray[Bitboard]: Bitboard(s) representing the legal moves for the current player.
    """
    # Check if inputs are scalars
    if isinstance(mover, Bitboard) and isinstance(opponent, Bitboard):
        # Wrap the scalar inputs into numpy arrays
        movers = np.array([mover], dtype=Bitboard)
        opponents = np.array([opponent], dtype=Bitboard)
        
        # Call the vectorized function
        result = vectorized_legal_moves(movers, opponents)
        
        # Return the result as a scalar
        return result[0]
    elif isinstance(mover, np.ndarray) and isinstance(opponent, np.ndarray):
        # Ensure the arrays have the same shape
        if mover.shape != opponent.shape:
            raise ValueError("The shapes of mover and opponent arrays must be the same.")
        
        # Ensure the dtype of the arrays is Bitboard
        if mover.dtype != Bitboard or opponent.dtype != Bitboard:
            raise TypeError("The dtype of mover and opponent arrays must be Bitboard.")
        
        # Call the vectorized function directly
        return vectorized_legal_moves(mover, opponent)
    else:
        raise TypeError("Inputs must be either both Bitboard or both npt.NDArray[Bitboard].")
