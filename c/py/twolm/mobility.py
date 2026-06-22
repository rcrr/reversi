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

from twolm.board import *
from twolm.mobility import *

import numpy as np
from numba import njit, prange

from functools import reduce
import pandas as pd



__all__ = ['mobilities']



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

    # Now in mobs I have a dictionary k:v where k is the string 'name' and v is the mobility mask.
    df = pd.DataFrame(list(mobs.items()), columns=['name', 'mask'])
    df['name'] = df['name'].astype(str)
    df['mask'] = df['mask'].astype(np.uint64)
    
    masks_array = df['mask'].to_numpy()
    
    # 1. We use the bitwise AND between the input lms and the masks.
    # We take advantage of broadcasting: 
    # lms[:, None] transforms lms into (N, 1)
    # masks_array is (9,)
    # The result will be (N, 9)
    intersections = lms[:, np.newaxis] & masks_array

    # 2. Count the active bits (popcount) for each intersection.
    # In Python/NumPy, the fastest way for bit_count on uint64:
    v_bit_count = np.vectorize(lambda x: int(x).bit_count())
    mobs_values = v_bit_count(intersections).astype(np.int32)
    
    # 3. Create a temporary DataFrame for statistics
    # Use the mask names defined in your original df
    mobs_df = pd.DataFrame(mobs_values, columns=df['name'].values)
    
    return mobs_df


