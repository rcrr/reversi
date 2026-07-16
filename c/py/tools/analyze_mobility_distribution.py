#
# analyze_mobility_distribution.py
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

#
#
# How to use this script:
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the script.
#
# PYTHONPATH="./py" python3 py/tools/analyze_mobility_distribution.py config_json
#

"""
Script to analyze the distribution of mobility features against game values.
This is a diagnostic/exploratory tool, not part of the ML pipeline.

Usage:
    PYTHONPATH="./py" python3 scripts/analyze_mobility_distribution.py py/twolm/test/data/rlm_00.json
"""

import argparse
import os
import sys

import numpy as np
import pandas as pd

from twolm.board import (Bitboard,
                         square_from_str,
                         bitboard_transformations,
                         legal_moves)

# Explicit imports following the new clean architecture rules
from twolm.rlmwf import ReversiLogisticModel, Verbosity

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

def print_mobility_stats(ms_df: pd.DataFrame, title: str) -> None:
    """
    Calculates and prints global and per-level game value statistics 
    for a given mobility DataFrame.
    
    Args:
        ms_df (pd.DataFrame): DataFrame containing mobility columns and a 'game_value' column.
        title (str): The title to display before the statistics.
    """
    print(f"\n{'#' * 50}")
    print(f"# {title.upper()}")
    print(f"{'#' * 50}")

    # 1. Global statistics
    global_stats = ms_df['game_value'].agg(['min', 'max', 'mean', 'std', 'var'])
    print("\n--- GLOBAL GAME_VALUE STATISTICS ---")
    print(global_stats.to_string())

    # 2. Per-mobility-level statistics
    mobility_columns = [col for col in ms_df.columns if col != 'game_value']

    for col in mobility_columns:
        # Group by the number of moves (0, 1, 2...) and calculate statistics
        stats = ms_df.groupby(col)['game_value'].agg(['min', 'max', 'mean', 'std', 'var', 'count'])
        
        # Rename columns for clarity
        stats.columns = ['MIN_GV', 'MAX_GV', 'AVG_GV', 'STD_GV', 'VAR_GV', 'SAMPLES']
        
        # Format floats for readable console output
        formatters = {
            'AVG_GV': '{:,.4f}'.format,
            'STD_GV': '{:,.4f}'.format,
            'VAR_GV': '{:,.4f}'.format
        }
        
        print(f"\n--- Analysis for: {col} ---")
        print(stats.to_string(formatters=formatters))
        print("-" * 50)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Analyze mobility distribution against game values from a Reversi Logistic Model dataset."
    )
    parser.add_argument(
        "config_json", 
        type=str, 
        help="Path to the RLM JSON configuration file."
    )
    parser.add_argument(
        "--tmp-dir", 
        type=str, 
        default="./build/tmp_mobility_analysis",
        help="Directory where temporary model files will be written."
    )
    
    args = parser.parse_args()

    if not os.path.exists(args.config_json):
        print(f"Error: Config file not found at '{args.config_json}'")
        sys.exit(1)

    # Ensure output directory exists
    os.makedirs(args.tmp_dir, exist_ok=True)

    print(f"Loading model from: {args.config_json}")
    print(f"Using temporary directory: {args.tmp_dir}")

    # Initialize and load the model
    # (We use a low verbosity to keep the console clean for our analysis)
    rlm = ReversiLogisticModel(
        args.config_json, 
        verbosity=Verbosity.LOW, 
        base_dir_override=args.tmp_dir
    )
    
    rlm.move_to_level('CONFIG')
    rlm.move_to_level('POSITIONS')

    # Extract raw data
    movers = rlm.positions['mover']
    opponents = rlm.positions['opponent']
    gv = rlm.game_values

    # ---------------------------------------------------------
    # 1. MOBILITY ANALYSIS
    # ---------------------------------------------------------
    print("\nCalculating Legal Moves for Mover...")
    lms = legal_moves(movers, opponents)
    
    print("Calculating Mobility Features...")
    ms_df = mobilities(lms)
    ms_df['game_value'] = gv
    
    print_mobility_stats(ms_df, "Mobility Analysis")

    # ---------------------------------------------------------
    # 2. ANTI-MOBILITY ANALYSIS
    # ---------------------------------------------------------
    print("\nCalculating Legal Moves for Opponent (Anti-Mobility)...")
    alms = legal_moves(opponents, movers)
    
    print("Calculating Anti-Mobility Features...")
    ams_df = mobilities(alms)
    ams_df['game_value'] = gv
    
    print_mobility_stats(ams_df, "Anti-Mobility Analysis")
    
    print("\n[DONE] Analysis complete.")


if __name__ == '__main__':
    main()

