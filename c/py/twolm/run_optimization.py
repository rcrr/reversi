#
# run_optimization.py
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
# How to use the run_optimization module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- How to use it:
#
# PYTHONPATH="./py" python3 -m twolm.run_optimization ./rglmdata/tlm/INTERCEPT/INTERCEPT.json --verbosity medium
#

# twolm/run_optimization.py

import argparse
from pathlib import Path
from pydantic import validate_call, ConfigDict

from twolm.logistic_model import LogisticModel
from twolm.state_machine import Verbosity



@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def build_model(json_config: Path, verbosity: Verbosity) -> None:
    rlm = LogisticModel(json_config, verbosity=verbosity)
    rlm.move_to_step('SAVE')

    return

#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

if __name__ == '__main__':
    
    # Initialize the parser and describe the script
    parser = argparse.ArgumentParser(description="Script to run model optimization.")
    
    # 1. Direct conversion to Path object
    parser.add_argument('config', type=Path, help='Json configuration file.')

    # Dictionary mapping lowercase strings to their corresponding Enum members
    verbosity_map = {e.name.lower(): e for e in Verbosity}

    # 2. Use string choices for clean CLI help/error messages, then convert via lambda
    parser.add_argument(
        '--verbosity',
        type=str,
        choices=list(verbosity_map.keys()),
        default='standard',
        help="Logging verbosity level"
    )
    
    # Parse the command-line arguments
    args = parser.parse_args()

    # 3. Resolve the string choice back to the actual Verbosity Enum object
    verbosity_enum = verbosity_map[args.verbosity.lower()]
    
    # Build the model using validated types
    build_model(args.config, verbosity_enum)
