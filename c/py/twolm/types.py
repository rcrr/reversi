#
# types.py
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

from __future__ import annotations

from enum import IntEnum

from twolm.pattern import IndexArray

from pydantic import validate_call, ConfigDict, BeforeValidator, AfterValidator, Field



__all__ = ['Verbosity', 'Relevance', 'ReversiLogisticModelIndexes']




class Verbosity(IntEnum):
    """
    Logging verbosity levels.
    """
    HIGH      = 0
    MODERATE  = 1
    STANDARD  = 2
    LOW       = 3
    NONE      = 4

class Relevance(IntEnum):
    """
    Logging relevance levels.
    """
    TRACE     = 0
    DEBUG     = 1
    INFO      = 2
    WARN      = 3
    ERROR     = 4

class ReversiLogisticModelIndexes:
    """
    Represents the 2D index matrix togheter with the hash signature of the feature set
    used to compute them.
    """
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self,
                 feature_set_hash: str,
                 indexes: IndexArray):
        """
        Initializes a RegabDataSet instance.

        Parameters
        ----------
        feature_set_hash : str
            The hash code taken from the feature_set used to compute indexes.
        indexes : IndexArray
        """
        self.feature_set_hash = feature_set_hash
        self.indexes = indexes
