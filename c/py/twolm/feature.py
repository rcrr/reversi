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

__all__ = ['Feature']

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
