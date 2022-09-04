#
# pattern.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022 Roberto Corradini. All rights reserved.
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

from reversi import libreversi as libreversi
from reversi.board import *

import ctypes as ct
import numpy as np
import re

from abc import ABCMeta

class _Singleton(ABCMeta):
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(_Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]


class Pattern(object, metaclass = _Singleton):

    @abstractmethod
    def pack(self, s : SquareSet) -> SquareSet: pass

    @abstractmethod
    def unpack(self, s : SquareSet) -> SquareSet: pass

    @abstractmethod
    def mirror(self, s : SquareSet) -> SquareSet: pass


class Edge(Pattern):

    def pack(self, s : SquareSet) -> SquareSet:
        return s

    def unpack(self, s : SquareSet) -> SquareSet:
        return s

    def mirror(self, s : SquareSet) -> SquareSet:
        return s


class Corner(Pattern):

    def pack(self, s : SquareSet) -> SquareSet:
        return s

    def unpack(self, s : SquareSet) -> SquareSet:
        return s

    def mirror(self, s : SquareSet) -> SquareSet:
        return s
