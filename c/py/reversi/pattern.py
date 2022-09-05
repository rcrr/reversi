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

from abc import ABCMeta, abstractmethod

class _Singleton(ABCMeta):
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(_Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]


class Pattern(object, metaclass = _Singleton):
    
    def __init__(self):
        self.id = -1
        self.name = None
        self.n_instances = -1
        self.n_squares = -1
        self.n_configurations = -1
        self.masks = None
        self.trans_to_principal_f = None
        self.__createsingleton__()

    @abstractmethod
    def __createsingleton__(self):
        pass

    @abstractmethod
    def pack(self, s : SquareSet) -> SquareSet: pass

    @abstractmethod
    def unpack(self, s : SquareSet) -> SquareSet: pass

    @abstractmethod
    def mirror(self, s : SquareSet) -> SquareSet: pass


class Edge(Pattern):
    
    def __createsingleton__(self):
        self.id = 0
        self.name = 'EDGE'
        self.n_instances = 4
        self.n_squares = 8
        self.n_configurations = 6561
        self.masks = [SquareSet.new_from_hex(x) for x in ['00000000000000ff',
                                                          '8080808080808080',
                                                          'ff00000000000000',
                                                          '0101010101010101']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_edge
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_edge
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_vertical()
    

class Corner(Pattern):
    
    def __createsingleton__(self):
        self.id = 1
        self.name = 'CORNER'
        self.n_instances = 4
        self.n_squares = 9
        self.n_configurations = 19683
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000000000070707',
                                                          '0000000000e0e0e0',
                                                          'e0e0e00000000000',
                                                          '0707070000000000']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_corner
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_corner
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()
