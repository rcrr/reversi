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
    """
    """
    
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


class PEdge(Pattern):
    
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
    

class PCorner(Pattern):
    
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
    

class PXedge(Pattern):
    
    def __createsingleton__(self):
        self.id = 2
        self.name = 'XEDGE'
        self.n_instances = 4
        self.n_squares = 10
        self.n_configurations = 59049
        self.masks = [SquareSet.new_from_hex(x) for x in ['00000000000042ff',
                                                          '80c080808080c080',
                                                          'ff42000000000000',
                                                          '0103010101010301']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_xedge
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_xedge
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_vertical()
    

class PR2(Pattern):
    
    def __createsingleton__(self):
        self.id = 3
        self.name = 'R2'
        self.n_instances = 4
        self.n_squares = 8
        self.n_configurations = 6561
        self.masks = [SquareSet.new_from_hex(x) for x in ['000000000000ff00',
                                                          '4040404040404040',
                                                          '00ff000000000000',
                                                          '0202020202020202']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_r2
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_r2
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_vertical()
    

class PR3(Pattern):
    
    def __createsingleton__(self):
        self.id = 4
        self.name = 'R3'
        self.n_instances = 4
        self.n_squares = 8
        self.n_configurations = 6561
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000000000ff0000',
                                                          '2020202020202020',
                                                          '0000ff0000000000',
                                                          '0404040404040404']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_r3
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_r3
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_vertical()


class PR4(Pattern):
    
    def __createsingleton__(self):
        self.id = 5
        self.name = 'R4'
        self.n_instances = 4
        self.n_squares = 8
        self.n_configurations = 6561
        self.masks = [SquareSet.new_from_hex(x) for x in ['00000000ff000000',
                                                          '1010101010101010',
                                                          '000000ff00000000',
                                                          '0808080808080808']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_r4
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_r4
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_vertical()


class PDiag4(Pattern):
    
    def __createsingleton__(self):
        self.id = 6
        self.name = 'DIAG4'
        self.n_instances = 4
        self.n_squares = 4
        self.n_configurations = 81
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000000001020408',
                                                          '0000000080402010',
                                                          '1020408000000000',
                                                          '0804020100000000']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_diag4
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_diag4
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()


class PDiag5(Pattern):
    
    def __createsingleton__(self):
        self.id = 7
        self.name = 'DIAG5'
        self.n_instances = 4
        self.n_squares = 5
        self.n_configurations = 243
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000000102040810',
                                                          '0000008040201008',
                                                          '0810204080000000',
                                                          '1008040201000000']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_diag5
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_diag5
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()


class PDiag6(Pattern):
    
    def __createsingleton__(self):
        self.id = 8
        self.name = 'DIAG6'
        self.n_instances = 4
        self.n_squares = 6
        self.n_configurations = 729
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000010204081020',
                                                          '0000804020100804',
                                                          '0408102040800000',
                                                          '2010080402010000']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_diag6
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_diag6
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()


class PDiag7(Pattern):
    
    def __createsingleton__(self):
        self.id = 9
        self.name = 'DIAG7'
        self.n_instances = 4
        self.n_squares = 7
        self.n_configurations = 2187
        self.masks = [SquareSet.new_from_hex(x) for x in ['0001020408102040',
                                                          '0080402010080402',
                                                          '0204081020408000',
                                                          '4020100804020100']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_diag7
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_diag7
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()


class PDiag8(Pattern):
    
    def __createsingleton__(self):
        self.id = 10
        self.name = 'DIAG8'
        self.n_instances = 2
        self.n_squares = 8
        self.n_configurations = 6561
        self.masks = [SquareSet.new_from_hex(x) for x in ['0102040810204080',
                                                          '8040201008040201']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_diag8
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_diag8
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()


class P2x5cor(Pattern):
    
    def __createsingleton__(self):
        self.id = 11
        self.name = '2X5COR'
        self.n_instances = 8
        self.n_squares = 10
        self.n_configurations = 59049
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000000000001f1f',
                                                          '000000c0c0c0c0c0',
                                                          'f8f8000000000000',
                                                          '0303030303000000',
                                                          '000000000000f8f8',
                                                          'c0c0c0c0c0000000',
                                                          '1f1f000000000000',
                                                          '0000000303030303']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c,
                                     SquareSet.trans_flip_vertical,
                                     SquareSet.trans_flip_diag_h1a8,
                                     SquareSet.trans_flip_horizontal,
                                     SquareSet.trans_flip_diag_a1h8]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_2x5cor
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_2x5cor
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_identity()


class PDiag3(Pattern):
    
    def __createsingleton__(self):
        self.id = 12
        self.name = 'DIAG3'
        self.n_instances = 4
        self.n_squares = 3
        self.n_configurations = 27
        self.masks = [SquareSet.new_from_hex(x) for x in ['0000000000010204',
                                                          '0000000000804020',
                                                          '2040800000000000',
                                                          '0402010000000000']]
        self.trans_to_principal_f = [SquareSet.trans_identity,
                                     SquareSet.trans_rotate_90a,
                                     SquareSet.trans_rotate_180,
                                     SquareSet.trans_rotate_90c]

    def pack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_pack_diag3
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def unpack(self, s : SquareSet) -> SquareSet:
        f = libreversi.board_pattern_unpack_diag3
        f.restype = ct.c_ulonglong
        f.argtypes = [ct.c_ulonglong]
        return SquareSet(f(s))

    def mirror(self, s : SquareSet) -> SquareSet:
        return s.trans_flip_diag_a1h8()



"""
All defined patterns in a set.
It is a global variable, it must not be modified.
"""
patterns_as_set = {PEdge(), PCorner(), PXedge(), PR2(), PR3(), PR4(), PDiag4(), PDiag5(), PDiag6(), PDiag7(), PDiag8(), P2x5cor(), PDiag3()}

"""
All defined patterns in a list sorted by id.
It is a global variable, it must not be modified.
"""
patterns_as_list = list(patterns_as_set)
patterns_as_list.sort(key = lambda x: x.id, reverse = False)

"""
All defined patterns in a dictionary having name as key.
It is a global variable, it must not be modified.
"""
patterns_as_dict = dict([(x.name, x) for x in patterns_as_list])
