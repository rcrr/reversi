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
from reversi.board import _BoardCTHelper

import ctypes as ct
import numpy as np
import re

from abc import ABCMeta, abstractmethod

_c_s_to_s_fun = ct.CFUNCTYPE(ct.c_ulonglong, ct.c_ulonglong)

class _BoardFeatureCTHelper(ct.Structure):
    """
    Objects are read from the libreversi C library by loading the board_features symbol.
    """
    _fields_ = [('id',ct.c_int),
                ('name', ct.c_char*11),
                ('field_cnt', ct.c_uint),
                ('feature_values_f', ct.CFUNCTYPE(None, ct.POINTER(_BoardCTHelper), ct.c_void_p))]

_c_board_feature_count = ct.c_int.in_dll(libreversi, "board_feature_count")
_c_board_features = ct.pointer(_BoardFeatureCTHelper.in_dll(libreversi, 'board_features'))

class _BoardPatternCTHelper(ct.Structure):
    """
    Objects are read from the libreversi C library by loading the board_patterns symbol.
    The class matches the definition in the C code of the type board_pattern_t.
    The field name is an array of bytes, in order to use it as a string:

      pattern_id = 3
      entry = _c_board_patterns[pattern_id]
      name_as_string = entry.name.decode('utf-8')

    To call a C function, e.g. pattern_mirror_f do the following:

      pattern_id = 3
      entry = _c_board_patterns[pattern_id]
      fun = _c_s_to_s_fun(entry.pattern_mirror_f)
      x = fun(y)

    where x and y are SquareSet objects.
    
    """
    _fields_ = [('id', ct.c_int),
                ('name', ct.c_char*7),
                ('n_instances', ct.c_int),
                ('n_squares', ct.c_int),
                ('n_configurations', ct.c_ulonglong),
                ('masks', ct.c_ulonglong*8),
                ('trans_to_principal_f', _c_s_to_s_fun*8),
                ('pattern_pack_f', _c_s_to_s_fun),
                ('pattern_unpack_f', _c_s_to_s_fun),
                ('pattern_mirror_f', _c_s_to_s_fun)]

_c_board_pattern_count = ct.c_int.in_dll(libreversi, "board_pattern_count")
_c_board_patterns = ct.pointer(_BoardPatternCTHelper.in_dll(libreversi, 'board_patterns'))

for i in range(0,  _c_board_pattern_count.value):
    p = _c_board_patterns[i]
    if not p.id == i: raise ValueError('Error reading the libreversi.board_patterns table.')


class _Singleton(ABCMeta):
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super(_Singleton, cls).__call__(*args, **kwargs)
        return cls._instances[cls]

class Feature(object, metaclass = _Singleton):
    """
    """
    def __init__(self):
        self.id = -1
        self.name = None
        self.field_cnt = -1
        self.values = None
        self.__createsingleton__()
        
        self._c_feature = None
        for i in range(0,  _c_board_feature_count.value):
            f = _c_board_features[i]
            if f.id == self.id and f.name.decode('utf-8') == self.name:
                self._c_feature = f
                def feature_values(b: Board) -> np.array:
                    ct_board_p = ct.byref(_BoardCTHelper((b.mover, b.opponent)))
                    ct_values = (ct.c_double*f.field_cnt) ()
                    f.feature_values_f(ct_board_p, ct.byref(ct_values))
                    return np.frombuffer(ct_values, np.double, count = f.field_cnt)
                self.values = feature_values
                break
        if not self._c_feature:
            raise ValueError('Fature id not found in _c_board_features C symbol')

    @abstractmethod
    def __createsingleton__(self): pass


class FIntercept(Feature):
    
    def __createsingleton__(self):
        self.id = 0
        self.name = 'INTERCEPT'
        self.field_cnt = 1


class FMobility(Feature):
    
    def __createsingleton__(self):
        self.id = 1
        self.name = 'MOBILITY'
        self.field_cnt = 1


class FMobility2(Feature):
    
    def __createsingleton__(self):
        self.id = 2
        self.name = 'MOBILITY2'
        self.field_cnt = 2


class FMobility3(Feature):
    
    def __createsingleton__(self):
        self.id = 3
        self.name = 'MOBILITY3'
        self.field_cnt = 3


"""
All defined features in a set.
It is a global variable, it must not be modified.
"""
features_as_set = {FIntercept(), FMobility(), FMobility2(), FMobility3()}

"""
All defined features in a list sorted by id.
It is a global variable, it must not be modified.
"""
features_as_list = list(features_as_set)
features_as_list.sort(key = lambda x: x.id, reverse = False)

"""
All defined features in a dictionary having name as key.
It is a global variable, it must not be modified.
"""
features_as_dict = dict([(x.name, x) for x in features_as_list])


class Pattern(object, metaclass = _Singleton):
    """
    A board pattern is a defined subset of named squares on the board.

    Attributes
    ----------
    id : int
      Unique identifier of the pattern. It must match with the REGAB database definition.
    name : str
      Unique name of the pattern. It must have a lenght of six characters or less.
    n_instances : int
    n_configurations : int
    masks
    trans_to_principal_f

    Methods
    -------
    pack
    unpack
    mirror

    """
    
    def __init__(self):
        """
        Methods pack, unpack and mirror could be defined here instead of the redundant approach.
        """
        self.id = -1
        self.name = None
        self.n_instances = -1
        self.n_squares = -1
        self.n_configurations = -1
        self.masks = None
        self.trans_to_principal_f = None
        self.__createsingleton__()
        
        self._c_pattern = None
        for i in range(0,  _c_board_pattern_count.value):
            p = _c_board_patterns[i]
            if p.id == self.id and p.name.decode('utf-8') == self.name:
                self._c_pattern = p
                break
        if not self._c_pattern:
            raise ValueError('Fature id not found in _c_board_patterns C symbol')

    @abstractmethod
    def __createsingleton__(self): pass

    @abstractmethod
    def pack(self, s : SquareSet) -> SquareSet: pass

    @abstractmethod
    def unpack(self, s : SquareSet) -> SquareSet: pass

    @abstractmethod
    def mirror(self, s : SquareSet) -> SquareSet: pass


class PEdge(Pattern):
    """
    The EDGE pattern has four instances ranging from [0..3]:
    
       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  a b c d e f g h     . . . . . . . a     . . . . . . . .     h . . . . . . .
    2  . . . . . . . .     . . . . . . . b     . . . . . . . .     g . . . . . . .
    3  . . . . . . . .     . . . . . . . c     . . . . . . . .     f . . . . . . .
    4  . . . . . . . .     . . . . . . . d     . . . . . . . .     e . . . . . . .
    5  . . . . . . . .     . . . . . . . e     . . . . . . . .     d . . . . . . .
    6  . . . . . . . .     . . . . . . . f     . . . . . . . .     c . . . . . . .
    7  . . . . . . . .     . . . . . . . g     . . . . . . . .     b . . . . . . .
    8  . . . . . . . .     . . . . . . . h     h g f e d c b a     a . . . . . . .

    """
    
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
    """
    The CORNER pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  a b c . . . . .     . . . . . g d a     . . . . . . . .     . . . . . . . .
    2  d e f . . . . .     . . . . . h e b     . . . . . . . .     . . . . . . . .
    3  g h i . . . . .     . . . . . i f c     . . . . . . . .     . . . . . . . .
    4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
    5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
    6  . . . . . . . .     . . . . . . . .     . . . . . i h g     c f i . . . . .
    7  . . . . . . . .     . . . . . . . .     . . . . . f e d     b e h . . . . .
    8  . . . . . . . .     . . . . . . . .     . . . . . c b a     a d g . . . . .
    
    """

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
    """
    The XEDGE pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  a b c d e f g h     . . . . . . . a     . . . . . . . .     h . . . . . . .
    2  . i . . . . l .     . . . . . . i b     . . . . . . . .     g l . . . . . .
    3  . . . . . . . .     . . . . . . . c     . . . . . . . .     f . . . . . . .
    4  . . . . . . . .     . . . . . . . d     . . . . . . . .     e . . . . . . .
    5  . . . . . . . .     . . . . . . . e     . . . . . . . .     d . . . . . . .
    6  . . . . . . . .     . . . . . . . f     . . . . . . . .     c . . . . . . .
    7  . . . . . . . .     . . . . . . l g     . l . . . . i .     b i . . . . . .
    8  . . . . . . . .     . . . . . . . h     h g f e d c b a     a . . . . . . .

    """
    
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
    """
    The R2 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . . . . . .     . . . . . . a .     . . . . . . . .     . h . . . . . .
    2  a b c d e f g h     . . . . . . b .     . . . . . . . .     . g . . . . . .
    3  . . . . . . . .     . . . . . . c .     . . . . . . . .     . f . . . . . .
    4  . . . . . . . .     . . . . . . d .     . . . . . . . .     . e . . . . . .
    5  . . . . . . . .     . . . . . . e .     . . . . . . . .     . d . . . . . .
    6  . . . . . . . .     . . . . . . f .     . . . . . . . .     . c . . . . . .
    7  . . . . . . . .     . . . . . . g .     h g f e d c b a     . b . . . . . .
    8  . . . . . . . .     . . . . . . h .     . . . . . . . .     . a . . . . . .

    """
    
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
    """
    The R3 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . . . . . .     . . . . . a . .     . . . . . . . .     . . h . . . . .
    2  . . . . . . . .     . . . . . b . .     . . . . . . . .     . . g . . . . .
    3  a b c d e f g h     . . . . . c . .     . . . . . . . .     . . f . . . . .
    4  . . . . . . . .     . . . . . d . .     . . . . . . . .     . . e . . . . .
    5  . . . . . . . .     . . . . . e . .     . . . . . . . .     . . d . . . . .
    6  . . . . . . . .     . . . . . f . .     h g f e d c b a     . . c . . . . .
    7  . . . . . . . .     . . . . . g . .     . . . . . . . .     . . b . . . . .
    8  . . . . . . . .     . . . . . h . .     . . . . . . . .     . . a . . . . .

    """
    
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
    """
    The `R4` pattern has four instances ranging from `[0..3]`:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . . . . . .     . . . . a . . .     . . . . . . . .     . . . h . . . .
    2  . . . . . . . .     . . . . b . . .     . . . . . . . .     . . . g . . . .
    3  . . . . . . . .     . . . . c . . .     . . . . . . . .     . . . f . . . .
    4  a b c d e f g h     . . . . d . . .     . . . . . . . .     . . . e . . . .
    5  . . . . . . . .     . . . . e . . .     h g f e d c b a     . . . d . . . .
    6  . . . . . . . .     . . . . f . . .     . . . . . . . .     . . . c . . . .
    7  . . . . . . . .     . . . . g . . .     . . . . . . . .     . . . b . . . .
    8  . . . . . . . .     . . . . h . . .     . . . . . . . .     . . . a . . . .

    """
    
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
    """
    The DIAG4 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . d . . . .     . . . . a . . .     . . . . . . . .     . . . . . . . .
    2  . . c . . . . .     . . . . . b . .     . . . . . . . .     . . . . . . . .
    3  . b . . . . . .     . . . . . . c .     . . . . . . . .     . . . . . . . .
    4  a . . . . . . .     . . . . . . . d     . . . . . . . .     . . . . . . . .
    5  . . . . . . . .     . . . . . . . .     . . . . . . . a     d . . . . . . .
    6  . . . . . . . .     . . . . . . . .     . . . . . . b .     . c . . . . . .
    7  . . . . . . . .     . . . . . . . .     . . . . . c . .     . . b . . . . .
    8  . . . . . . . .     . . . . . . . .     . . . . d . . .     . . . a . . . .

    """
    
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
    """
    The DIAG5 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . . e . . .     . . . a . . . .     . . . . . . . .     . . . . . . . .
    2  . . . d . . . .     . . . . b . . .     . . . . . . . .     . . . . . . . .
    3  . . c . . . . .     . . . . . c . .     . . . . . . . .     . . . . . . . .
    4  . b . . . . . .     . . . . . . d .     . . . . . . . a     e . . . . . . .
    5  a . . . . . . .     . . . . . . . e     . . . . . . b .     . d . . . . . .
    6  . . . . . . . .     . . . . . . . .     . . . . . c . .     . . c . . . . .
    7  . . . . . . . .     . . . . . . . .     . . . . d . . .     . . . b . . . .
    8  . . . . . . . .     . . . . . . . .     . . . e . . . .     . . . . a . . .

    """
    
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
    """
    The DIAG6 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . . . f . .     . . a . . . . .     . . . . . . . .     . . . . . . . .
    2  . . . . e . . .     . . . b . . . .     . . . . . . . .     . . . . . . . .
    3  . . . d . . . .     . . . . c . . .     . . . . . . . a     f . . . . . . .
    4  . . c . . . . .     . . . . . d . .     . . . . . . b .     . e . . . . . .
    5  . b . . . . . .     . . . . . . e .     . . . . . c . .     . . d . . . . .
    6  a . . . . . . .     . . . . . . . f     . . . . d . . .     . . . c . . . .
    7  . . . . . . . .     . . . . . . . .     . . . e . . . .     . . . . b . . .
    8  . . . . . . . .     . . . . . . . .     . . f . . . . .     . . . . . a . .

    """
    
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
    """
    The DIAG7 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . . . . g .     . a . . . . . .     . . . . . . . .     . . . . . . . .
    2  . . . . . f . .     . . b . . . . .     . . . . . . . a     g . . . . . . .
    3  . . . . e . . .     . . . c . . . .     . . . . . . b .     . f . . . . . .
    4  . . . d . . . .     . . . . d . . .     . . . . . c . .     . . e . . . . .
    5  . . c . . . . .     . . . . . e . .     . . . . d . . .     . . . d . . . .
    6  . b . . . . . .     . . . . . . f .     . . . e . . . .     . . . . c . . .
    7  a . . . . . . .     . . . . . . . g     . . f . . . . .     . . . . . b . .
    8  . . . . . . . .     . . . . . . . .     . g . . . . . .     . . . . . . a .

    """
    
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
    """
    The DIAG8 pattern has two instances ranging [0, 1]:

       a b c d e f g h     a b c d e f g h

    1  . . . . . . . h     a . . . . . . .
    2  . . . . . . g .     . b . . . . . .
    3  . . . . . f . .     . . c . . . . .
    4  . . . . e . . .     . . . d . . . .
    5  . . . d . . . .     . . . . e . . .
    6  . . c . . . . .     . . . . . f . .
    7  . b . . . . . .     . . . . . . g .
    8  a . . . . . . .     . . . . . . . h

    """
    
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
    """
    The 2X5COR pattern has eight instances ranging from [0..7]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  a b c d e . . .     . . . . . . f a     . . . . . . . .     . . . . . . . .
    2  f g h i l . . .     . . . . . . g b     . . . . . . . .     . . . . . . . .
    3  . . . . . . . .     . . . . . . h c     . . . . . . . .     . . . . . . . .
    4  . . . . . . . .     . . . . . . i d     . . . . . . . .     e l . . . . . .
    5  . . . . . . . .     . . . . . . l e     . . . . . . . .     d i . . . . . .
    6  . . . . . . . .     . . . . . . . .     . . . . . . . .     c h . . . . . .
    7  . . . . . . . .     . . . . . . . .     . . . l i h g f     b g . . . . . .
    8  . . . . . . . .     . . . . . . . .     . . . e d c b a     a f . . . . . .


       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . . e d c b a     . . . . . . . .     . . . . . . . .     a f . . . . . .
    2  . . . l i h g f     . . . . . . . .     . . . . . . . .     b g . . . . . .
    3  . . . . . . . .     . . . . . . . .     . . . . . . . .     c h . . . . . .
    4  . . . . . . . .     . . . . . . l e     . . . . . . . .     d i . . . . . .
    5  . . . . . . . .     . . . . . . i d     . . . . . . . .     e l . . . . . .
    6  . . . . . . . .     . . . . . . h c     . . . . . . . .     . . . . . . . .
    7  . . . . . . . .     . . . . . . g b     f g h i l . . .     . . . . . . . .
    8  . . . . . . . .     . . . . . . f a     a b c d e . . .     . . . . . . . .

    """
    
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
    """
    The DIAG3 pattern has four instances ranging from [0..3]:

       a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h

    1  . . c . . . . .     . . . . . a . .     . . . . . . . .     . . . . . . . .
    2  . b . . . . . .     . . . . . . b .     . . . . . . . .     . . . . . . . .
    3  a . . . . . . .     . . . . . . . c     . . . . . . . .     . . . . . . . .
    4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
    5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
    6  . . . . . . . .     . . . . . . . .     . . . . . . . a     c . . . . . . .
    7  . . . . . . . .     . . . . . . . .     . . . . . . b .     . b . . . . . .
    8  . . . . . . . .     . . . . . . . .     . . . . . c . .     . . a . . . . .

    """
    
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


def _compute_pattern_indexes(self, p : Pattern) -> np.ndarray:
    if not isinstance(p, Pattern):
        raise TypeError('Argument p is not an instance of Pattern')
    f = libreversi.board_pattern_compute_indexes
    f.restype = None
    f.argtypes = [ct.POINTER(ct.c_uint16*8), ct.POINTER(_BoardPatternCTHelper), ct.POINTER(_BoardCTHelper)]
    ct_board_p = ct.byref(_BoardCTHelper((self.mover, self.opponent)))
    ct_indexes = (ct.c_uint16*8) (0, 0, 0, 0, 0, 0, 0, 0)
    f(ct.byref(ct_indexes), p._c_pattern, ct_board_p)
    return np.frombuffer(ct_indexes, np.uint16, count = p.n_instances)

setattr(Board, "compute_pattern_indexes", _compute_pattern_indexes)

def _compute_patternlist_indexes(self, pl : list) -> np.ndarray:
    if not isinstance(pl, list):
        raise TypeError('Argument pl is not an instance of list')
    if not all([isinstance(e, Pattern) for e in pl]):
        raise TypeError('Argument pl must have all elements belonging to Pattern type')
    if not pl:
        return None
    return np.concatenate([self.compute_pattern_indexes(p) for p in pl])

setattr(Board, "compute_patternlist_indexes", _compute_patternlist_indexes)

def compute_pattern_principal_indexes(indexes : np.ndarray, p : Pattern) -> np.ndarray:
    if not isinstance(indexes, np.ndarray):
        raise TypeError('Argument indexes is not an instance of numpy.ndarray')
    if not indexes.dtype == 'uint16':
        raise TypeError('Argument indexes must be an array having dtype equal to uint16')
    if not isinstance(p, Pattern):
        raise TypeError('Argument p is not an instance of Pattern')
    if not len(indexes) == p.n_instances:
        raise ValueError('Argument mismatch: len(indexes) must be equal to p.n_instances')
    f = libreversi.board_pattern_compute_principal_indexes
    f.restype = None
    f.argtypes = [ct.POINTER(ct.c_uint16*8), ct.POINTER(ct.c_uint16*8), ct.POINTER(_BoardPatternCTHelper), ct.c_bool]
    ct_principals = (ct.c_uint16*8) (0, 0, 0, 0, 0, 0, 0, 0)
    indexes_padded = np.pad(indexes, (0, 8 - p.n_instances), mode='constant', constant_values=0)
    ct_indexes_p = indexes_padded.ctypes.data_as(ct.POINTER(ct.c_uint16*8))
    f(ct.byref(ct_principals), ct_indexes_p, p._c_pattern, False)
    return np.frombuffer(ct_principals, np.uint16, count = p.n_instances)

def _compute_pattern_principal_indexes(self, p : Pattern) -> (np.ndarray, np.ndarray):
    if not isinstance(p, Pattern):
        raise TypeError('Argument p is not an instance of Pattern')
    f0 = libreversi.board_pattern_compute_indexes
    f0.restype = None
    f0.argtypes = [ct.POINTER(ct.c_uint16*8), ct.POINTER(_BoardPatternCTHelper), ct.POINTER(_BoardCTHelper)]
    ct_board_p = ct.byref(_BoardCTHelper((self.mover, self.opponent)))
    ct_indexes = (ct.c_uint16*8) (0, 0, 0, 0, 0, 0, 0, 0)
    f0(ct.byref(ct_indexes), p._c_pattern, ct_board_p)
    f1 = libreversi.board_pattern_compute_principal_indexes
    f1.restype = None
    f1.argtypes = [ct.POINTER(ct.c_uint16*8), ct.POINTER(ct.c_uint16*8), ct.POINTER(_BoardPatternCTHelper), ct.c_bool]
    ct_principals = (ct.c_uint16*8) (0, 0, 0, 0, 0, 0, 0, 0)
    f1(ct.byref(ct_principals), ct.byref(ct_indexes), p._c_pattern, False)
    return (np.frombuffer(ct_indexes, np.uint16, count = p.n_instances),
            np.frombuffer(ct_principals, np.uint16, count = p.n_instances))

setattr(Board, "compute_pattern_principal_indexes", _compute_pattern_principal_indexes)

def _compute_patternlist_principal_indexes(self, pl : list) -> (np.ndarray, np.ndarray):
    if not isinstance(pl, list):
        raise TypeError('Argument pl is not an instance of list')
    if not all([isinstance(e, Pattern) for e in pl]):
        raise TypeError('Argument pl must have all elements belonging to Pattern type')
    if not pl:
        return None
    idx_and_pri = [self.compute_pattern_principal_indexes(p) for p in pl]
    idx = np.concatenate([i for i, p in idx_and_pri])
    pri = np.concatenate([p for i, p in idx_and_pri])
    return idx, pri

setattr(Board, "compute_patternlist_principal_indexes", _compute_patternlist_principal_indexes)

def board_pattern_packed_to_index(packed : Board, n_squares : int) -> int:
    f = libreversi.board_pattern_packed_to_index
    f.restype = ct.c_uint16
    f.argtypes = [ct.POINTER(_BoardCTHelper), ct.c_uint]
    c_packed_p = ct.byref(_BoardCTHelper((packed.mover, packed.opponent)))
    ret = f(c_packed_p, n_squares)
    return ret

def board_pattern_index_to_packed(index : int) -> Board:
    f = libreversi.board_pattern_index_to_packed
    f.restype = None
    f.argtypes = [ct.POINTER(_BoardCTHelper), ct.c_uint16]
    ct_board = _BoardCTHelper((0, 0))
    ct_board_p = ct.byref(ct_board)
    f(ct_board_p, index)
    return Board(SquareSet(ct_board.square_sets[0]), SquareSet(ct_board.square_sets[1]))
