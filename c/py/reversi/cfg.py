#
# cfg.py
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

import reversi
from reversi import libreversi as libreversi

import ctypes as ct

import os.path
from os import path


class _CfgCTHelper(ct.Structure):
    _fields_ = [("data", ct.c_char_p),
                ("end", ct.c_char_p)]

    
class Cfg:
    """
    A config structure.

    Collects value/key pairs organized by sections.
    """

    __load_key = object()
    
    def __init__(self, load_key, helper):
        """
        Do not use. Internal use only. Use the Cfg.load(filepath) class method instead.
        """
        assert(load_key == Cfg.__load_key), "Cfg objects must be created using the Cfg.load(filepath) method"
        self._helper = helper

    @classmethod
    def load(cls, filepath: str) -> 'Cfg':
        """
        Loads a config file into memory.
        """
        if not isinstance(filepath, str):
            raise TypeError('Argument filepath is not an instance of str')
        if not path.exists(filepath):
            raise FileNotFoundError('Argument filepath is not found')
        if not path.isfile(filepath):
            raise FileNotFoundError('Argument filepath is not a file')
        f = libreversi.cfg_load
        f.restype = ct.POINTER(_CfgCTHelper)
        f.argtypes = [ct.c_char_p]
        helper = f(filepath.encode())
        if not helper:
            raise RuntimeError('The C function returned NULL')
        return Cfg(cls.__load_key, helper)

    def free(self) -> None:
        """
        Free the memory used by the configuration object when we are done with it.
        Calling this function invalidates the object.
        """
        if self._helper:
            f = libreversi.cfg_free
            f.restype = None
            f.argtypes = [ct.c_void_p]
            f(self._helper)
            self._helper = None

    def get(self, section: str, key: str) -> str:
        """
        Returns the value, if found, identified by section  and key.

        Given a section and a key the corresponding value is returned if it exists.
        If the section argument is None then all sections are searched.
        """
        if section:
            if not isinstance(section, str):
                raise TypeError('Argument section is not an instance of str')
        if not isinstance(key, str):
            raise TypeError('Argument key is not an instance of str')
        if not self._helper:
            raise ValueError('The config object has been already freed')
        f = libreversi.cfg_get
        f.restype = ct.c_char_p
        f.argtypes = [ct.POINTER(_CfgCTHelper), ct.c_char_p, ct.c_char_p]
        if section:
            ct_section = ct.c_char_p(section.encode('utf-8'))
        else:
            ct_section = None
        ct_key = ct.c_char_p(key.encode('utf-8'))
        ct_res = f(self._helper, ct_section, ct_key)
        if ct_res:
            result = ct_res.decode("utf-8")
        else:
            result = None
        return result
