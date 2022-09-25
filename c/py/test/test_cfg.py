#
# test_cfg.py
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

import unittest

import reversi
import reversi.cfg
from reversi.cfg import *


class TestCfg(unittest.TestCase):

    def test_init_wrong_arg(self):
        with self.assertRaises(TypeError) as context:
            c = Cfg(None)
        self.assertIsInstance(context.exception, TypeError)

    def test_init_not_existing(self):
        with self.assertRaises(FileNotFoundError) as context:
            c = Cfg('./py/test/data/missing_file.cfg')
        self.assertIsInstance(context.exception, FileNotFoundError)

    def test_init_dir(self):
        with self.assertRaises(FileNotFoundError) as context:
            c = Cfg('./py/test/data')
        self.assertIsInstance(context.exception, FileNotFoundError)

    def test_init_is_not_none(self):
        c = Cfg('./py/test/data/config_for_testing.cfg')
        self.assertIsNotNone(c)
        c.free()

    def test_get(self):
        c = Cfg('./py/test/data/config_for_testing.cfg')
        self.assertIsNotNone(c)
        
        value = c.get('planets','mars')
        self.assertEqual(0.532, float(value))

        value = c.get('moons','io')
        self.assertEqual(0.285, float(value))
        
        value = c.get('some_planets_and_moons','mars')
        self.assertEqual(0.00, float(value))
        
        value = c.get(None,'titan')
        self.assertEqual(0.404, float(value))
        
        value = c.get('stars','uranus')
        self.assertEqual(None, value)
        
        value = c.get('planets','chaos')
        self.assertEqual(None, value)
        
        value = c.get('empty','chaos')
        self.assertEqual(None, value)
        
        value = c.get(None,'chaos')
        self.assertEqual(None, value)

        c.free()
        
        with self.assertRaises(ValueError) as context:
            value = c.get('planets','mars')
        self.assertIsInstance(context.exception, ValueError)
