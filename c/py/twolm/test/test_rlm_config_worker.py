#
# test_rlm_config_worker.py
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
# How to use the unit tests rlmodel module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_rlm_config_worker
#

import unittest
from unittest.mock import patch

import os
import tempfile
import shutil

from twolm.rlmwf import *



class TestRLMConfigWorker(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_01.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=ReversiLogisticModel.Verbosity.HIGH,
                                        base_dir_override=self.tmp_dir)
        self.assertEqual(self.rlm.current_level.value, 0)
        self.assertEqual(self.rlm.current_level.name, 'CREATED')

    def tearDown(self):
        if True:
            print()
            print(f"self.tmp_dir = {self.tmp_dir}")
            os.system(f"ls -l {self.tmp_dir}")
        shutil.rmtree(self.tmp_dir)

    def test_move_up(self):
        self.rlm.move_to_level('CONFIG')
        self.assertEqual(self.rlm.current_level.value, 1)
        self.assertEqual(self.rlm.current_level.name, 'CONFIG')

    def test_move_down(self):
        rlm = self.rlm
        rlm.move_to_level('CONFIG')
        self.assertEqual(rlm.current_level.value, 1)
        self.assertEqual(rlm.current_level.name, 'CONFIG')
        rlm.move_to_level('CREATED')
        self.assertEqual(rlm.current_level.value, 0)
        self.assertEqual(rlm.current_level.name, 'CREATED')

    def test_move_up_with_cache(self):
        rlm = self.rlm
        rlm.move_to_level('CONFIG')
        self.assertEqual(rlm.current_level.value, 1)
        self.assertEqual(rlm.current_level.name, 'CONFIG')
        rlm.move_to_level('CREATED')
        self.assertEqual(rlm.current_level.value, 0)
        self.assertEqual(rlm.current_level.name, 'CREATED')
        rlm.move_to_level('CONFIG')
        self.assertEqual(rlm.current_level.value, 1)
        self.assertEqual(rlm.current_level.name, 'CONFIG')

    def test_properties(self):
        rlm = self.rlm
        rlm.move_to_level('CONFIG')
        self.assertEqual(rlm.current_level.value, 1)
        self.assertEqual(rlm.current_level.name, 'CONFIG')

        if False:
            print(f"{rlm.cfg.properties}")

        number = rlm.cfg.properties.get('number')
        expected_number = 7
        self.assertEqual(number, expected_number)

        string = rlm.cfg.properties.get('string')
        expected_string = 'A string'
        self.assertEqual(string, expected_string)

        is_on = rlm.cfg.properties.get('is_on')
        expected_is_on = False
        self.assertEqual(is_on, expected_is_on)

        value = rlm.cfg.properties.get('missing_key')
        expected_value = None
        self.assertEqual(value, expected_value)
        
