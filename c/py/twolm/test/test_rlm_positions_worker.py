#
# test_rlm_positions_worker.py
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
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_rlm_positions_worker
#

import unittest
from unittest.mock import patch

from io import StringIO

import os
import tempfile
import shutil
import csv
import tempfile

from pathlib import Path

import twolm
import twolm.domain
import twolm.rdata
import twolm.rlmwf

from twolm.domain import *
from twolm.rdata import *
from twolm.rlmwf import *

class TestRLMPositionsWorkerCache(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_01.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=ReversiLogisticModel.Verbosity.LOW,
                                        base_dir_override=self.tmp_dir)
        self.assertEqual(self.rlm.current_level.value, 0)
        self.assertEqual(self.rlm.current_level.name, 'CREATED')
        self.rlm.move_to_level('CONFIG')
        self.assertEqual(self.rlm.current_level.value, 1)
        self.assertEqual(self.rlm.current_level.name, 'CONFIG')

    def tearDown(self):
        if True:
            os.system(f"ls -l {self.tmp_dir}")
            os.system(f"cat {self.tmp_dir}/rlmwf_01_CONFIG.dat")
        shutil.rmtree(self.tmp_dir)

    def test_positions(self):
        rlm = self.rlm

        rlm.verbosity = ReversiLogisticModel.Verbosity.HIGH
        rlm.move_to_level('POSITIONS')
        self.assertEqual(rlm.current_level.value, 2)
        self.assertEqual(rlm.current_level.name, 'POSITIONS')
        
