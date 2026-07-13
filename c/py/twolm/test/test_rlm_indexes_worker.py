#
# test_rlm_indexes_worker.py
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
# How to use the unit tests rlm_indexes_worker module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_rlm_indexes_worker
#

import unittest
from unittest.mock import patch

from io import StringIO

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import os
import tempfile
import shutil
import csv
import tempfile
import time

from pathlib import Path

from twolm.types import *
from twolm.rlmwf import *



class TestRLMIndexesWorker(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=Verbosity.LOW,
                                        base_dir_override=self.tmp_dir)
        self.assertEqual(self.rlm.current_level.value, 0)
        self.assertEqual(self.rlm.current_level.name, 'CREATED')
        self.rlm.move_to_level('FEATURES')
        self.assertEqual(self.rlm.current_level.value, 3)
        self.assertEqual(self.rlm.current_level.name, 'FEATURES')

    def tearDown(self):
        if False:
            os.system(f"ls -l {self.tmp_dir}")
        shutil.rmtree(self.tmp_dir)

    def test_build_indexes(self):
        rlm = self.rlm
        
        #self.assertIsNone(rlm.feature_set)

        rlm.verbosity = Verbosity.HIGH
        rlm.move_to_level('INDEXES')

