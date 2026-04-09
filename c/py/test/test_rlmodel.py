#
# test_rlmodel.py
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
# PYTHONPATH="./py" python3 -m unittest test.test_rlmodel
#

import unittest

import twolm
import twolm.domain
import twolm.rdata
import twolm.rlmodel

from twolm.domain import *
from twolm.rdata import *
from twolm.rlmodel import *

from twolm.rlmodel import PatternConfig

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import tempfile
import shutil

from pathlib import Path


class TestReversiLogisticModel(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_init(self):
        json_config = 'py/test/data/tlm/rlmodel_00.json'
        rlm = ReversiLogisticModel(json_config, base_dir_override=self.tmp_dir)

        retrieved_name = rlm.cfg.name
        retrieved_description = rlm.cfg.description
        retrieved_project_dir = rlm.cfg.project_dir

        expected_name = 'RGLM (Logistic) - Unit Test 00 (rlmodel_00.json)'
        expected_description = 'Used for unit testing.'
        expected_base_dir = "It is always different. It is not checked."
        expected_project_dir = Path("UNITTEST")

        self.assertEqual(expected_name, retrieved_name)
        self.assertEqual(expected_description, retrieved_description)
        self.assertEqual(expected_project_dir, retrieved_project_dir)

        self.assertIsNotNone(rlm.cfg.regab_indexed_data_set_cached)
        ridsc = rlm.cfg.regab_indexed_data_set_cached
        self.assertIsNotNone(ridsc.regab_indexed_data_set)
        rids = ridsc.regab_indexed_data_set
        self.assertIsNotNone(rids.regab_data_set_cached)
        rdsc = rids.regab_data_set_cached
        self.assertIsNotNone(rdsc.regab_data_set)
        rds = rdsc.regab_data_set
        self.assertIsNotNone(rds.regab_db_connection)
        conn = rds.regab_db_connection
        self.assertEqual('tst_regab', conn.dbname)
        self.assertEqual('tst_regab', conn.user)
        self.assertEqual('localhost', conn.host)
        self.assertEqual([8], rds.bid)
        self.assertEqual(['CMS', 'CMR'], rds.status)
        self.assertEqual(20, rds.ec)
        self.assertEqual(Path('regab_data_set.dat'), rdsc.filename)
        self.assertIsNotNone(rids.pattern_set)
        pset = rids.pattern_set
        self.assertEqual('BasicPatternSet', pset.name)
        self.assertIsNotNone(pset.patterns)
        self.assertIsInstance(pset.patterns, list)
        self.assertEqual([PatternConfig(name='ELLE', mask='0000000000000107'),
                          PatternConfig(name='EDGE', mask='00000000000000FF')],
                         pset.patterns)
        self.assertEqual(False, rids.has_indexes)
        self.assertEqual(False, rids.has_findexes)
        self.assertEqual(True, rids.has_pindexes)
        self.assertEqual(True, rids.has_lookup)
        self.assertEqual(True, rids.has_revmap)
        self.assertEqual(Path('regab_indexed_data_set.dat'), ridsc.filename)
