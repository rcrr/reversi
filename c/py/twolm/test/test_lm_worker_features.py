#
# test_lm_worker_features.py
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
# How to use the unit tests lm_worker_features module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_lm_worker_features
#

# twolm/test/test_lm_worker_features.py
import unittest
from unittest.mock import patch
from io import StringIO

import tempfile
import shutil
from pathlib import Path

from twolm.enums import Verbosity
from twolm.logistic_model import LogisticModel


class TestLMWorkerFeatures(unittest.TestCase):
    """Tests for the FEATURES worker basic movements and feature set generation."""

    def setUp(self):
        # Patch stdout to keep test output clean
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.LOW,
                                 base_dir_override=self.tmp_dir)
        
        self.assertEqual(self.rlm.current_step, 0)
        self.assertEqual(self.rlm.current_worker_name, 'CREATED')
        
        # Move to POSITIONS first, as FEATURES depends on the configuration loaded there
        self.rlm.move_to_step('POSITIONS')
        self.assertEqual(self.rlm.current_step, 2)
        self.assertEqual(self.rlm.current_worker_name, 'POSITIONS')

    def tearDown(self):
        self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)

    def test_build_feature_set(self):
        rlm = self.rlm
        
        # Before moving to FEATURES, the feature_set should be None
        self.assertIsNone(rlm.context.feature_set)

        rlm.move_to_step('FEATURES')
        self.assertEqual(rlm.current_step, 3)
        self.assertEqual(rlm.current_worker_name, 'FEATURES')            

        # After moving, the feature_set should be populated
        self.assertIsNotNone(rlm.context.feature_set)


if __name__ == '__main__':
    unittest.main()
