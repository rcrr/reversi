#
# test_lm_worker_full_chain.py
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

# twolm/test/test_lm_worker_full_chain.py
import unittest
import os
from unittest import skipUnless
from unittest.mock import patch
from io import StringIO

import tempfile
import shutil
from pathlib import Path

import numpy as np
import numpy.testing as nptest

from twolm.enums import Verbosity
from twolm.logistic_model import LogisticModel


@skipUnless(os.environ.get('LONG') == '1', "Skipping long-running test (set LONG=1 to run)")
class TestFullChainA2050(unittest.TestCase):
    """Tests the full Reversi Logistic Model chain on the A2050 configuration."""

    suppress_stdout_io = False
    
    def setUp(self):
        if self.suppress_stdout_io:
            self.patcher_stdout = patch('sys.stdout', new=StringIO())
            self.mock_stdout = self.patcher_stdout.start()

        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_03.json'
        
        self.rlm = LogisticModel(self.json_config,
                                 verbosity=Verbosity.HIGH,
                                 base_dir_override=self.tmp_dir)
        
    def tearDown(self):
        if self.suppress_stdout_io:        
            self.patcher_stdout.stop()
        shutil.rmtree(self.tmp_dir)


    def test_full_worker_chain(self):
        """Run full chain on the A2050 model."""
        ctx = self.rlm.context
        
        # Run optimization
        self.rlm.move_to_step('VALIDATE')
        self.assertEqual(True, True)


#: ###

if __name__ == '__main__':
    unittest.main()
