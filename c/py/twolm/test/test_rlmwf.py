#
# test_rlmwf.py
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
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_rlmwf
#

import unittest
from unittest.mock import patch, MagicMock

from io import StringIO

import os
import tempfile
import shutil
import csv
import tempfile

from pathlib import Path

from twolm.types import Verbosity, Relevance
from twolm.rlmwf import ReversiLogisticModel

class TestReversiLogisticModelLevel(unittest.TestCase):

    def test_level_inner_class(self):
        l = ReversiLogisticModel.Level(0)
        self.assertEqual(l, 0)
        
        l = ReversiLogisticModel.Level['CREATED']
        self.assertEqual(l, 0)
        self.assertEqual(l.value, 0)
        self.assertEqual(l.name, 'CREATED')
        self.assertEqual(l.description, 'Just created.')

    def test_invalid_level_int_raises_key_error(self):
        with self.assertRaises(ValueError) as context:
            ReversiLogisticModel.Level(100)

    def test_invalid_level_string_raises_key_error(self):
        with self.assertRaises(KeyError) as context:
            ReversiLogisticModel.Level['FAKE']

    def test_level_objects(self):

        levels = ReversiLogisticModel.levels
        self.assertEqual(levels[0], ReversiLogisticModel.Level(0))
        self.assertEqual(levels[0], ReversiLogisticModel.Level['CREATED'])

        names = ReversiLogisticModel.levels_names
        self.assertEqual(names[0], 'CREATED')
        
        lookup = ReversiLogisticModel.levels_lookup
        self.assertEqual(lookup['CREATED'], 0)
        
        descriptions = ReversiLogisticModel.levels_descriptions
        self.assertEqual(descriptions[0], 'Just created.')

class TestReversiLogisticModelInit(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_init(self):
        rlm = ReversiLogisticModel(self.json_config)
        self.assertEqual(rlm.current_level.value, 0)
        self.assertEqual(rlm.current_level.name, 'CREATED')
        self.assertEqual(rlm.current_level.description, 'Just created.')
        
class TestReversiLogisticModelMoveToLevel(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config, base_dir_override=self.tmp_dir)
        self.rlm.verbosity = Verbosity.LOW

    def tearDown(self):
        if False:
            os.system(f"ls -l {self.tmp_dir}")
            os.system(f"cat {self.tmp_dir}/rlmwf_00_CREATED.dat")
        shutil.rmtree(self.tmp_dir)

    def test_move_to_level(self):
        l = ReversiLogisticModel.Level['CREATED']
        self.rlm.move_to_level(l)
        self.assertEqual(self.rlm.current_level, l)

    def test_move_to_level_using_string(self):
        level_name = 'CREATED'
        level = ReversiLogisticModel.Level[level_name]
        self.rlm.move_to_level(level_name)
        self.assertEqual(self.rlm.current_level, level)        

    def test_move_to_level_using_string_lowercase(self):
        level_name = 'Created'
        level = ReversiLogisticModel.Level[level_name.upper()]
        self.rlm.move_to_level(level_name)
        self.assertEqual(self.rlm.current_level, level)        
         
    def test_invalid_level_string_raises_value_error(self):
        with self.assertRaises(ValueError) as context:
            self.rlm.move_to_level('FAKE')
            
    def test_move_up_one_level(self):
        level_name = 'CONFIG'
        self.rlm.move_to_level(level_name)
        self.assertEqual(self.rlm.current_level.name, level_name)
            
    def test_move_up_two_level(self):
        level_name = 'POSITIONS'
        self.rlm.move_to_level(level_name)
        self.assertEqual(self.rlm.current_level.name, level_name)
            
    def test_move_down_one_level(self):
        level_name_0 = 'CREATED'
        level_name_1 = 'CONFIG'
        self.rlm.move_to_level(level_name_1)
        self.assertEqual(self.rlm.current_level.name, level_name_1)
        self.rlm.move_to_level(level_name_0)
        self.assertEqual(self.rlm.current_level.name, level_name_0)

    def test_wrong_argument_value_raises_type_error(self):
        with self.assertRaises(ValueError):
            self.rlm.move_to_level(123)

class TestReversiLogisticModelEventHistory(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config)

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_log(self):
        rlm = self.rlm
        self.assertIsNotNone(rlm.logs)
        self.assertTrue(isinstance(rlm.logs, list))
        self.assertEqual(len(rlm.logs), 1)
        entry = rlm.logs[0]
        self.assertEqual(entry['level'], 0)
        self.assertEqual(entry['level'].name, 'CREATED')
        self.assertEqual(entry['message'], 'ReversiLogisticModel initialized.')

    def test_show_event_log(self):
        rlm = self.rlm
        if False:
            rlm.show_event_log()
        with patch('sys.stdout', new=StringIO()) as fake_out:
            rlm.show_event_log()
            output = fake_out.getvalue()
            self.assertIn("EVENT LOG", output)
            self.assertIn("CREATED", output)
            self.assertIn("ReversiLogisticModel initialized.", output)

        
class TestReversiLogisticModelExportHistoryOfMovesAsCvs(unittest.TestCase):

    def setUp(self):
        level_name_0 = 'CREATED'
        level_name_1 = 'CONFIG'
        
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config, base_dir_override=self.tmp_dir)
        
        self.rlm.verbosity = Verbosity.LOW
        self.rlm.move_to_level(level_name_1)
        self.assertEqual(self.rlm.current_level.name, level_name_1)
        self.rlm.move_to_level(level_name_0)
        self.assertEqual(self.rlm.current_level.name, level_name_0)
        
        self.filename = os.path.join(self.tmp_dir, 'log_moves.csv')

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_write_csv_file(self):
        self.rlm.export_history_of_moves_as_csv(self.filename)
        self.assertTrue(os.path.exists(self.filename), "The CSV file has been not created..")

        with open(self.filename, mode='r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            rows = list(reader)

            self.assertEqual(len(rows), 2, "The count of row is not consistent with moves.")
            first_row = rows[0]
            self.assertEqual(first_row['level'], '1')
            self.assertEqual(first_row['level_name'], 'CONFIG')
            self.assertEqual(first_row['direction'], 'up')
    
            duration = float(first_row['duration_sec'])
            self.assertGreater(duration, 0)

            second_row = rows[1]
            self.assertEqual(second_row['direction'], 'down')
    
            self.assertIn('T', second_row['start_utc'])
            self.assertTrue(second_row['start_utc'].endswith('+00:00'))

        if False:
            os.system(f"ls -l {self.tmp_dir}")
            os.system(f"cat {self.filename}")


class TestReversiLogisticModelHelpers(unittest.TestCase):

    def test_check_config_file_path_not_found(self):
        with self.assertRaises(FileNotFoundError):
            from twolm.rlmwf import check_config_file_path
            check_config_file_path("/fake/path/to/file.json")

    def test_check_base_dir_override_is_not_dir(self):
        # Use an existing file (like this test script itself) to trigger NotADirectoryError
        with self.assertRaises(NotADirectoryError):
            from twolm.rlmwf import check_base_dir_override
            check_base_dir_override(__file__)


class TestReversiLogisticModelWorkerExceptions(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_run_step_catches_and_reraises_exception(self):
        """Test that if a worker fails, it is logged and the exception is re-raised."""
        rlm = ReversiLogisticModel(self.json_config, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        
        # Create a mock level object that mimics the Enum structure
        mock_lvl_obj = MagicMock()
        
        # Create a mock worker that raises an exception when 'up' is called
        mock_worker_instance = MagicMock()
        mock_worker_instance.up.side_effect = RuntimeError("Worker crashed!")
        
        # Attach the worker to the mock level
        mock_lvl_obj.worker = MagicMock(return_value=mock_worker_instance)
        mock_lvl_obj.name = "MOCK_LEVEL"

        # Suppress the print output generated by log_event during the crash
        with patch('sys.stdout', new=StringIO()):
            # 1. Expect the RuntimeError to bubble up
            with self.assertRaises(RuntimeError) as context:
                rlm._run_step(mock_lvl_obj, 'up')
                
        self.assertEqual("Worker crashed!", str(context.exception))
        
        # 2. Verify it was logged internally
        error_logs = [log for log in rlm.logs if log['relevance'] == Relevance.ERROR]
        self.assertEqual(len(error_logs), 1)
        self.assertIn("Worker crashed!", error_logs[0]['message'])


class TestReversiLogisticModelLogPrint(unittest.TestCase):
    """Tests the actual console printing triggered by verbosity logic."""

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_log_prints_when_relevance_is_high(self):
        """Ensure the log_event prints to stdout when relevance (4) >= verbosity (3)."""
        # Verbosity.LOW is 3, Relevance.ERROR is 4. 4 >= 3 is True.
        rlm = ReversiLogisticModel(self.json_config, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        
        with patch('sys.stdout', new=StringIO()) as fake_out:
            rlm.log_event(Relevance.ERROR, "Test print message.")
            output = fake_out.getvalue()
            
        self.assertIn("Test print message.", output)
        self.assertIn("4:3", output)  # Checks Relevance(4) : Verbosity(3)

    def test_log_suppresses_when_relevance_is_low(self):
        """Ensure nothing is printed if relevance (DEBUG) is lower than verbosity (LOW)."""
        # Assuming DEBUG is a low number (e.g., 0) and Verbosity.LOW is 3. 0 >= 3 is False.
        rlm = ReversiLogisticModel(self.json_config, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        
        with patch('sys.stdout', new=StringIO()) as fake_out:
            rlm.log_event(Relevance.DEBUG, "Should not appear.")
            output = fake_out.getvalue()
            
        self.assertEqual(output, "")

    def test_show_history_of_moves_output_format(self):
        """Test that history of moves formats correctly, checking for microseconds."""
        rlm = ReversiLogisticModel(self.json_config, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        rlm.move_to_level('CONFIG')
        
        with patch('sys.stdout', new=StringIO()) as fake_out:
            rlm.show_history_of_moves()
            output = fake_out.getvalue()
            
        self.assertIn("Level", output)
        self.assertIn("CONFIG", output)
        self.assertIn("up", output)
        # Verify the datetime format includes a dot followed by 6 digits (microseconds)
        self.assertRegex(output, r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d{6}')
