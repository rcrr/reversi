#
# test_logistic_model.py
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
# How to use the unit tests logistic_model module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_logistic_model
#

# twolm/test/test_logistic_model.py
import unittest
from unittest.mock import MagicMock, patch
from io import StringIO
import tempfile
import os

from twolm.logistic_model import LogisticModel, RLMContext
from twolm.state_machine import Worker
from twolm.enums import Verbosity, Relevance


class TestLogisticModel(unittest.TestCase):
    """Test suite for the LogisticModel facade and RLMContext."""

    def setUp(self):
        """Set up a dummy config file and patch stdout and worker factories."""
        # Patch stdout to keep test output clean
        self.patcher_stdout = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher_stdout.start()

        # Create a dummy config file
        self.temp_dir = tempfile.mkdtemp()
        self.dummy_config_path = os.path.join(self.temp_dir, "dummy_config.json")
        with open(self.dummy_config_path, "w") as f:
            f.write("{}") # Empty JSON is enough for path validation

        # Mock the callables for the dummy workers
        self.config_up_mock = MagicMock()
        self.config_down_mock = MagicMock()
        self.positions_up_mock = MagicMock()
        self.positions_down_mock = MagicMock()

        # Patch the factory functions to return our dummy workers
        self.patcher_config = patch('twolm.logistic_model.lm_worker_config', 
                                    return_value=Worker("CONFIG", self.config_up_mock, self.config_down_mock))
        self.patcher_positions = patch('twolm.logistic_model.lm_worker_positions', 
                                       return_value=Worker("POSITIONS", self.positions_up_mock, self.positions_down_mock))
        
        self.mock_config_factory = self.patcher_config.start()
        self.mock_positions_factory = self.patcher_positions.start()

        # Instantiate the model
        self.model = LogisticModel(
            config_file_path=self.dummy_config_path,
            verbosity=Verbosity.STANDARD
        )

    def tearDown(self):
        """Clean up patches and temporary files."""
        self.patcher_stdout.stop()
        self.patcher_config.stop()
        self.patcher_positions.stop()
        
        if os.path.exists(self.dummy_config_path):
            os.remove(self.dummy_config_path)
        if os.path.exists(self.temp_dir):
            os.rmdir(self.temp_dir)

    def test_context_initialization(self):
        """Test that RLMContext is correctly initialized with paths."""
        self.assertIsInstance(self.model.context, RLMContext)
        self.assertEqual(str(self.model.context.config_file_path), self.dummy_config_path)
        self.assertIsNone(self.model.context.base_dir_override)
        self.assertIsNone(self.model.context.cfg) # Should start empty

    def test_state_machine_initialization(self):
        """Test that the state machine is initialized at step 0 (CREATED)."""
        self.assertEqual(self.model.current_step, 0)
        self.assertEqual(self.model.current_worker_name, "CREATED")
        self.assertEqual(len(self.model.sm.workers), 8)

    def test_move_to_step_delegates_to_state_machine(self):
        """Test that moving to a step triggers the correct mock worker."""
        self.model.move_to_step(1) # Move to CONFIG
        
        self.assertEqual(self.model.current_step, 1)
        self.config_up_mock.assert_called_once_with(self.model.context)
        self.config_down_mock.assert_not_called()

    def test_move_to_step_by_string(self):
        """Test moving using the worker name string."""
        self.model.move_to_step("POSITIONS")
        
        self.assertEqual(self.model.current_step, 2)
        self.config_up_mock.assert_called_once_with(self.model.context)
        self.positions_up_mock.assert_called_once_with(self.model.context)

    def test_invalid_config_path(self):
        """Test that providing a non-existent config file raises FileNotFoundError."""
        with self.assertRaises(FileNotFoundError):
            LogisticModel(config_file_path="non_existent_file.json")

    def test_base_dir_override_validation(self):
        """Test that an invalid base_dir_override raises FileNotFoundError."""
        with self.assertRaises(FileNotFoundError):
            LogisticModel(
                config_file_path=self.dummy_config_path,
                base_dir_override="non_existent_dir"
            )

    def test_facade_methods_exist(self):
        """Verify that facade methods correctly point to the state machine."""
        # We just check they are callable and don't raise exceptions
        try:
            self.model.show_event_log()
            self.model.show_history_of_moves()
        except Exception as e:
            self.fail(f"Facade methods raised an unexpected exception: {e}")


if __name__ == '__main__':
    unittest.main()
