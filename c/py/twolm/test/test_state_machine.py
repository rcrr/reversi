#
# test_state_machine.py
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
# How to use the unit tests state_machine module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_state_machine
#

# twolm/test/test_state_machine.py
import unittest
from unittest.mock import MagicMock, patch
from io import StringIO
from datetime import datetime

from twolm.state_machine import StateMachine, Worker, Context
from twolm.enums import Relevance, Verbosity


class TestStateMachine(unittest.TestCase):
    """Test suite for the generic StateMachine infrastructure."""

    def setUp(self):
        """Set up a fresh state machine with 3 mock workers and patch stdout."""
        # Patch stdout globally for all tests in this class to keep output clean
        self.patcher = patch('sys.stdout', new=StringIO())
        self.mock_stdout = self.patcher.start()

        self.context = Context()
        
        # Create mock callables for up and down
        self.up_callables = [MagicMock() for _ in range(3)]
        self.down_callables = [MagicMock() for _ in range(3)]
        
        self.workers = [
            Worker("CREATED", self.up_callables[0], self.down_callables[0]),
            Worker("CONFIG", self.up_callables[1], self.down_callables[1]),
            Worker("POSITIONS", self.up_callables[2], self.down_callables[2])
        ]
        
        self.sm = StateMachine(self.workers, self.context)

    def tearDown(self):
        """Stop patching stdout."""
        self.patcher.stop()

    def test_initialization(self):
        """Test that the state machine initializes correctly at step 0."""
        self.assertEqual(self.sm.current_step, 0)
        self.assertEqual(self.sm.current_worker_name, "CREATED")
        self.assertEqual(len(self.sm.workers), 3)
        self.assertEqual(len(self.sm.logs), 1)  # The init debug log

    def test_move_up_single_step(self):
        """Test moving up a single step."""
        self.sm.move_to_step(1)
        
        self.assertEqual(self.sm.current_step, 1)
        self.up_callables[1].assert_called_once_with(self.context)
        self.down_callables[1].assert_not_called()
        self.assertEqual(len(self.sm.history), 1)
        self.assertEqual(self.sm.history[0].direction, 'up')

    def test_move_up_multiple_steps(self):
        """Test moving up multiple steps in one call."""
        self.sm.move_to_step(2)
        
        self.assertEqual(self.sm.current_step, 2)
        self.up_callables[1].assert_called_once_with(self.context)
        self.up_callables[2].assert_called_once_with(self.context)
        self.down_callables[1].assert_not_called()
        self.down_callables[2].assert_not_called()
        self.assertEqual(len(self.sm.history), 2)

    def test_move_down_single_step(self):
        """Test moving down a single step."""
        # First go up to step 2
        self.sm.move_to_step(2)
        # Reset mocks to only observe the downward movement
        for mock in self.up_callables + self.down_callables:
            mock.reset_mock()
            
        self.sm.move_to_step(1)
        
        self.assertEqual(self.sm.current_step, 1)
        self.down_callables[2].assert_called_once_with(self.context)
        self.up_callables[2].assert_not_called()
        self.assertEqual(len(self.sm.history), 3)
        self.assertEqual(self.sm.history[-1].direction, 'down')

    def test_move_down_to_zero(self):
        """Test moving down to the first step (index 0)."""
        self.sm.move_to_step(2)
        for mock in self.up_callables + self.down_callables:
            mock.reset_mock()
            
        self.sm.move_to_step(0)
        
        self.assertEqual(self.sm.current_step, 0)
        self.down_callables[2].assert_called_once_with(self.context)
        self.down_callables[1].assert_called_once_with(self.context)
        self.assertEqual(len(self.sm.history), 4)

    def test_move_to_same_step(self):
        """Test that moving to the current step does nothing."""
        self.sm.move_to_step(0)
        
        self.assertEqual(self.sm.current_step, 0)
        for mock in self.up_callables + self.down_callables:
            mock.assert_not_called()
        self.assertEqual(len(self.sm.history), 0)

    def test_move_to_step_by_string(self):
        """Test moving to a step using the worker's name (case-insensitive)."""
        self.sm.move_to_step("positions")
        
        self.assertEqual(self.sm.current_step, 2)
        self.up_callables[1].assert_called_once_with(self.context)
        self.up_callables[2].assert_called_once_with(self.context)

    def test_move_to_step_by_invalid_string(self):
        """Test that providing an invalid string name raises ValueError."""
        with self.assertRaises(ValueError) as context:
            self.sm.move_to_step("NON_EXISTENT_WORKER")
        
        self.assertIn("not found in the pipeline", str(context.exception))

    def test_move_to_step_out_of_bounds_high(self):
        """Test that providing an index >= len(workers) raises IndexError."""
        with self.assertRaises(IndexError):
            self.sm.move_to_step(3)

    def test_move_to_step_out_of_bounds_negative(self):
        """Test that providing a negative index raises IndexError."""
        with self.assertRaises(IndexError):
            self.sm.move_to_step(-1)

    def test_move_to_step_invalid_type(self):
        """Test that providing an invalid type raises TypeError."""
        with self.assertRaises(TypeError):
            self.sm.move_to_step(3.14)

    def test_worker_exception_propagation_and_logging(self):
        """Test that exceptions in workers are logged and re-raised."""
        # Make the 'up' callable of step 1 raise an exception
        self.up_callables[1].side_effect = RuntimeError("Simulated computation failure")
        
        with self.assertRaises(RuntimeError):
            self.sm.move_to_step(2)
            
        # The state machine should still be at step 1 because step 1 failed during execution
        self.assertEqual(self.sm.current_step, 1)
        
        # Check that an error log was generated
        error_logs = [log for log in self.sm.logs if log['relevance'] == Relevance.ERROR]
        self.assertEqual(len(error_logs), 1)
        self.assertIn("Simulated computation failure", error_logs[0]['message'])
        self.assertEqual(error_logs[0]['worker_name'], "CONFIG")
        self.assertEqual(error_logs[0]['step'], 1)
        
        # History should still record the attempted move
        self.assertEqual(len(self.sm.history), 1)
        self.assertEqual(self.sm.history[0].worker_name, "CONFIG")

    def test_history_export_csv(self):
        """Test exporting history to a CSV file."""
        import os
        import csv
        
        self.sm.move_to_step(2)
        self.sm.move_to_step(0)
        
        test_filename = "test_history_export.csv"
        try:
            self.sm.export_history_of_moves_as_csv(test_filename)
            
            self.assertTrue(os.path.exists(test_filename))
            
            with open(test_filename, "r") as f:
                reader = csv.DictReader(f)
                rows = list(reader)
                
            # We made 2 move_to_step calls, resulting in 4 individual steps
            self.assertEqual(len(rows), 4)
            self.assertEqual(rows[0]['worker_name'], "CONFIG")
            self.assertEqual(rows[0]['direction'], "up")
            # When moving down to 0, the last worker to execute 'down' is step 1 (CONFIG)
            self.assertEqual(rows[3]['worker_name'], "CONFIG")
            self.assertEqual(rows[3]['direction'], "down")
        finally:
            if os.path.exists(test_filename):
                os.remove(test_filename)


if __name__ == '__main__':
    unittest.main()
