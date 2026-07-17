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
import json5
import tempfile
import shutil

from twolm.types import Verbosity
from twolm.rlmwf import ReversiLogisticModel

from pathlib import Path
from io import StringIO

from pydantic import ValidationError



class TestRLMConfigWorker(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=Verbosity.LOW,
                                        base_dir_override=self.tmp_dir)
        self.assertEqual(self.rlm.current_level.value, 0)
        self.assertEqual(self.rlm.current_level.name, 'CREATED')

    def tearDown(self):
        if False:
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
        
#: ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

class TestRLMConfigWorkerValidation(unittest.TestCase):
    """Tests that Pydantic correctly rejects invalid JSON configurations."""

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.valid_json_content = {
            "name": "Test Model",
            "description": "Test",
            "base_dir": "will/be/overwritten",
            "use_cache": True,
            "regab_data_set": {
                "regab_db_connection": {"dbname": "t", "user": "t", "host": "l"},
                "bid": [1], "status": ["CMS"], "ec": 10
            },
            "feature_set": {
                "name": "FSet",
                "intercept": False,
                "mobility_set": {"name": "MSet", "mobility_features": [
                    {"name": "M", "mask": "FFFFFFFFFFFFFFFF", "amask": "0000000000000000"}
                ]},
                "pattern_set": {"name": "PSet", "patterns": [
                    {"name": "P", "mask": "00000000000000FF"}
                ]}
            }
        }

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def _write_json(self, payload: dict) -> Path:
        """Helper to write a JSON payload to a temp file and return its path."""
        file_path = Path(self.tmp_dir) / "test_config.json"
        with open(file_path, "w") as f:
            json5.dump(payload, f)
        return file_path

    def test_invalid_json_syntax(self):
        """Test that malformed JSON raises an exception."""
        file_path = Path(self.tmp_dir) / "bad_syntax.json"
        file_path.write_text("{ this is not valid json }")
        
        rlm = ReversiLogisticModel(file_path, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        with patch('sys.stdout', new=StringIO()):
            with self.assertRaises(Exception): # json5 or pydantic will raise
                rlm.move_to_level('CONFIG')

    def test_invalid_ec_out_of_range(self):
        """Test that ec > 60 is rejected by Pydantic."""
        payload = self.valid_json_content.copy()
        payload['regab_data_set']['ec'] = 99 # Invalid!
        
        rlm = ReversiLogisticModel(self._write_json(payload), verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        with patch('sys.stdout', new=StringIO()):
            with self.assertRaises(ValidationError):
                rlm.move_to_level('CONFIG')

    def test_invalid_status_format(self):
        """Test that status not matching 3 uppercase letters is rejected."""
        payload = self.valid_json_content.copy()
        payload['regab_data_set']['status'] = ["INVALID"]
        
        rlm = ReversiLogisticModel(self._write_json(payload), verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        with patch('sys.stdout', new=StringIO()):
            with self.assertRaises(ValidationError):
                rlm.move_to_level('CONFIG')

    def test_invalid_hex_mask_format(self):
        """Test that a non-hex string in a pattern mask is rejected."""
        payload = self.valid_json_content.copy()
        payload['feature_set']['pattern_set']['patterns'][0]['mask'] = "ZZZZZZZZZZZZZZZZ"
        
        rlm = ReversiLogisticModel(self._write_json(payload), verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        with patch('sys.stdout', new=StringIO()):
            with self.assertRaises(ValidationError):
                rlm.move_to_level('CONFIG')


class TestRLMConfigWorkerBaseDir(unittest.TestCase):
    """Tests the validation of the base_dir_override parameter."""

    def tearDown(self):
        if hasattr(self, 'tmp_dir') and os.path.exists(self.tmp_dir):
            # Restore write permissions before deleting
            os.chmod(self.tmp_dir, 0o755)
            shutil.rmtree(self.tmp_dir)

    def test_base_dir_is_a_file_raises_error(self):
        """Test that passing a file path instead of a directory raises NotADirectoryError."""
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        fake_file = Path(self.tmp_dir) / "I_am_a_file.txt"
        fake_file.touch()
        
        valid_json = Path('py/twolm/test/data/rlm_00.json')
        
        # The check happens in __init__, not in the worker!
        with self.assertRaises(NotADirectoryError) as context:
            rlm = ReversiLogisticModel(valid_json, verbosity=Verbosity.LOW, base_dir_override=fake_file)
            
        self.assertIn("not a directory", str(context.exception).lower())

    def test_base_dir_is_not_writable_raises_error(self):
        """Test that an un-writable directory raises RuntimeError."""
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        os.chmod(self.tmp_dir, 0o444)
        
        valid_json = Path('py/twolm/test/data/rlm_00.json')
        rlm = ReversiLogisticModel(valid_json, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        
        with patch('sys.stdout', new=StringIO()):
            with self.assertRaises(RuntimeError) as context:
                rlm.move_to_level('CONFIG')
        self.assertIn("not writable", str(context.exception))

class TestRLMConfigWorkerCacheLogic(unittest.TestCase):
    """Tests the specific cache/sentinel behaviors of the CONFIG worker."""

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=Verbosity.LOW,
                                        base_dir_override=self.tmp_dir)
        # Expected files generated by CONFIG level
        self.cache_file = Path(self.tmp_dir) / 'rlmwf_01_CONFIG.dat'
        self.checksum_file = Path(self.tmp_dir) / 'rlmwf_01_CONFIG.dat.SHA3-256'

    def tearDown(self):
        shutil.rmtree(self.tmp_dir)

    def test_cache_files_created_on_first_run(self):
        """Test that moving to CONFIG creates both .dat and .SHA3-256 files."""
        self.assertFalse(self.cache_file.exists())
        self.assertFalse(self.checksum_file.exists())
        
        self.rlm.move_to_level('CONFIG')
        
        self.assertTrue(self.cache_file.exists(), "Cache .dat file was not created.")
        self.assertTrue(self.checksum_file.exists(), "Checksum sidecar file was not created.")

    def test_cache_skipped_on_identical_second_run(self):
        """Test that running CONFIG twice does not overwrite the cache files."""
        self.rlm.move_to_level('CONFIG')
        
        # Read initial file stats
        stat_1 = os.stat(self.cache_file)
        sha3_content_1 = self.checksum_file.read_text()
        
        # Go down and up again
        self.rlm.move_to_level('CREATED')
        self.rlm.move_to_level('CONFIG')
        
        stat_2 = os.stat(self.cache_file)
        sha3_content_2 = self.checksum_file.read_text()
        
        # File metadata and content must be identical (not rewritten)
        self.assertEqual(stat_1.st_mtime, stat_2.st_mtime, "Cache .dat was rewritten unnecessarily.")
        self.assertEqual(sha3_content_1, sha3_content_2, "Checksum was rewritten unnecessarily.")

    def test_cache_updated_on_json_change(self):
        """Test that modifying the source JSON updates the cache files."""
        self.rlm.move_to_level('CONFIG')
        sha3_content_1 = self.checksum_file.read_text()
        
        # 1. Create a SAFE copy in the temp directory to avoid destroying the real test file
        original_json_path = Path(self.json_config)
        temp_json_path = Path(self.tmp_dir) / "modified_config.json"
        
        with open(original_json_path, 'r') as f:
            data = json5.load(f)
            
        data['description'] = "MODIFIED FOR TESTING"
        
        # Write ONLY to the temporary copy
        with open(temp_json_path, 'w') as f:
            json5.dump(data, f, indent=4)

        # 2. Re-instantiate and run using the MODIFIED COPY
        rlm2 = ReversiLogisticModel(temp_json_path, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        rlm2.move_to_level('CONFIG')
        sha3_content_2 = self.checksum_file.read_text()
        
        # 3. Assert the checksum changed
        self.assertNotEqual(sha3_content_1, sha3_content_2, "Checksum was not updated after JSON change.")

    def test_cache_corruption_raises_runtime_error(self):
        """Test the SENTINEL behavior: a corrupted .dat file MUST crash the pipeline."""
        self.rlm.move_to_level('CONFIG')
        
        # Simulate disk corruption or external manual edit of the .dat file
        with open(self.cache_file, 'w') as f:
            f.write("CORRUPTED DATA TRUSTED BY NOBODY")
            
        rlm2 = ReversiLogisticModel(self.json_config, verbosity=Verbosity.LOW, base_dir_override=self.tmp_dir)
        
        with patch('sys.stdout', new=StringIO()):
            with self.assertRaises(RuntimeError) as context:
                rlm2.move_to_level('CONFIG')
            
        self.assertIn("not matching", str(context.exception).lower())
