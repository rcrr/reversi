#
# test_cache_manager.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Author Roberto Corradini mailto:rob_corradini@yahoo.it
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

"""
Unit tests for the twolm.cache_manager module.

This suite verifies the 4-step validation pipeline, ensuring that caching,
checksum validation, fallback computation logic, and the cache_hit flag 
behave predictably under various scenarios.
"""

from __future__ import annotations

import unittest
import tempfile
from pathlib import Path
from unittest.mock import MagicMock

from twolm.cache_manager import (
    cache_manager_load_or_compute,
    _compute_checksum,
    _verify_checksum,
    _save_checksum,
    _delete_cache_files,
    SHA3_256_CHECKSUM_SUFFIX,
)
from twolm.enums import Relevance


#: A constant dummy payload to simulate computed/cached data
DUMMY_DATA = {"result": "success", "value": 42}

#: Custom base directory for temporary test files
CUSTOM_TMP_BASE = Path("./build/tmp")


class BaseCacheTestCase(unittest.TestCase):
    """Base test case to handle custom temporary directory setup and teardown."""

    @classmethod
    def setUpClass(cls):
        """Ensure the custom temporary base directory exists."""
        CUSTOM_TMP_BASE.mkdir(parents=True, exist_ok=True)

    def setUp(self):
        """Create a temporary directory inside ./build/tmp/."""
        self.temp_dir = tempfile.TemporaryDirectory(dir=str(CUSTOM_TMP_BASE))
        
    def tearDown(self):
        """Clean up the temporary directory."""
        self.temp_dir.cleanup()


class TestChecksumHelpers(BaseCacheTestCase):
    """Tests for the low-level checksum utility functions."""

    def setUp(self):
        super().setUp()
        self.temp_path = Path(self.temp_dir.name) / "test_data.dat"
        self.temp_path.write_text("Hello, Cache World!")

    def test_compute_checksum_returns_valid_sha3_256(self):
        """Verify that _compute_checksum returns a 64-character hex string (SHA3-256)."""
        checksum = _compute_checksum(self.temp_path)
        self.assertEqual(len(checksum), 64)
        self.assertTrue(all(c in '0123456789abcdef' for c in checksum))

    def test_compute_checksum_is_deterministic(self):
        """Verify that computing the checksum twice yields the same result."""
        checksum1 = _compute_checksum(self.temp_path)
        checksum2 = _compute_checksum(self.temp_path)
        self.assertEqual(checksum1, checksum2)

    def test_save_and_verify_checksum_success(self):
        """Verify that a saved checksum passes the verification step."""
        _save_checksum(self.temp_path)
        self.assertTrue(_verify_checksum(self.temp_path))

    def test_verify_checksum_fails_if_sidecar_missing(self):
        """Verify that verification returns False if the .SHA3-256 file does not exist."""
        self.assertFalse(_verify_checksum(self.temp_path))

    def test_verify_checksum_fails_if_sidecar_corrupted(self):
        """Verify that verification returns False if the sidecar content is altered."""
        checksum_path = self.temp_path.with_suffix(
            self.temp_path.suffix + SHA3_256_CHECKSUM_SUFFIX
        )
        checksum_path.write_text("0" * 64)
        self.assertFalse(_verify_checksum(self.temp_path))

    def test_delete_cache_files_removes_both(self):
        """Verify that _delete_cache_files removes both data and checksum files."""
        _save_checksum(self.temp_path)
        
        self.assertTrue(self.temp_path.exists())
        self.assertTrue(self.temp_path.with_suffix(
            self.temp_path.suffix + SHA3_256_CHECKSUM_SUFFIX
        ).exists())

        _delete_cache_files(self.temp_path)

        self.assertFalse(self.temp_path.exists())
        self.assertFalse(self.temp_path.with_suffix(
            self.temp_path.suffix + SHA3_256_CHECKSUM_SUFFIX
        ).exists())

    def test_delete_cache_files_does_not_raise_if_missing(self):
        """Verify that deletion is idempotent and does not raise FileNotFoundError."""
        _delete_cache_files(self.temp_path)
        _delete_cache_files(self.temp_path)


class TestCacheManagerPipeline(BaseCacheTestCase):
    """
    Tests for the main cache_manager_load_or_compute orchestration function.
    Verifies both the returned data and the cache_hit boolean flag.
    """

    def setUp(self):
        super().setUp()
        self.cache_path = Path(self.temp_dir.name) / "cache.dat"
        
        self.mock_compute = MagicMock(return_value=DUMMY_DATA)
        self.mock_store = MagicMock(side_effect=self._fake_store)
        self.mock_load = MagicMock(return_value=DUMMY_DATA)
        self.mock_validate = MagicMock(return_value=True)
        self.mock_logger = MagicMock()

    @staticmethod
    def _fake_store(data: object, path: Path) -> None:
        """Simulates a real store function by physically writing data to disk."""
        path.write_text(str(data))

    def test_step0_not_allowed_forces_computation(self):
        """
        Step 0: If caching is not allowed, it must compute, store, 
        and return cache_hit=False.
        """
        self.cache_path.write_text("old data")

        cache_hit, result = cache_manager_load_or_compute(
            cache_path=self.cache_path,
            is_allowed=False,
            load_fn=self.mock_load,
            store_fn=self.mock_store,
            validate_fn=self.mock_validate,
            compute_fn=self.mock_compute,
            logger_fn=self.mock_logger
        )

        self.assertFalse(cache_hit, "cache_hit must be False when cache is not allowed.")
        self.assertEqual(result, DUMMY_DATA)
        self.mock_compute.assert_called_once()
        self.mock_store.assert_called_once_with(DUMMY_DATA, self.cache_path)
        self.mock_load.assert_not_called()
        
        self.mock_logger.assert_any_call(
            Relevance.INFO, "Cache not allowed by configuration. Forcing computation."
        )

    def test_step1_missing_file_forces_computation(self):
        """
        Step 1: If cache is allowed but file is missing, 
        it must compute, store, and return cache_hit=False.
        """
        cache_hit, result = cache_manager_load_or_compute(
            cache_path=self.cache_path,
            is_allowed=True,
            load_fn=self.mock_load,
            store_fn=self.mock_store,
            validate_fn=self.mock_validate,
            compute_fn=self.mock_compute,
            logger_fn=self.mock_logger
        )

        self.assertFalse(cache_hit, "cache_hit must be False when file is missing.")
        self.assertEqual(result, DUMMY_DATA)
        self.mock_compute.assert_called_once()
        self.mock_store.assert_called_once()
        self.mock_load.assert_not_called()

    def test_step2_checksum_mismatch_forces_recomputation(self):
        """
        Step 2: If file exists but checksum fails, it must delete files,
        compute, store, and return cache_hit=False.
        """
        self.cache_path.write_text("corrupted data")
        checksum_path = self.cache_path.with_suffix(
            self.cache_path.suffix + SHA3_256_CHECKSUM_SUFFIX
        )
        checksum_path.write_text("0" * 64)

        cache_hit, result = cache_manager_load_or_compute(
            cache_path=self.cache_path,
            is_allowed=True,
            load_fn=self.mock_load,
            store_fn=self.mock_store,
            validate_fn=self.mock_validate,
            compute_fn=self.mock_compute,
            logger_fn=self.mock_logger
        )

        self.assertFalse(cache_hit, "cache_hit must be False on checksum mismatch.")
        self.assertEqual(result, DUMMY_DATA)
        self.mock_compute.assert_called_once()
        self.mock_store.assert_called_once()
        self.mock_load.assert_not_called()
        
        self.mock_logger.assert_any_call(
            Relevance.WARN, f"Checksum mismatch for '{self.cache_path}'. Deleting cache and recomputing."
        )

    def test_step3a_load_exception_forces_recomputation(self):
        """
        Step 3a: If checksum is ok, but load_fn raises an exception,
        it must delete files, compute, store, and return cache_hit=False.
        """
        self.cache_path.write_text("valid data")
        _save_checksum(self.cache_path)

        self.mock_load.side_effect = IOError("Disk read error")

        cache_hit, result = cache_manager_load_or_compute(
            cache_path=self.cache_path,
            is_allowed=True,
            load_fn=self.mock_load,
            store_fn=self.mock_store,
            validate_fn=self.mock_validate,
            compute_fn=self.mock_compute,
            logger_fn=self.mock_logger
        )

        self.assertFalse(cache_hit, "cache_hit must be False when load_fn raises an exception.")
        self.assertEqual(result, DUMMY_DATA)
        self.mock_compute.assert_called_once()
        self.mock_store.assert_called_once()
        
        self.mock_logger.assert_any_call(
            Relevance.ERROR, 
            f"Failed to load cache file '{self.cache_path}': Disk read error. Recomputing."
        )

    def test_step3b_validation_fails_forces_recomputation(self):
        """
        Step 3b: If checksum is ok, load succeeds, but validate_fn returns False,
        it must delete files, compute, store, and return cache_hit=False.
        """
        self.cache_path.write_text("valid data")
        _save_checksum(self.cache_path)

        self.mock_validate.return_value = False

        cache_hit, result = cache_manager_load_or_compute(
            cache_path=self.cache_path,
            is_allowed=True,
            load_fn=self.mock_load,
            store_fn=self.mock_store,
            validate_fn=self.mock_validate,
            compute_fn=self.mock_compute,
            logger_fn=self.mock_logger
        )

        self.assertFalse(cache_hit, "cache_hit must be False when validation fails.")
        self.assertEqual(result, DUMMY_DATA)
        self.mock_compute.assert_called_once()
        self.mock_store.assert_called_once()
        self.mock_validate.assert_called_once_with(DUMMY_DATA)

    def test_success_path_returns_cached_data(self):
        """
        Success Path: If all 4 steps pass, it must return the loaded data
        WITHOUT calling compute or store, and return cache_hit=True.
        """
        self.cache_path.write_text("valid data")
        _save_checksum(self.cache_path)

        cached_payload = {"result": "from_cache"}
        self.mock_load.return_value = cached_payload

        cache_hit, result = cache_manager_load_or_compute(
            cache_path=self.cache_path,
            is_allowed=True,
            load_fn=self.mock_load,
            store_fn=self.mock_store,
            validate_fn=self.mock_validate,
            compute_fn=self.mock_compute,
            logger_fn=self.mock_logger
        )

        self.assertTrue(cache_hit, "cache_hit must be True on successful cache load.")
        self.assertEqual(result, cached_payload)
        self.mock_load.assert_called_once_with(self.cache_path)
        self.mock_validate.assert_called_once_with(cached_payload)
        
        self.mock_compute.assert_not_called()
        self.mock_store.assert_not_called()

        self.mock_logger.assert_any_call(
            Relevance.INFO, f"Cache successfully loaded and validated from '{self.cache_path}'."
        )


if __name__ == '__main__':
    unittest.main()
