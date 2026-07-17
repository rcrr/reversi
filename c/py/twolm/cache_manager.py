#
# cache_manager.py
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
Utility module to handle computation caching with a 4-step validation pipeline.
It abstracts file I/O, checksum validation, and cache invalidation.
"""

from __future__ import annotations

import hashlib
from pathlib import Path
from typing import Any, Callable, TypeVar

from twolm.enums import Relevance



__all__ = ['cache_manager_load_or_compute']



#: Generic type for the cached data structure
T = TypeVar('T')

#: Type aliases for injected business logic callbacks
ComputeFunc = Callable[[], T]
LoadFunc = Callable[[Path], T]
StoreFunc = Callable[[T, Path], None]
ValidateFunc = Callable[[T], bool]
LoggerFunc = Callable[[Relevance, str], None]


SHA3_256_CHECKSUM_SUFFIX = ".SHA3-256"


def _compute_checksum(file_path: Path) -> str:
    """Compute SHA3-256 checksum of a file."""
    h = hashlib.sha3_256()
    with open(file_path, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()


def _verify_checksum(file_path: Path) -> bool:
    """Step 2: Verify if the checksum file exists and matches the data file."""
    checksum_path = file_path.with_suffix(file_path.suffix + SHA3_256_CHECKSUM_SUFFIX)
    if not checksum_path.exists():
        return False
    
    expected_checksum = checksum_path.read_text().strip()
    actual_checksum = _compute_checksum(file_path)
    
    return expected_checksum == actual_checksum


def _save_checksum(file_path: Path) -> None:
    """Save the checksum file after successful computation and storage."""
    checksum_path = file_path.with_suffix(file_path.suffix + SHA3_256_CHECKSUM_SUFFIX)
    checksum = _compute_checksum(file_path)
    checksum_path.write_text(checksum)


def _delete_cache_files(cache_path: Path) -> None:
    """Safely delete both the data file and its checksum."""
    checksum_path = cache_path.with_suffix(cache_path.suffix + SHA3_256_CHECKSUM_SUFFIX)
    cache_path.unlink(missing_ok=True)
    checksum_path.unlink(missing_ok=True)


def cache_manager_load_or_compute(cache_path: Path,
                                  is_allowed: bool,
                                  load_fn: LoadFunc[T],
                                  store_fn: StoreFunc[T],
                                  validate_fn: ValidateFunc[T],
                                  compute_fn: ComputeFunc[T],
                                  logger_fn: LoggerFunc
                                  ) -> tuple[bool, T]:
    """
    Orchestrate the 4-step cache validation pipeline.
    
    Args:
        cache_path: Path to the cache file.
        is_allowed: Step 0 - Is caching allowed for this step ?
        load_fn: Function to load data from cache_path.
        store_fn: Function to store data to cache_path.
        validate_fn: Step 3 - Business logic to validate loaded data.
        compute_fn: Function to compute data from scratch.
        logger_fn: Logger callable.
        
    Returns:
        A tuple containing:
        - bool: True if data was loaded from cache, False if it was computed.
        - T: The computed or loaded data.
    """
    
    #: Step 0: Is cache allowed?
    if not is_allowed:
        logger_fn(Relevance.INFO, "Cache not allowed by configuration. Forcing computation.")
        return _compute_and_store(cache_path, compute_fn, store_fn, logger_fn)

    #: Step 1: Is cache present?
    if not cache_path.exists():
        logger_fn(Relevance.INFO, f"Cache file not found at '{cache_path}'. Forcing computation.")
        return _compute_and_store(cache_path, compute_fn, store_fn, logger_fn)

    #: Step 2: Is checksum consistent?
    if not _verify_checksum(cache_path):
        logger_fn(Relevance.WARN, f"Checksum mismatch for '{cache_path}'. Deleting cache and recomputing.")
        _delete_cache_files(cache_path)
        return _compute_and_store(cache_path, compute_fn, store_fn, logger_fn)

    #: Step 3: Is application data valid? (Business Logic)
    try:
        cached_data = load_fn(cache_path)
    except Exception as e:
        logger_fn(Relevance.ERROR, f"Failed to load cache file '{cache_path}': {e}. Recomputing.")
        _delete_cache_files(cache_path)
        return _compute_and_store(cache_path, compute_fn, store_fn, logger_fn)

    if not validate_fn(cached_data):
        logger_fn(Relevance.WARN, f"Application data validation failed for '{cache_path}'. Deleting cache and recomputing.")
        _delete_cache_files(cache_path)
        return _compute_and_store(cache_path, compute_fn, store_fn, logger_fn)

    #: All steps passed: return cached data
    logger_fn(Relevance.INFO, f"Cache successfully loaded and validated from '{cache_path}'.")
    return True, cached_data


def _compute_and_store(cache_path: Path, 
                       compute_fn: ComputeFunc[T], 
                       store_fn: StoreFunc[T], 
                       logger_fn: LoggerFunc
                       ) -> tuple[bool, T]:
    """Execute computation, store result, and save checksum."""
    logger_fn(Relevance.INFO, "Executing heavy computation...")
    data = compute_fn()
    
    logger_fn(Relevance.INFO, f"Computation finished. Storing to cache '{cache_path}'...")
    store_fn(data, cache_path)
    
    _save_checksum(cache_path)
    logger_fn(Relevance.INFO, "Cache file and checksum successfully written.")
    
    return False, data
