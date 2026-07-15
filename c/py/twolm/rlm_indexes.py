#
# rlm_indexes.py
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

from __future__ import annotations

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from twolm.rlmwf import ReversiLogisticModel

from enum import IntEnum

from twolm.pattern import IndexArray

from typing import Protocol

from pydantic import validate_call, ConfigDict, BeforeValidator, AfterValidator, Field

from pathlib import Path
from twolm import binio



__all__ = ['ReversiLogisticModelIndexes',
           'rlm_indexes_store_to_file',
           'rlm_indexes_load_from_file',
           'rlm_indexes_compute',
           'rlm_indexes_is_cache_consistent']



class ReversiLogisticModelIndexes:
    """
    Represents the 2D index matrix togheter with the hash signature of the feature set
    used to compute them.
    """
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self,
                 feature_set_hash: str,
                 indexes: IndexArray):
        """
        Initializes a RegabDataSet instance.

        Parameters
        ----------
        feature_set_hash : str
            The hash code taken from the feature_set used to compute indexes.
        indexes : IndexArray
        """
        self.feature_set_hash = feature_set_hash
        self.indexes = indexes


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_indexes_store_to_file(
        rlm_indexes: ReversiLogisticModelIndexes,
        filename: str | Path
) -> None:
    """
    Saves the ReversiLogisticModelIndexes instance to a binary file and calculates the SHA3-256 checksum.
    """
    filename = Path(filename)

    description = "ReversiLogisticModelIndexes binary data file"
    version = 1

    with binio.BinaryWriter(filename) as w:

        #: Write header
        w.write_header(description, version)

        #: Write the feature_set_hash
        w.write_string(rlm_indexes.feature_set_hash)

        # Write the data of the 2D indexes matrix
        w.write_array(rlm_indexes.indexes)
    
    return


@validate_call(config=ConfigDict(arbitrary_types_allowed=True))
def rlm_indexes_load_from_file(
        filename: str | Path,
        checksum: bool = True
) -> ReversiLogisticModelIndexes:
    """
    Loads a ReversiLogisticModelIndexes instance from a binary file.
    """

    if checksum:
        is_consistent = binio.verify_sha3_256_sidecar(filename)
        if not is_consistent:
            raise RuntimeError("The checksum file signature doesn't match with current file.")

    expected_description = "ReversiLogisticModelIndexes binary data file"
    expected_version = 1

    with binio.BinaryReader(filename) as r:

        # Read the file header info
        description, version = r.read_header()
        if description != expected_description:
            raise (RuntimeError, f"The file is not a proper {expected_description}.")
        if version != 1:
            raise (RuntimeError, f"The file version is not consistent, found {version}, expected {expected_version}")

        #: Read the feature_set_hash
        feature_set_hash = r.read_string()
        
        # read the data of the 2D indexes matrix
        indexes = r.read_array()

    rlm_indexes = ReversiLogisticModelIndexes(feature_set_hash, indexes)
    
    return rlm_indexes

#: Here validation is not possible, it brings in a convoluted issue with circular imports.
def rlm_indexes_compute(
        model: ReversiLogisticModel
) -> ReversiLogisticModelIndexes:
    """Computes model indexes."""
    feature_set_hash = model.feature_set.hash
    indexes = model.feature_set.compute_indexes(model.positions)
    rlm_indexes = ReversiLogisticModelIndexes(feature_set_hash, indexes)
    return rlm_indexes


def rlm_indexes_is_cache_consistent(
        model: ReversiLogisticModel,
        rlm_indexes: ReversiLogisticModelIndexes
) -> bool:
    """Compare live configuration with cached dataset metadata."""

    cached_feature_set_hash = rlm_indexes.feature_set_hash
    expected_feature_set_hash = model.feature_set.hash
    is_cache_consistent = cached_feature_set_hash == expected_feature_set_hash
    
    return is_cache_consistent
