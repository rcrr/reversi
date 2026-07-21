#
# feature.py
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

from __future__ import annotations

from typing import Self, TypeAlias, List, Tuple, Union, IO, Annotated, Any

from dataclasses import dataclass, field

import hashlib
import sys
import io

from enum import IntEnum

from twolm.board import PositionArray

from twolm.pattern import (Pattern, PatternSet,
                           Index, IndexArray)

from twolm.mobility import (Mobility, MobilitySet)

import numpy as np

from pydantic import validate_call, ConfigDict, BeforeValidator



__all__ = ['Feature',
           'FeatureSet']



@dataclass(init=False)
class Feature:
    """
    Represents a specific structural evaluation feature for the game engine.
    
    This class enforces encapsulation by completely blocking direct instantiation
    via the default constructor or __init__, forcing clients to use explicitly
    defined factory methods.
    """
    category: Category
    name: str
    n_configurations: int
    n_instances: int
    ref: Mobility | Pattern | None = field(default=None, repr=False)
    
    class Category(IntEnum):
        """
        Enumeration representing the logical classification of the feature.
        """
        INTERCEPT = 0
        MOBILITY  = 1
        PATTERN   = 2

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __new__(cls, *args: Any, **kwargs: Any) -> Self:
        """
        Blocks the client to create new instances of feature via the Feature() call.
        """
        raise RuntimeError(
            "Direct use of __init__ or Feature() is not allowed. "
            "Use the Factory Method instead (eg. Feature.new_from_pattern())."
        )

    @classmethod
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def _create_empty(cls) -> Self:
        """
        Internal utility method used to bypass the block in calling __init__.
        """
        return object.__new__(cls)

    @classmethod
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def new_intercept(cls) -> Self:
        """
        Factory method to create a default INTERCEPT feature.
        
        Returns:
            Feature: A pre-configured feature instance representing an intercept.
        """
        self = cls._create_empty()
        self.category = cls.Category.INTERCEPT
        self.name = "INTERCEPT"
        self.n_configurations = 1
        self.n_instances = 1
        self.ref = None
        return self

    @classmethod
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def new_from_pattern(cls, pattern: Pattern) -> Self:
        """
        Factory method to construct a PATTERN feature from a Pattern instance.
        
        Args:
            pattern: The game Pattern configuration object.
            
        Returns:
            Feature: A validated feature instance linked to the pattern.
        """
        self = cls._create_empty()
        self.category = cls.Category.PATTERN
        self.name = pattern.name
        self.n_configurations = pattern.n_configurations
        self.n_instances = pattern.n_instances
        self.ref = pattern
        return self

    @classmethod
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def new_from_mobility(cls, mobility: Mobility) -> Self:
        """
        Factory method to construct a MOBILITY feature from a Mobility instance.
        
        Args:
            mobility: The game Mobility configuration object.
            
        Returns:
            Feature: A validated feature instance linked to the mobility.
        """
        self = cls._create_empty()
        self.category = cls.Category.MOBILITY
        self.name = mobility.name
        self.n_configurations = mobility.n_configurations
        self.n_instances = mobility.n_instances
        self.ref = mobility
        return self

class FeatureSet:
    """
    The FeatureSet class represents a collection of Feature objects.
    Intercept is kept directly, mobilities and patterns are grouped into
    a MobilitySet and a PatternSet respectively.

    Attributes:
    name (str): A human-readable label for the set of features.
    hash (str): A SHA256 hash of the intercept, mset, and pset attributes, serving as a unique identifier for the set.
    intercept (Feature | None): The intercept feature or none.
    mset (MobilitySet | None): The set of mobility objects.
    pset (PatternSet | None): The set of Pattern objects.
    features (List[Feature]): A list of Feature objects, orderly collecting the intercept, the mobilities and the patterns.
    n_instances (int): Sum of all n_instances collected from each feature.

    Methods:
    print_summary: Prints a summary of the set including the name, hash, and basic feature information.
    """

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def __init__(self,
                 name: str,
                 intercept: Feature | None,
                 mset: MobilitySet | None,
                 pset: PatternSet | None) -> Self:
        """
        Initializes a new FeatureSet instance with the given name and list of features.
        """
        
        if intercept and intercept.category is not Feature.Category.INTERCEPT:
            error_msg = f"Argument intercept is a feature having the wrong category {intercept.category}."
            raise ValueError(error_msg)

        self.name = name
        self.intercept = intercept
        self.mset = mset
        self.pset = pset

        fs = []
        if self.intercept:
            fs.append(self.intercept)
        if self.mset:
            for m in self.mset.mobilities:
                fs.append(Feature.new_from_mobility(m))
        if self.pset:
            for p in self.pset.patterns:
                fs.append(Feature.new_from_pattern(p))
        self.features = fs

        self.n_instances = sum([f.n_instances for f in self.features])

        chunks = []
        if intercept:
            chunks.append(b'INTERCEPT')
        if mset:
            chunks.append(mset.hash.encode('utf-8')) 
        if pset:
            chunks.append(pset.hash.encode('utf-8')) 
        hash_input = b''.join(chunks)
        self.hash = hashlib.sha256(hash_input).hexdigest()
        
        return

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def print_summary(self, output: Union[IO, io.StringIO] = sys.stdout) -> None:
        """
        Prints a summary representation of the feature set to stdout as default, or a specific IO.
        The summary includes the name, hash, and basic feature information.
        """
        prt = lambda msg: print(msg, file=output)
        prt(f"FeatureSet: name = {self.name}, length = {len(self.features)}, hash = {self.hash}")
        if self.intercept:
            prt(f"  Intercept is present.")
        else:
            prt(f"  Intercept is None.")
        if self.mset:
            prt(f"  MobilitySet: name = {self.mset.name}, hash = {self.mset.hash}")
            for i, m in enumerate(self.mset.mobilities):
                prt(f"    {i:02d} name = {m.name:10s}, mask = 0x{m.mask:016X}, amask = 0x{m.amask:016X}")
        else:
            prt(f"  MobilitySet is None.")
        if self.pset:
            prt(f"  PatternSet: name = {self.pset.name}, hash = {self.pset.hash}")
            for i, p in enumerate(self.pset.patterns):
                prt(f"    {i:02d} name = {p.name:10s}, mask = 0x{p.mask:016X}")
        else:
            prt(f"  PatternSet is None.")
        prt(f"  Features: [<i>, <category>, <name>, <n_instances>, <n_configurations>]")
        for i, f in enumerate(self.features):
            prt(f"    {i:02d} {f.category} {f.name:10s} {f.n_instances} {f.n_configurations:10,d}")

    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def compute_indexes(self, positions: PositionArray) -> IndexArray:
        """
        Compute the 2D matrix of indices by horizontally stacking features.
        
        Handles empty edge cases (N=0 or I=0) gracefully by returning 
        an empty NumPy array with the correct expected shape.
        """
        N = len(positions)
        I = sum(f.n_instances for f in self.features)
        
        # Handle empty edge cases immediately to prevent unnecessary processing
        if N == 0 or I == 0:
            return np.empty((N, I), dtype=Index)
        
        # Collect only the active blocks of indices
        active_indexes = []
        
        if self.intercept:
            active_indexes.append(np.zeros((N, 1), dtype=Index))
            
        if self.mset:
            active_indexes.append(self.mset.compute_indexes(positions))
            
        if self.pset:
            active_indexes.append(self.pset.compute_principal_indexes(positions))
            
        # Efficiently merge all sub-arrays horizontally in a single C-level step
        return np.hstack(active_indexes)
