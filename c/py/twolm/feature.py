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

from typing import Self, TypeAlias, List, Tuple, Union, IO, Annotated

from dataclasses import dataclass, field

from enum import IntEnum

from twolm.board import *
from twolm.pattern import *

import numpy as np

from pydantic import validate_call, ConfigDict, BeforeValidator



__all__ = ['Feature']



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
    pattern: Pattern | None = field(default=None, repr=False)
    
    class Category(IntEnum):
        """
        Enumeration representing the logical classification of the feature.
        """
        INTERCEPT = 0
        PATTERN   = 1
        MOBILITY  = 2

    def __new__(cls, *args: Any, **kwargs: Any) -> Self:
        """
        Blocks the client to create new instances of feature via the Feature() call.
        """
        raise RuntimeError(
            "Direct use of __init__ or Feature() is not allowed. "
            "Use the Factory Method instead (eg. Feature.new_from_pattern())."
        )

    @classmethod
    def _create_empty(cls) -> Feature:
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
        self.pattern = None
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
            
        Raises:
            TypeError: If the pattern object is not of type Pattern.
            ValueError: If configuration or instance calculations are invalid.
        """
        self = cls._create_empty()
        self.category = cls.Category.PATTERN
        self.name = pattern.name
        self.n_configurations = pattern.n_configurations
        self.n_instances = pattern.n_instances
        self.pattern = pattern
        return self

    @classmethod
    @validate_call(config=ConfigDict(arbitrary_types_allowed=True))
    def new_mobility(cls, name: str, n_configurations: int, n_instances: int) -> Self:
        """
        Factory method to construct a MOBILITY feature with explicit parameters.
        
        Args:
            name: The custom identifier name for the mobility tracker.
            n_configurations: Total unique configurations possible.
            n_instances: Total target evaluation instances.
            
        Returns:
            Feature: A validated mobility feature instance.
            
        Raises:
            TypeError: If argument types do not match expectations.
            ValueError: If counts are equal to or lower than zero.
        """
        if not isinstance(name, str):
            raise TypeError("Argument 'name' must be an instance of str.")
        if not isinstance(n_configurations, int):
            raise TypeError("Argument 'n_configurations' must be an instance of int.")
        if not isinstance(n_instances, int):
            raise TypeError("Argument 'n_instances' must be an instance of int.")
            
        if n_configurations <= 0:
            raise ValueError(f"n_configurations must be positive, got {n_configurations}")
        if n_instances <= 0:
            raise ValueError(f"n_instances must be positive, got {n_instances}")

        self = cls._create_empty()
        self.category = cls.Category.MOBILITY
        self.name = name
        self.n_configurations = n_configurations
        self.n_instances = n_instances
        self.pattern = None
        return self
