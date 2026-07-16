#
# test_mobility.py
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
# How to use the unit tests mobility module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# time PERF=0 PYTHONPATH="./py" python3 -m unittest twolm.test.test_mobility
#

import unittest

from twolm.board import (Bitboard,
                         Position,
                         bitboard_count,
                         bitboard_from_signed_int,
                         make_position,
                         legal_moves)

from twolm.pattern import Index

from twolm.mobility import (Mobility, MobilitySet,
                            popcount64)

from twolm.rlmwf import ReversiLogisticModel

from twolm.types import Verbosity

import numpy as np
import numpy.testing as nptest

import pandas as pd

import io
import time        
import os
import tempfile
import shutil
import hashlib

from pydantic import ValidationError



class TestMobility(unittest.TestCase):

    def test_init(self):
        m = Mobility('LMC', Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0x0000000000000000))
        self.assertEqual(m.name, 'LMC')
        self.assertEqual(m.mask, Bitboard(0xFFFFFFFFFFFFFFFF))
        self.assertEqual(m.amask, Bitboard(0x0000000000000000))
        self.assertEqual(m.n_configurations, 65) # 64 + 0 + 1
        
        m = Mobility('ALMC', Bitboard(0x0000000000000000), Bitboard(0xFFFFFFFFFFFFFFFF))
        self.assertEqual(m.name, 'ALMC')
        self.assertEqual(m.mask, Bitboard(0x0000000000000000))
        self.assertEqual(m.amask, Bitboard(0xFFFFFFFFFFFFFFFF))
        self.assertEqual(m.n_configurations, 65) # 64 + 0 + 1
        
        m = Mobility('DLMC', Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0xFFFFFFFFFFFFFFFF))
        self.assertEqual(m.name, 'DLMC')
        self.assertEqual(m.mask, Bitboard(0xFFFFFFFFFFFFFFFF))
        self.assertEqual(m.amask, Bitboard(0xFFFFFFFFFFFFFFFF))
        self.assertEqual(m.n_configurations, 129) # 64 + 64 + 1

    def test_init_invalid_type_arg_1_raises_assertion(self):
        with self.assertRaises(ValidationError):
            Mobility(1, Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0x0000000000000000))

    def test_init_invalid_type_arg_2_raises_assertion(self):
        with self.assertRaises(ValidationError):
            Mobility('LMC', 'wrong type', Bitboard(0x0000000000000000))

    def test_init_invalid_type_arg_3_raises_assertion(self):
        with self.assertRaises(ValidationError):
            Mobility('LMC', Bitboard(0xFFFFFFFFFFFFFFFF), 1)

    def test_print(self):
        m = Mobility('LMC', Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0x0000000000000000))
        expected_output = f"[Mobility: name = LMC     , mask = 0xffffffffffffffff, amask = 0x0000000000000000]\n"
        with io.StringIO() as buffer:
            m.print(output=buffer)
            actual_output = buffer.getvalue()
        self.assertEqual(actual_output, expected_output)

    def test_n_configurations_calculation(self):
        """Tests that n_configurations correctly includes the zero-state (+1)."""
        # mask has 2 bits set, amask has 3 bits set.
        # 0 moves, 1 move, 2 moves + 0 anti, 1 anti, 2 anti, 3 anti = 6 total states
        m = Mobility('TEST_CFG', Bitboard(0x0000000000000003), Bitboard(0x0000000000000007))
        self.assertEqual(m.n_configurations, 6) # 2 + 3 + 1
        
class TestMobilitySet(unittest.TestCase):

    def setUp(self):
        self.mobility_set_data = [
            ('LMC',  0xFFFFFFFFFFFFFFFF, 0x0000000000000000),
            ('ALMC', 0x0000000000000000, 0xFFFFFFFFFFFFFFFF),
            ('DLMC', 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF),
        ]
        
        self.mlf = lambda x: [Mobility(n, Bitboard(m), Bitboard(a)) for n, m, a in x]

    def test_init(self):
        mobility_set = MobilitySet('TestMobilitySet', self.mlf(self.mobility_set_data))

        if False:
            print()
            print(f"Mobility Set Name: {mobility_set.name}")
            for i, m in enumerate(mobility_set.mobilities):
                print(f"{i}: {m.name:10s} ({m.mask:016X}, {m.amask:016X})")
            print(f"Hash: {mobility_set.hash}")

    def test_duplicate_name_collision(self):
        """Test that initializing with duplicate mobility names raises a ValueError."""
        ml = [
            Mobility('LMC', Bitboard(0x1), Bitboard(0x2)),
            Mobility('LMC', Bitboard(0x3), Bitboard(0x4))  # Duplicate name
        ]
        with self.assertRaises(ValueError) as context:
            MobilitySet('CollisionSetName', ml)
        self.assertIn('duplicate names', str(context.exception))

    def test_duplicate_key_collision(self):
        """Test that initializing with duplicate (mask, amask) pairs raises a ValueError."""
        ml = [
            Mobility('Mobility1', Bitboard(0x1111), Bitboard(0x2222)),
            Mobility('Mobility2', Bitboard(0x1111), Bitboard(0x2222))  # Duplicate composite key
        ]
        with self.assertRaises(ValueError) as context:
            MobilitySet('CollisionSetKey', ml)
        self.assertIn('duplicate (mask, amask) pairs', str(context.exception))

    def test_hash_equivalence_same_keys(self):
        """Test that two sets with identical keys but different element orders produce the exact same hash."""
        # Order A
        ml_a = [
            Mobility('LMC', Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0x0000000000000000)),
            Mobility('ALMC', Bitboard(0x0000000000000000), Bitboard(0xFFFFFFFFFFFFFFFF))
        ]
        # Order B (reversed initialization)
        ml_b = [
            Mobility('ALMC', Bitboard(0x0000000000000000), Bitboard(0xFFFFFFFFFFFFFFFF)),
            Mobility('LMC', Bitboard(0xFFFFFFFFFFFFFFFF), Bitboard(0x0000000000000000))
        ]
        
        set_a = MobilitySet('SetA', ml_a)
        set_b = MobilitySet('SetB', ml_b)
        
        self.assertEqual(set_a.hash, set_b.hash)

    def test_hash_calculation_verification(self):
        """Manually compute and verify that the SHA-256 hash matches expected structural bytes."""
        ml = [Mobility(n, Bitboard(m), Bitboard(a)) for n, m , a in self.mobility_set_data]
        mobility_set = MobilitySet('TestHash', ml)
        
        # Sort manually to predict hash sequence matching the class implementation
        sorted_ml = sorted(ml, key=lambda x: (x.mask, x.amask))
        
        # Recreate bytes manually
        expected_bytes = b''.join(m.mask.tobytes() + m.amask.tobytes() for m in sorted_ml)
        expected_hash = hashlib.sha256(expected_bytes).hexdigest()
        
        self.assertEqual(mobility_set.hash, expected_hash)

    def test_mobility_sorting_order(self):
        """Test that elements are strictly sorted lexicographically by mask, then by amask."""
        # Unsorted input list
        ml = [
            Mobility('HighMask', Bitboard(0xFFFFFFFF), Bitboard(0x0)),
            Mobility('LowMaskHighAmask', Bitboard(0x11111111), Bitboard(0xFFFFFFFF)),
            Mobility('LowMaskLowAmask', Bitboard(0x11111111), Bitboard(0x00000000))
        ]
        
        mobility_set = MobilitySet('SortingTest', ml)
        
        # Expected sorting sequence: 
        # 1. 0x11111111, 0x00000000
        # 2. 0x11111111, 0xFFFFFFFF
        # 3. 0xFFFFFFFF, 0x00000000
        self.assertEqual(mobility_set.mobilities[0].name, 'LowMaskLowAmask')
        self.assertEqual(mobility_set.mobilities[1].name, 'LowMaskHighAmask')
        self.assertEqual(mobility_set.mobilities[2].name, 'HighMask')

    def test_invalid_mobility_type_in_list(self):
        """Test that non-Mobility objects inside the list trigger a validation failure via @validate_call."""
        ml = [
            Mobility('Valid', Bitboard(0x1), Bitboard(0x2)),
            "NotAMobilityObject"  # Invalid type item
        ]
        # Pydantic @validate_call raises a ValidationError for type violations
        with self.assertRaises(ValidationError):
            MobilitySet('InvalidElementsSet', ml)

    def test_invalid_name_argument_type(self):
        """Test that a non-string 'name' argument triggers a validation failure via @validate_call."""
        ml = [Mobility('Valid', Bitboard(0x1), Bitboard(0x2))]
        
        # Passing an integer instead of a string for the name
        with self.assertRaises(ValidationError):
            MobilitySet(12345, ml)

    def test_names(self):
        mobility_list = self.mlf(self.mobility_set_data)
        ms = MobilitySet("Test", mobility_list)
        computed = ms.names()
        expected = ['ALMC', 'LMC', 'DLMC']
        self.assertEqual(computed, expected)

    def test_masks(self):
        mobility_list = self.mlf(self.mobility_set_data)
        ms = MobilitySet("Test", mobility_list)
        computed = ms.masks()
        expected = np.array([[0x0000000000000000, 0xFFFFFFFFFFFFFFFF],
                             [0xFFFFFFFFFFFFFFFF, 0x0000000000000000],
                             [0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF]], dtype=Bitboard)
        nptest.assert_array_equal(computed, expected)

    def test_print_summary(self):
        ms = MobilitySet("Test", self.mlf(self.mobility_set_data))

        expected_hash = ms.hash
        expected_output = [
            'MobilitySet: name = Test, length = 3, hash = {}',
            '  Mobility: name = ALMC    , mask = 0x0000000000000000, amask = 0xffffffffffffffff',
            '  Mobility: name = LMC     , mask = 0xffffffffffffffff, amask = 0x0000000000000000',
            '  Mobility: name = DLMC    , mask = 0xffffffffffffffff, amask = 0xffffffffffffffff'
        ]
        expected_output[0] = expected_output[0].format(expected_hash)
        expected_output = '\n'.join(expected_output) + '\n'
        
        with io.StringIO() as buffer:
            ms.print_summary(output=buffer)
            actual_output = buffer.getvalue()

        self.assertEqual(actual_output, expected_output)

    def test_compute_indexes(self):
        ms = MobilitySet("Test", self.mlf(self.mobility_set_data))

        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        positions = np.full(1, make_position(mover, opponent))

        indexes = ms.compute_indexes(positions)

        expected_indexes = [53, 13, 66]
        expected_array = np.array([expected_indexes], dtype=Index)
        nptest.assert_array_equal(indexes, expected_array)

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_compute_indexes_performance_1m(self):
        
        N = 1_000_000
        
        ms = MobilitySet("Test", self.mlf(self.mobility_set_data))

        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        positions = np.full(N, make_position(mover, opponent))

        _ = ms.compute_indexes(positions[:100])
        start_time = time.perf_counter()
        indexes = ms.compute_indexes(positions)
        end_time = time.perf_counter()
        duration = end_time - start_time
        positions_per_sec = N / duration
        print(f"\n[PERF MobilitySet.compute_indexes] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")

    def test_popcount64(self):
        data = [
            (0x0000000000000000,  0), # line 0
            (0x0000000000000001,  1),
            (0x0000000000000002,  1),
            (0x0000000000000004,  1),
            (0x0000000000000008,  1),
            (0x0000000000000010,  1),
            (0x0000000000000020,  1),
            (0x0000000000000040,  1),
            (0x0000000000000080,  1),
            (0x0000000000000100,  1),
            (0x8000000000000000,  1), # line 10
            (0x0000000000000003,  2),
            (0x00000000000000FF,  8),
            (0x000000000000FF00,  8),
            (0x0000000000FF0000,  8),
            (0x00000000FF000000,  8),
            (0x000000FF00000000,  8),
            (0x0000FF0000000000,  8),
            (0x00FF000000000000,  8),
            (0xFF00000000000000,  8),
            (0x0101010101010101,  8), # line 20
            (0xFFFFFFFFFFFFFFFF, 64),
            (0x0000000000000007,  3),
            (0x0000000000000070,  3),
            (0x1000000000000030,  3),
            (0x8000000000000001,  2),
            (0x7000000000000007,  6),
            (0x4444444444444444, 16),
            (0x1484211821822141, 16),
            (0x0000000000000300,  2),
            (0x3000000000000000,  2), # line 30
        ]

        for i, element in enumerate(data):
            mask, expected = element
            b = Bitboard(mask)
            computed_a = bitboard_count(b)
            computed_b = popcount64(b)
            if computed_a != expected:
                msg = f"line {i:03}: expected = {expected}, bitboard_count() = {computed_a}"
                print(msg)
                self.assertTrue(False, msg)
            if computed_b != expected:
                msg = f"line {i:03}: expected = {expected}, popcount64() = {computed_b}"
                print(msg)
                self.assertTrue(False, msg)

        # Vectorized testing (Verifies Numba UFunc processing over entire arrays)
        masks_list = [element[0] for element in data]
        expected_list = [element[1] for element in data]

        # Create proper NumPy arrays aligned with Bitboard (uint64) and Index (uint32) types
        bb_array = np.array(masks_list, dtype=Bitboard)
        expected_array = np.array(expected_list, dtype=Index)

        # Call the vectorized popcount function on the entire array batch
        computed_vector = popcount64(bb_array)

        # Ensure the output structure and shapes match exactly
        self.assertEqual(computed_vector.shape, expected_array.shape, "Vectorized output shape mismatch")
        self.assertEqual(computed_vector.dtype, expected_array.dtype, "Vectorized output dtype mismatch")

        # Perform an element-wise array comparison assertion
        np.testing.assert_array_equal(
            computed_vector, 
            expected_array, 
            err_msg="Vectorized popcount computation mismatch on element-by-element verification"
        )

    def test_compute_indexes_prevents_underflow(self):
        """
        Blinds the Numba kernel against 32-bit integer underflows.
        If alms_count > lms_count, the register must not wrap around to ~4 billion.
        """
        # Create a situation where opponent has moves, but mover has ZERO moves in the mask.
        # mask = 0x000000000000000F (A1, B1, C1, D1)
        # amask = 0x000000000000000F (same squares)
        
        ml = [
            Mobility('UNDERFLOW_TEST', Bitboard(0x000000000000000F), Bitboard(0x000000000000000F))
        ]
        ms = MobilitySet('UnderflowSet', ml)
        
        # Craft a board where mover has NO legal moves on A1-D1, but opponent has ONE.
        # Let's put mover on E1 (4), opponent on F1 (5), G1 (6), H1 (7).
        # Mover legal moves will be 0x0000000000000000 (no valid flips possible).
        # Opponent legal moves will be 0x0000000000000008 (D1).
        mover = Bitboard(0x0000000000000010) # E1
        opponent = Bitboard(0x00000000000000E0) # F1, G1, H1
        pos = make_position(mover, opponent)
        positions = np.full(1, pos)
        
        indexes = ms.compute_indexes(positions)
        
        # lms_count = 0
        # alms_count = 1 (bits D1 intersecting the mask)
        # shifts = 4 (popcount of amask)
        # Expected formula: 0 + (4 - 1) = 3
        expected_index = 3
        self.assertEqual(indexes[0, 0], expected_index)

    def test_init_empty_list(self):
        """Test that initializing MobilitySet with an empty list works correctly."""
        ms = MobilitySet('EmptySet', [])
        
        self.assertEqual(ms.name, 'EmptySet')
        self.assertEqual(len(ms.mobilities), 0)
        self.assertEqual(ms.names(), [])
        
        # Verify masks is an empty 2D array with shape (0, 2) and correct dtype
        masks = ms.masks()
        self.assertEqual(masks.shape, (0, 2))
        self.assertEqual(masks.dtype, Bitboard)
        
        # Verify the hash matches an empty byte string (SHA256 of b'')
        expected_hash = hashlib.sha256(b'').hexdigest()
        self.assertEqual(ms.hash, expected_hash)

    def test_compute_indexes_empty_positions(self):
        """Test that computing indexes on an empty position array returns an empty (0, L) array."""
        # Use the standard setup data (3 mobilities)
        ms = MobilitySet('EmptyPosTest', self.mlf(self.mobility_set_data))
        
        # Create an empty structured array matching PositionArray dtype
        positions = np.array([], dtype=Position)
        
        indexes = ms.compute_indexes(positions)
        
        L = len(ms.mobilities) # Should be 3
        self.assertEqual(indexes.shape, (0, L))
        self.assertEqual(indexes.dtype, Index)
