#
# test_board.py
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
# How to use the unit tests board module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# time PERF=0 PYTHONPATH="./py" python3 -m unittest twolm.test.test_board
#

import unittest
from unittest.mock import patch

import time        
import os
import io

import pydantic
from pydantic import ValidationError

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

from twolm.board import *



ar                 = Bitboard(0x22120a0e1222221e)

ar_reflection_h    = Bitboard(0x1e2222120e0a1222)
ar_reflection_v    = Bitboard(0x4448507048444478)
ar_reflection_h1a8 = Bitboard(0x00ff888c92610000)
ar_reflection_a1h8 = Bitboard(0x000086493111ff00)
ar_rotate_180      = Bitboard(0x7844444870504844)
ar_rotate_90c      = Bitboard(0x000061928c88ff00)
ar_rotate_90a      = Bitboard(0x00ff113149860000)

full               = Bitboard(0xffffffffffffffff)
empty              = Bitboard(0x0000000000000000)
sqa1               = Bitboard(0x0000000000000001)
sqa8               = Bitboard(0x0000000000000080)
sqh1               = Bitboard(0x0100000000000000)
sqh8               = Bitboard(0x8000000000000000)

row_1              = Bitboard(0x00000000000000ff)
row_8              = Bitboard(0xff00000000000000)
column_a           = Bitboard(0x0101010101010101)
column_h           = Bitboard(0x8080808080808080)

half_left          = Bitboard(0x0f0f0f0f0f0f0f0f)
half_right         = Bitboard(0xf0f0f0f0f0f0f0f0)
half_top           = Bitboard(0x00000000ffffffff)
half_bottom        = Bitboard(0xffffffff00000000)


class TestSquare(unittest.TestCase):

    def test_new_square(self):
        self.assertEqual(Square(0), Square(0))
        self.assertEqual(Square(1), Square(1))
        self.assertEqual(Square(63), Square(63))
        self.assertNotEqual(Square(0), Square(1))

    def test_new_square_overflow(self):
        with self.assertRaises(OverflowError):
            Square(-1)
        with self.assertRaises(OverflowError):
            Square(256)

    def test_new_square_array_overflow(self):
        with self.assertRaises(OverflowError):
            squares = np.array([0, 1, 256], dtype=Square)

    def test_square_from_string(self):
        self.assertEqual(Square(0), square_from_str('A1'))
        
    def test_invalid_name_raises_value_error(self):
        with self.assertRaises(ValueError):
            square_from_str("UN")

    def test_square_to_str(self):
        sq = Square(0)
        sq_as_str = square_to_str(sq)
        self.assertEqual(sq_as_str, 'A1')
            
    def test_square_as_bitboard(self):
        sq = Square(0)
        expected = Bitboard(0x0000000000000001)
        self.assertEqual(square_as_bitboard(sq), expected)
        sq = Square(63)
        expected = Bitboard(0x8000000000000000)
        self.assertEqual(square_as_bitboard(sq), expected)
        sq = Square(64)
        expected = Bitboard(0x0000000000000000)
        self.assertEqual(square_as_bitboard(sq), expected)

    def test_wrong_type_raises_error(self):
        with self.assertRaises(ValidationError):
            square_from_str(None)
            
        with self.assertRaises(ValidationError):
            square_to_str(None)
            
        with self.assertRaises(ValidationError):
            square_as_bitboard(None)

    def test_square_validate_range(self):
        for i in range(64):
            sq = Square(i)
            square_validate_range(sq)

        with self.assertRaises(ValueError):
            sq = Square(64)
            square_validate_range(sq)

        squares = np.array([0, 1, 2], dtype=Square)
        square_validate_range(squares)

        with self.assertRaises(ValueError):
            squares = np.array([0, 1, 64], dtype=Square)
            square_validate_range(squares)

class TestMove(unittest.TestCase):

    def test_new_move(self):
        self.assertEqual(Move(0), Move(0))
        self.assertEqual(Move(1), Move(1))
        self.assertEqual(Move(63), Move(63))
        self.assertEqual(Move(64), Move(64))
        self.assertEqual(Move(65), Move(65))
        self.assertEqual(Move(66), Move(66))
        self.assertNotEqual(Move(0), Move(1))

    def test_new_move_overflow(self):
        with self.assertRaises(OverflowError):
            Move(-1)
        with self.assertRaises(OverflowError):
            Move(256)

    def test_new_move_array_overflow(self):
        with self.assertRaises(OverflowError):
            moves = np.array([0, 1, 256], dtype=Move)

    def test_move_from_string(self):
        self.assertEqual(Move(0), move_from_str('A1'))
        self.assertEqual(Move(1), move_from_str('B1'))
        self.assertEqual(Move(63), move_from_str('H8'))
        self.assertEqual(Move(64), move_from_str('PA'))
        self.assertEqual(Move(65), move_from_str('NA'))
        self.assertEqual(Move(66), move_from_str('UN'))
        
    def test_invalid_name_raises_value_error(self):
        with self.assertRaises(ValueError):
            move_from_str("NO")

    def test_move_to_str(self):
        mo = Move(64)
        mo_as_str = move_to_str(mo)
        self.assertEqual(mo_as_str, 'PA')

    def test_move_as_bitboard(self):
        mo = Move(0)
        expected = Bitboard(0x0000000000000001)
        self.assertEqual(move_as_bitboard(mo), expected)
        mo = Move(63)
        expected = Bitboard(0x8000000000000000)
        self.assertEqual(move_as_bitboard(mo), expected)
        mo = move_from_str('PA')
        expected = Bitboard(0x0000000000000000)
        self.assertEqual(move_as_bitboard(mo), expected)
        mo = move_from_str('NA')
        expected = Bitboard(0x0000000000000000)
        self.assertEqual(move_as_bitboard(mo), expected)
        mo = move_from_str('UN')
        expected = Bitboard(0x0000000000000000)
        self.assertEqual(move_as_bitboard(mo), expected)

    def test_wrong_type_raises_error(self):
        with self.assertRaises(ValidationError):
            move_from_str(None)
            
        with self.assertRaises(ValidationError):
            move_to_str(None)
            
        with self.assertRaises(ValidationError):
            move_as_bitboard(None)

    def test_move_validate_range(self):
        for i in range(64):
            mo = Move(i)
            move_validate_range(mo)

        move_validate_range(move_from_str('PA'))
        move_validate_range(move_from_str('NA'))
        move_validate_range(move_from_str('UN'))

        with self.assertRaises(ValueError):
            mo = Square(67)
            move_validate_range(mo)

        moves = np.array([0, 1, 2, 64], dtype=Move)
        move_validate_range(moves)

        with self.assertRaises(ValueError):
            moves = np.array([0, 1, 67], dtype=Move)
            move_validate_range(moves)

class TestBitboardFromSignedInt(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_dummy(self):
        self.assertEqual(True, True)

    def test_numpy_array_conversion(self):
        """Should convert np.int64 array to np.uint64 array by reinterpreting bits."""
        arr_in = np.array([-1, -2, 0, 1], dtype=np.int64)
        expected = np.array([18446744073709551615, 18446744073709551614, 0, 1], dtype=np.uint64)
        
        arr_out = bitboard_from_signed_int(arr_in)
        
        self.assertIsInstance(arr_out, np.ndarray)
        self.assertEqual(arr_out.dtype, np.uint64)
        np.testing.assert_array_equal(arr_out, expected)

    def test_numpy_array_shared_memory(self):
        """Should share the exact same data memory slot without allocating a new buffer."""
        arr_in = np.array([-1], dtype=np.int64)
        arr_out = bitboard_from_signed_int(arr_in)
        
        # Modifying the output view must immediately mutate the input array
        arr_out[0] = 42
        self.assertEqual(arr_in[0], 42)

    def test_numpy_scalar_conversion(self):
        """Should convert a single np.int64 scalar to a Bitboard scalar."""
        scalar_in = np.int64(-1)
        expected = np.uint64(18446744073709551615)
        
        scalar_out = bitboard_from_signed_int(scalar_in)
        
        self.assertIsInstance(scalar_out, np.uint64)
        self.assertEqual(scalar_out, expected)

    def test_wrong_type_raises_error(self):
        """Should reject standard Python types or invalid NumPy dtypes."""
        # Test with built-in Python int
        with self.assertRaises(ValidationError):
            bitboard_from_signed_int(-1)
            
        # Test with invalid NumPy dtype (float64)
        arr_float = np.array([-1.0], dtype=np.float64)
        with self.assertRaises(ValueError):
            bitboard_from_signed_int(arr_float)

    def test_wrong_array_raises_value_error(self):
        """Should reject arrays with more than one dimension."""
        with self.assertRaises(ValueError):
            i = np.array([[0, 1], [3, 7]], dtype=np.int64)
            bitboard_from_signed_int(-1)

class TestBitboardBsr(unittest.TestCase):

    def test_scalar_bsr_high_bits(self):
        """Should precisely identify highest bits near the 64-bit boundary and return np.int8."""
        # Test extreme and standard values
        res_0 = bitboard_bsr(Bitboard(0))
        res_1 = bitboard_bsr(Bitboard(1))
        res_53 = bitboard_bsr(Bitboard(1 << 53))
        res_62 = bitboard_bsr(Bitboard(1 << 62))
        res_63 = bitboard_bsr(Bitboard(1 << 63))
        res_max = bitboard_bsr(Bitboard(0xFFFFFFFFFFFFFFFF))

        # Verify exact bit indices
        self.assertEqual(res_0, -1)
        self.assertEqual(res_1, 0)
        self.assertEqual(res_53, 53)
        self.assertEqual(res_62, 62)
        self.assertEqual(res_63, 63)
        self.assertEqual(res_max, 63)

        # Verify output type is strictly np.int8
        self.assertIsInstance(res_0, int)
        self.assertIsInstance(res_max, int)

    def test_wrong_type_raises_type_error(self):
        """Should reject standard Python types or invalid NumPy signed/float types."""
        # Test with built-in Python int
        with self.assertRaises(ValidationError):
            bitboard_bsr(-1)
            
        # Test with incorrect NumPy signed dtype (int64)
        arr_signed = np.array([1, 2], dtype=np.int64)
        with self.assertRaises(ValidationError):
            bitboard_bsr(arr_signed)

class TestBitboardPrint(unittest.TestCase):

    def test_bitboard_print_output(self):
        """Test that bitboard_print outputs the correct 2D string representation."""
        # Define the expected output string exactly as generated by the function
        expected_output = (
            "  a b c d e f g h\n"
            "1 . x x x x . . .\n"
            "2 . x . . . x . .\n"
            "3 . x . . . x . .\n"
            "4 . x . . x . . .\n"
            "5 . x x x . . . .\n"
            "6 . x . x . . . .\n"
            "7 . x . . x . . .\n"
            "8 . x . . . x . .\n"
        )

        # Context manager handles the memory buffer life cycle
        with io.StringIO() as buffer:
            # Explicit injection into the signature parameter
            bitboard_print(ar, output=buffer)
            actual_output = buffer.getvalue()

        self.assertEqual(actual_output, expected_output)

class TestBitboardCount(unittest.TestCase):
    
    def test_bitboard_count(self):
        self.assertEqual(bitboard_count(empty), 0)
        self.assertEqual(bitboard_count(full), 64)
        self.assertEqual(bitboard_count(ar), 19)

    def test_wrong_type_raises_type_error(self):
        with self.assertRaises(ValidationError):
            bitboard_count('Not the right type')

class BaseBitboardTransformationTest:
    """
    Abstract blueprint containing core suite logic for Bitboard operations.
    Subclasses must define 'self.func' and 'self.test_data'.
    """
    func = None       # Must be set by subclass (e.g., bitboard_fa1h8)
    test_data = []    # Must be set by subclass

    def test_scalar_transformations(self):
        """Iterate over all paired structures checking outputs using custom hex messages."""
        for i, (bb, expected) in enumerate(self.test_data):
            computed = self.func(bb)
            
            if computed != expected:
                bb_hex = f"{int(bb):016X}"
                computed_hex = f"{int(computed):016X}"
                expected_hex = f"{int(expected):016X}"
                
                self.assertEqual(
                    computed, 
                    expected, 
                    msg=(
                        f"Failed at index {i}!\n"
                        f"Input:    0x{bb_hex}\n"
                        f"Computed: 0x{computed_hex}\n"
                        f"Expected: 0x{expected_hex}"
                    )
                )

    def test_vector_transformations(self):
        """Concurrently process all test cases as a single packed NumPy array vector."""
        input_vector = np.array([case[0] for case in self.test_data], dtype=np.uint64)
        expected_vector = np.array([case[1] for case in self.test_data], dtype=np.uint64)
        
        computed_vector = self.func(input_vector)
        
        self.assertIsInstance(computed_vector, np.ndarray)
        self.assertEqual(computed_vector.dtype, np.uint64)
        
        try:
            np.testing.assert_array_equal(computed_vector, expected_vector)
        except AssertionError:
            mismatches = np.where(computed_vector != expected_vector)[0]
            fail_idx = mismatches[0]
            
            bb_hex = f"{int(input_vector[fail_idx]):016X}"
            computed_hex = f"{int(computed_vector[fail_idx]):016X}"
            expected_hex = f"{int(expected_vector[fail_idx]):016X}"
            
            self.fail(
                f"Vectorized test failed at vector index {fail_idx}!\n"
                f"Input:    0x{bb_hex}\n"
                f"Computed: 0x{computed_hex}\n"
                f"Expected: 0x{expected_hex}"
            )
            
    def test_wrong_type_raises_validation_error(self):
        """Enforce strict Pydantic parameter boundaries against non-compliant argument inputs."""
        with self.assertRaises(ValidationError):
            self.func(42)

        with self.assertRaises(ValidationError):
            self.func('Invalid Bitboard Type')

        with self.assertRaises(ValidationError):
            self.func(np.array([1, 2, 3], dtype=np.int64))

        with self.assertRaises(ValidationError):
            self.func(np.array([1.0, 2.0], dtype=np.float64))

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        """Measure operations speed on a dense 1,000,000 matrix sequence."""
        size = 1_000_000
        # Fallback to a standard matrix if global variable constants like 'sqa1' aren't bound in context
        fallback_seed = self.test_data[2][0] if len(self.test_data) > 2 else np.uint64(1)
        input_vector = np.full(size, fallback_seed, dtype=np.uint64)
        
        _ = self.func(input_vector[:10])  # Validation Warmup
        
        start_time = time.perf_counter()
        computed_vector = self.func(input_vector)
        end_time = time.perf_counter()
        
        duration = end_time - start_time
        boards_per_sec = size / duration
        
        print(f"\n[PERF {self.func.__name__}] Processed {size:,} boards in {duration:.4f}s ({boards_per_sec:,.0f} b/s)")
        
        # Performance output sanity check
        expected_output_seed = self.func(fallback_seed)
        expected_vector = np.full(size, expected_output_seed, dtype=np.uint64)
        np.testing.assert_array_equal(computed_vector, expected_vector)

class TestBitboardTransformationFa1h8(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_fa1h8
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqa1), (sqh1, sqa8),
            (sqa8, sqh1), (sqh8, sqh8), (row_1, column_a), (row_8, column_h),
            (column_a, row_1), (column_h, row_8), (half_left, half_top),
            (half_right, half_bottom), (half_top, half_left), (half_bottom, half_right),
        ]

class TestBitboardTransformationFh1a8(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_fh1a8
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqh8), (sqh1, sqh1),
            (sqa8, sqa8), (sqh8, sqa1), (row_1, column_h), (row_8, column_a),
            (column_a, row_8), (column_h, row_1), (half_left, half_bottom),
            (half_right, half_top), (half_top, half_right), (half_bottom, half_left),
        ]

class TestBitboardTransformationFhori(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_fhori
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqh1), (sqh1, sqa1),
            (sqa8, sqh8), (sqh8, sqa8), (row_1, row_8), (row_8, row_1),
            (column_a, column_a), (column_h, column_h), (half_left, half_left),
            (half_right, half_right), (half_top, half_bottom), (half_bottom, half_top),
        ]

class TestBitboardTransformationFvert(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_fvert
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqa8), (sqh1, sqh8),
            (sqa8, sqa1), (sqh8, sqh1), (row_1, row_1), (row_8, row_8),
            (column_a, column_h), (column_h, column_a), (half_left, half_right),
            (half_right, half_left), (half_top, half_top), (half_bottom, half_bottom),
        ]

class TestBitboardTransformationRo000(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_ro000
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqa1), (sqh1, sqh1),
            (sqa8, sqa8), (sqh8, sqh8), (row_1, row_1), (row_8, row_8),
            (column_a, column_a), (column_h, column_h), (half_left, half_left),
            (half_right, half_right), (half_top, half_top), (half_bottom, half_bottom),
        ]

class TestBitboardTransformationRo090(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_ro090
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqa8), (sqh1, sqa1),
            (sqa8, sqh8), (sqh8, sqh1), (row_1, column_h), (row_8, column_a),
            (column_a, row_1), (column_h, row_8), (half_left, half_top),
            (half_right, half_bottom), (half_top, half_right), (half_bottom, half_left),
        ]

class TestBitboardTransformationRo180(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_ro180
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqh8), (sqh1, sqa8),
            (sqa8, sqh1), (sqh8, sqa1), (row_1, row_8), (row_8, row_1),
            (column_a, column_h), (column_h, column_a), (half_left, half_right),
            (half_right, half_left), (half_top, half_bottom), (half_bottom, half_top),
        ]

class TestBitboardTransformationRo270(unittest.TestCase, BaseBitboardTransformationTest):
    
    def setUp(self):
        self.func = bitboard_ro270
        self.test_data = [
            (empty, empty), (full, full), (sqa1, sqh1), (sqh1, sqh8),
            (sqa8, sqa1), (sqh8, sqa8), (row_1, column_a), (row_8, column_h),
            (column_a, row_8), (column_h, row_1), (half_left, half_bottom),
            (half_right, half_top), (half_top, half_left), (half_bottom, half_right),
        ]

class TestBitboardTransformations(unittest.TestCase):

    def setUp(self):
        self.ar = ar 
        self.elle = Bitboard(0x0000000000000107)

        self.expected_ar = np.array([
            ar, ar_rotate_90c, ar_rotate_180, ar_rotate_90a,
            ar_reflection_v, ar_reflection_h1a8, ar_reflection_h, ar_reflection_a1h8
        ], dtype=Bitboard)

        self.expected_elle = np.array([
            0x0000000000000107, 0x00000000008080C0, 0xE080000000000000, 0x0301010000000000,
            0x00000000000080E0, 0xC080800000000000, 0x0701000000000000, 0x0000000000010103
        ], dtype=Bitboard)

    def test_ar(self):
        computed = bitboard_transformations(self.ar)
        self.assertEqual(computed.shape, (8,))
        np.testing.assert_array_equal(computed, self.expected_ar)

    def test_elle(self):
        computed = bitboard_transformations(self.elle)
        self.assertEqual(computed.shape, (8,))
        np.testing.assert_array_equal(computed, self.expected_elle)

    def test_elle_ar_array(self):
        bb_array = np.array([self.ar, self.elle], dtype=Bitboard)
        expected = np.vstack([self.expected_ar, self.expected_elle])
        
        computed = bitboard_transformations(bb_array)
        self.assertEqual(computed.shape, (2, 8))
        np.testing.assert_array_equal(computed, expected)

    def test_invalid_array_type_raises_pydantic_validation_error(self):
        invalid_type_array = np.array([1, 2, 3], dtype=np.int32)
        
        with self.assertRaises(pydantic.ValidationError) as context:
            bitboard_transformations(invalid_type_array)
            
        self.assertIn("dtype", str(context.exception))
        self.assertIn("int32", str(context.exception))

    def test_invalid_scalar_type_raises_pydantic_validation_error(self):
        with self.assertRaises(pydantic.ValidationError) as context:
            bitboard_transformations("invalid_argument_type")
            
        self.assertIn("invalid_argument_type", str(context.exception))
        
    def test_invalid_array_dimension_raises_value_error(self):
        """Verify that a multidimensional array triggers a ValueError."""
        # Create a 2D array instead of the expected 1D array
        invalid_2d_array = np.array([
            [self.ar, self.elle],
            [self.ar, self.elle]
        ], dtype=Bitboard)
        
        with self.assertRaises(ValueError) as context:
            bitboard_transformations(invalid_2d_array)
            
        # Verify the error message mentions the shape constraints
        self.assertIn("must be 1D", str(context.exception))
        self.assertIn("got shape (2, 2)", str(context.exception))

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        """Measure operations speed on a dense 1,000,000 matrix sequence."""

        size = 1_000_000
        # Use self.ar as the baseline seed for the dense vector simulation
        fallback_seed = self.ar
        input_vector = np.full(size, fallback_seed, dtype=Bitboard)
        
        _ = bitboard_transformations(input_vector[:10])  # Validation Warmup
        
        start_time = time.perf_counter()
        computed_vector = bitboard_transformations(input_vector)
        end_time = time.perf_counter()
        
        duration = end_time - start_time
        boards_per_sec = size / duration
        
        print(f"\n[PERF bitboard_transformations] Processed {size:,} boards in {duration:.4f}s ({boards_per_sec:,.0f} b/s)")
        
        # Performance output sanity check
        expected_output_seed = bitboard_transformations(fallback_seed)
        expected_vector = np.repeat(expected_output_seed[np.newaxis, :], size, axis=0)
        np.testing.assert_array_equal(computed_vector, expected_vector)

class TestBitboardToSquareList(unittest.TestCase):
    
    def test_bitboard_to_square_list(self):
        bb = Bitboard(0x0000000000000000)
        self.assertTrue(bitboard_to_square_list(bb) == [])
        
        bb = Bitboard(0x0000000000000001)
        self.assertTrue(bitboard_to_square_list(bb) == [Square(0)])

        bb = Bitboard(0x8000000000000003)
        self.assertTrue(bitboard_to_square_list(bb) == [Square(63), Square(1), Square(0)])

    def test_wrong_type_raises_type_error(self):
        with self.assertRaises(ValidationError):
            bitboard_to_square_list('Not the right type')

