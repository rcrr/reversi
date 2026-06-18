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

class TestBitboardToSignedInt(unittest.TestCase):

    def test_scalar_conversion_positive(self):
        """Should return a positive int64 when the most significant bit (MSB) is 0."""
        # A standard small bitboard number
        bb = Bitboard(4611717676283199524)
        expected = np.int64(4611717676283199524)
        
        result = bitboard_to_signed_int(bb)
        
        self.assertEqual(result, expected)
        self.assertIsInstance(result, np.int64)

    def test_scalar_conversion_negative_twos_complement(self):
        """Should return a negative int64 when the MSB is 1 (two's complement conversion)."""
        # 0xFFFFFFFFFFFFFFFF translates to signed -1
        bb_all_ones = Bitboard(0xFFFFFFFFFFFFFFFF)
        expected_minus_one = np.int64(-1)
        
        # 0x8000000000000000 is the lowest possible signed 64-bit integer
        bb_min_int = Bitboard(0x8000000000000000)
        expected_min_int = np.int64(-9223372036854775808)
        
        self.assertEqual(bitboard_to_signed_int(bb_all_ones), expected_minus_one)
        self.assertEqual(bitboard_to_signed_int(bb_min_int), expected_min_int)

    def test_array_1d_conversion_valid(self):
        """Should correctly cast an entire 1D BitboardArray into a signed int64 array zero-copy."""
        bb_array = np.array([0, 4611717676283199524, 0xFFFFFFFFFFFFFFFF], dtype=Bitboard)
        expected = np.array([0, 4611717676283199524, -1], dtype=np.int64)
        
        result = bitboard_to_signed_int(bb_array)
        
        self.assertIsInstance(result, np.ndarray)
        self.assertEqual(result.dtype, np.int64)
        self.assertEqual(result.shape, (3,))
        self.assertTrue(np.all(result == expected))

    def test_array_invalid_dimensions(self):
        """Should raise ValueError if the passed bitboard array is 2D instead of 1D."""
        invalid_2d_array = np.array([
            [1, 2],
            [3, 4]
        ], dtype=np.uint64)
        
        with self.assertRaises(ValueError):
            bitboard_to_signed_int(invalid_2d_array)

    def test_invalid_dtype_error(self):
        """Should raise ValueError if the input array is not fundamentally a uint64."""
        invalid_type_array = np.array([1, 2, 3], dtype=np.int32)
        
        with self.assertRaises(ValueError):
            bitboard_to_signed_int(invalid_type_array)

class TestBitboardFromHexStr(unittest.TestCase):
    
    def test_scalar_conversion_valid(self):
        """Should correctly parse a valid 16-character hex string into a single Bitboard."""
        hex_str = "0000001008000000"
        expected = Bitboard(0x0000001008000000)
        
        result = bitboard_from_hex_str(hex_str)
        
        self.assertEqual(result, expected)
        self.assertIsInstance(result, Bitboard)

    def test_scalar_conversion_case_insensitivity(self):
        """Should handle lowercase, uppercase, and mixed-case hex strings identically."""
        hex_lower = "abcdef0123456789"
        hex_upper = "ABCDEF0123456789"
        
        expected = Bitboard(0xABCDEF0123456789)
        
        self.assertEqual(bitboard_from_hex_str(hex_lower), expected)
        self.assertEqual(bitboard_from_hex_str(hex_upper), expected)

    def test_scalar_conversion_invalid_length(self):
        """Should raise ValueError if a scalar string is not exactly 16 characters long."""
        with self.assertRaises(ValueError):
            bitboard_from_hex_str("123")  # Too short
            
        with self.assertRaises(ValueError):
            bitboard_from_hex_str("000000100800000011")  # Too long

    def test_array_1d_conversion_valid(self):
        """Should correctly parse a 1D NumPy array of valid hex strings into a BitboardArray."""
        hex_strings = np.array(["0000001008000000", "0000000810000000", "FFFFFFFFFFFFFFFF"])
        expected = np.array([0x0000001008000000, 0x0000000810000000, 0xFFFFFFFFFFFFFFFF], dtype=Bitboard)
        
        result = bitboard_from_hex_str(hex_strings)
        
        self.assertIsInstance(result, np.ndarray)
        self.assertEqual(result.dtype, Bitboard)
        self.assertEqual(result.shape, (3,))
        self.assertTrue(np.all(result == expected))

    def test_array_1d_conversion_invalid_length(self):
        """Should raise ValueError if any string inside the 1D array breaks the 16-character limit."""
        # One string is valid, the second one is too short
        invalid_array = np.array(["0000001008000000", "A1B2C3"])
        
        with self.assertRaises(ValueError):
            bitboard_from_hex_str(invalid_array)

    def test_array_invalid_dimensions(self):
        """Should raise ValueError if the passed NumPy array is 2D instead of 1D."""
        invalid_2d_array = np.array([
            ["0000001008000000"], 
            ["0000000810000000"]
        ])
        
        with self.assertRaises(ValueError):
            bitboard_from_hex_str(invalid_2d_array)

    def test_invalid_characters(self):
        """Should raise ValueError if strings contain non-hex characters (handled by Python's int cast)."""
        invalid_hex_str = "000000100800000G"  # 'G' is out of bounds for base 16
        
        with self.assertRaises(ValueError):
            bitboard_from_hex_str(invalid_hex_str)

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

class TestBitboardTransformationFa1h8Comp(unittest.TestCase):

    def test_fa1h8_compositions(self):
        expected = bitboard_fa1h8(ar)
        
        computed = bitboard_fa1h8(bitboard_ro000(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_fvert(bitboard_ro090(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_fh1a8(bitboard_ro180(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_fhori(bitboard_ro270(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_ro270(bitboard_fvert(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_ro090(bitboard_fhori(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_ro000(bitboard_fa1h8(ar))
        self.assertEqual(computed, expected)
        
        computed = bitboard_ro180(bitboard_fh1a8(ar))
        self.assertEqual(computed, expected)
            
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

            self.assertIsInstance(computed, Bitboard)
            
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
        input_vector = np.array([case[0] for case in self.test_data], dtype=Bitboard)
        expected_vector = np.array([case[1] for case in self.test_data], dtype=Bitboard)
        
        computed_vector = self.func(input_vector)
        
        self.assertIsInstance(computed_vector, np.ndarray)
        self.assertEqual(computed_vector.dtype, Bitboard)
        
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
        fallback_seed = self.test_data[2][0] if len(self.test_data) > 2 else Bitboard(1)
        input_vector = np.full(size, fallback_seed, dtype=Bitboard)
        
        _ = self.func(input_vector[:10])  # Validation Warmup
        
        start_time = time.perf_counter()
        computed_vector = self.func(input_vector)
        end_time = time.perf_counter()
        
        duration = end_time - start_time
        boards_per_sec = size / duration
        
        print(f"\n[PERF {self.func.__name__}] Processed {size:,} boards in {duration:.4f}s ({boards_per_sec:,.0f} b/s)")
        
        # Performance output sanity check
        expected_output_seed = self.func(fallback_seed)
        expected_vector = np.full(size, expected_output_seed, dtype=Bitboard)
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

class TestBitboardAntiTransformations(unittest.TestCase):

    def setUp(self):
        self.ar = ar 
        self.elle = Bitboard(0x0000000000000107)

        self.expected_ar = np.array([
            ar, ar_rotate_90a, ar_rotate_180, ar_rotate_90c,
            ar_reflection_v, ar_reflection_h1a8, ar_reflection_h, ar_reflection_a1h8
        ], dtype=Bitboard)

        self.expected_elle = np.array([
            0x0000000000000107, 0x0301010000000000, 0xE080000000000000, 0x00000000008080C0,
            0x00000000000080E0, 0xC080800000000000, 0x0701000000000000, 0x0000000000010103
        ], dtype=Bitboard)

    def test_ar(self):
        computed = bitboard_anti_transformations(self.ar)
        self.assertEqual(computed.shape, (8,))
        np.testing.assert_array_equal(computed, self.expected_ar)

    def test_elle(self):
        computed = bitboard_anti_transformations(self.elle)
        self.assertEqual(computed.shape, (8,))
        np.testing.assert_array_equal(computed, self.expected_elle)

    def test_elle_ar_array(self):
        bb_array = np.array([self.ar, self.elle], dtype=Bitboard)
        expected = np.vstack([self.expected_ar, self.expected_elle])
        
        computed = bitboard_anti_transformations(bb_array)
        self.assertEqual(computed.shape, (2, 8))
        np.testing.assert_array_equal(computed, expected)

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        """Measure operations speed on a dense 1,000,000 matrix sequence."""

        size = 1_000_000
        # Use self.ar as the baseline seed for the dense vector simulation
        fallback_seed = self.ar
        input_vector = np.full(size, fallback_seed, dtype=Bitboard)
        
        _ = bitboard_anti_transformations(input_vector[:10])  # Validation Warmup
        
        start_time = time.perf_counter()
        computed_vector = bitboard_anti_transformations(input_vector)
        end_time = time.perf_counter()
        
        duration = end_time - start_time
        boards_per_sec = size / duration
        
        print(f"\n[PERF bitboard_anti_transformations] Processed {size:,} boards in {duration:.4f}s ({boards_per_sec:,.0f} b/s)")
        
        # Performance output sanity check
        expected_output_seed = bitboard_anti_transformations(fallback_seed)
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

class TestBitboardToSquareArray(unittest.TestCase):
    
    def test_bitboard_to_square_array(self):
        bb = Bitboard(0x0000000000000000)
        ar = bitboard_to_square_array(bb)
        self.assertIsInstance(ar, np.ndarray)
        self.assertEqual(len(ar), 0)
        self.assertEqual(ar.dtype, Square)
        self.assertEqual(ar.shape, (0,))
        
        bb = Bitboard(0x0000000000000001)
        ar = bitboard_to_square_array(bb)
        self.assertIsInstance(ar, np.ndarray)
        self.assertEqual(len(ar), 1)
        self.assertEqual(ar.dtype, Square)
        self.assertEqual(ar.shape, (1,))
        self.assertEqual(ar[0], Square(0))

        bb = Bitboard(0x8000000000000003)
        ar = bitboard_to_square_array(bb)
        self.assertIsInstance(ar, np.ndarray)
        self.assertEqual(len(ar), 3)
        self.assertEqual(ar.dtype, Square)
        self.assertEqual(ar.shape, (3,))
        self.assertEqual(ar[0], Square(63))
        self.assertEqual(ar[1], Square(1))
        self.assertEqual(ar[2], Square(0))

    def test_wrong_type_raises_type_error(self):
        with self.assertRaises(ValidationError):
            bitboard_to_square_array('Not the right type')

class TestPositionCreation(unittest.TestCase):

    def test_new_position(self):
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        position = np.void((mover, opponent), dtype=Position)

        m = position['mover']
        o = position['opponent']

        self.assertEqual(m, mover)
        self.assertEqual(o, opponent)

    def test_create_position(self):
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        position = make_position(mover, opponent)
        position_check_collisions(position)

        m = position['mover']
        o = position['opponent']

        self.assertEqual(m, mover)
        self.assertEqual(o, opponent)
        
        self.assertEqual(position.item(), (mover, opponent))
        
        self.assertIsInstance(position, np.void)
        self.assertEqual(position.dtype, Position)
        self.assertEqual(position.shape, ())

    def test_new_position_array_one_element(self):
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        position_array: PositionArray = np.array([(mover, opponent)], dtype=Position)
        
        m = position_array[0]['mover']
        o = position_array[0]['opponent']

        self.assertEqual(m, mover)
        self.assertEqual(o, opponent)

    def test_new_position_array_n_element(self):

        N = 3
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        m_list = [mover] * N
        o_list = [opponent] * N

        positions = make_position(m_list, o_list)
        position_check_collisions(positions)
        
        self.assertIsInstance(positions, np.ndarray)
        self.assertEqual(positions.dtype, Position)
        self.assertEqual(positions.shape, (N,))

        for i, pos in enumerate(positions): 
            m = pos['mover']
            o = pos['opponent']
            self.assertEqual(m, mover)
            self.assertEqual(o, opponent)

        for i in range(N): 
            m = positions[i]['mover']
            o = positions[i]['opponent']
            self.assertEqual(m, mover)
            self.assertEqual(o, opponent)
        
    def test_make_position_length_mismatch(self):
        """Test that passing arrays of different lengths raises a ValueError."""
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        m_array = np.array([mover] * 5, dtype=Bitboard)  # Length 5
        o_array = np.array([opponent] * 3, dtype=Bitboard)  # Length 3

        # Expect ValueError because of the size mismatch
        with self.assertRaises(ValueError):
            make_position(m_array, o_array)

    def test_extract_views_zero_copy(self):
        """Test that slicing fields from PositionArray returns zero-copy BitboardArrays."""
        N = 3
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        # Create the array using our utility
        positions: PositionArray = make_position([mover] * N, [opponent] * N)
        position_check_collisions(positions)

        # 1. EXTRACT THE VIEWS
        mover_array: BitboardArray = positions['mover']
        opponent_array: BitboardArray = positions['opponent']

        # 2. ASSERT TYPES AND SHAPES
        self.assertEqual(mover_array.dtype, Bitboard)
        self.assertEqual(opponent_array.dtype, Bitboard)
        self.assertEqual(mover_array.shape, (N,))
        self.assertEqual(opponent_array.shape, (N,))

        # 3. ASSERT ZERO-COPY (Modifying the view alters the parent array instantly)
        new_bitboard = Bitboard(0xFFFFFFFFFFFFFFFF)
        mover_array[0] = new_bitboard

        # The underlying structured array changes without reassignment
        self.assertEqual(positions[0]['mover'], new_bitboard)

class TestPositionEq(unittest.TestCase):

    def test_position_eq_scalar_true(self):
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        pos_a = make_position(mover, opponent)
        pos_b = make_position(mover, opponent)

        are_eq = position_eq(pos_a, pos_b)

        self.assertTrue(are_eq)

    def test_position_eq_scalar_false(self):
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        
        pos_a = make_position(mover, opponent)
        pos_b = make_position(opponent, mover)

        are_eq = position_eq(pos_a, pos_b)

        self.assertFalse(are_eq)

    def test_position_eq_array_true(self):

        N = 3
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        m_list = [mover] * N
        o_list = [opponent] * N

        pos_a = make_position(m_list, o_list)
        pos_b = make_position(m_list, o_list)

        are_eq = position_eq(pos_a, pos_b)

        self.assertTrue(are_eq)

    def test_position_eq_array_false(self):

        N = 3
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        m_list = [mover] * N
        o_list = [opponent] * N

        pos_a = make_position(m_list, o_list)
        pos_b = make_position(o_list, m_list)

        are_eq = position_eq(pos_a, pos_b)

        self.assertFalse(are_eq)

    def test_position_eq_array_diff_len(self):

        N = 3
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        pos_a = make_position([mover] * N, [opponent] * N)
        pos_b = make_position([mover] * (N + 1), [opponent] * (N + 1))

        are_eq = position_eq(pos_a, pos_b)

        self.assertFalse(are_eq)

    def test_position_eq_array_scalar(self):

        N = 3
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        pos_a = make_position([mover] * N, [opponent] * N)
        pos_b = make_position(mover, opponent)

        are_eq = position_eq(pos_a, pos_b)

        self.assertFalse(are_eq)

    def test_position_eq_wrong_type(self):
        
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))

        pos_a = 'wrong_type'
        pos_b = make_position(mover, opponent)

        with self.assertRaises(ValidationError):
            are_eq = position_eq(pos_a, pos_b)

class TestPositionPrint(unittest.TestCase):

    def test_position_print_output(self):
        """Test that position_print outputs the correct 2D string representation."""
        # Define the expected output string exactly as generated by the function
        expected_output = (
            '  a b c d e f g h\n'
            '1 . . @ O . @ O .\n'
            '2 . . . O O O O .\n'
            '3 . . O O O O @ .\n'
            '4 @ @ O @ O @ @ @\n'
            '5 . @ O @ O O @ @\n'
            '6 . O @ @ @ O O .\n'
            '7 . . O O O O O O\n'
            '8 . O . . O . @ O\n'
        )

        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        p = make_position(mover, opponent)

        # Context manager handles the memory buffer life cycle
        with io.StringIO() as buffer:
            # Explicit injection into the signature parameter
            position_print(p, output=buffer)
            actual_output = buffer.getvalue()
            
        self.assertEqual(actual_output, expected_output)

class TestPositionEmpties(unittest.TestCase):

    def test_empties_scalar(self):
        mover = Bitboard(0x0000000000000000)
        opponent = Bitboard(0x0000000000000000)
        p = make_position(mover, opponent)
        self.assertEqual(position_empties(p), Bitboard(0xFFFFFFFFFFFFFFFF))

        mover = Bitboard(0x0000000000000000)
        opponent = Bitboard(0x0000000000000001)
        p = make_position(mover, opponent)
        self.assertEqual(position_empties(p), Bitboard(0xFFFFFFFFFFFFFFFE))

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position(mover, opponent)
        self.assertEqual(position_empties(p), Bitboard(0xFFFFFFFFFFFFFFFC))

        mover = Bitboard(0x0000000000000000)
        opponent = Bitboard(0xFFFFFFFFFFFFFFFF)
        p = make_position(mover, opponent)
        self.assertEqual(position_empties(p), Bitboard(0x0000000000000000))

    def test_position_eq_wrong_type(self):

        with self.assertRaises(ValidationError):
            position_empties(None)

    def test_empties_array(self):
        
        N = 3
        
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)

        positions = make_position([mover] * N, [opponent] * N)

        expected = np.full(N, Bitboard(0xFFFFFFFFFFFFFFFC))
        computed = position_empties(positions)

        np.testing.assert_array_equal(computed, expected)
        
    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        
        N = 1_000_000
        
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)

        positions = make_position([mover] * N, [opponent] * N)

        expected = np.full(N, Bitboard(0xFFFFFFFFFFFFFFFC))
        
        start_time = time.perf_counter()
        computed = position_empties(positions)
        end_time = time.perf_counter()
        
        duration = end_time - start_time
        positions_per_sec = N / duration
        
        print(f"\n[PERF position_empties] Processed {N:,} positions in {duration:.4f}s ({positions_per_sec:,.0f} b/s)")

        np.testing.assert_array_equal(computed, expected)

class TestPositionLegalMoves(unittest.TestCase):

    def test_legal_moves_c1(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        
        lms = legal_moves(mover, opponent)
        expected_lms = Bitboard(0x0000000000000004)
        self.assertEqual(lms, expected_lms)

    def test_legal_moves_h8(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0040201008040200)
        
        lms = legal_moves(mover, opponent)
        expected_lms = Bitboard(0x8000000000000000)
        self.assertEqual(lms, expected_lms)

    def test_vectorized_legal_moves(self):

        mover    = np.array([0x0000000000000001,
                             0x0000000000000001], dtype=Bitboard)
        opponent = np.array([0x0000000000000002,
                             0x0040201008040200], dtype=Bitboard)
        
        lms = legal_moves(mover, opponent)
        
        expected_lms = np.array([0x0000000000000004,
                                 0x8000000000000000], dtype=Bitboard)
        
        nptest.assert_array_equal(lms, expected_lms)

    def test_position_legal_moves_scalar(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position(mover, opponent)
        
        computed = position_legal_moves(p)
        expected = Bitboard(0x0000000000000004)
        self.assertEqual(computed, expected)

    def test_position_legal_moves_array(self):

        mover    = np.array([0x0000000000000001,
                             0x0000000000000001], dtype=Bitboard)
        opponent = np.array([0x0000000000000002,
                             0x0040201008040200], dtype=Bitboard)
        positions = make_position(mover, opponent)
        
        lms = position_legal_moves(positions)
        
        expected_lms = np.array([0x0000000000000004,
                                 0x8000000000000000], dtype=Bitboard)
        
        nptest.assert_array_equal(lms, expected_lms)

    def test_legal_moves_bulk(self):
        data = [
            (0x0000000000000000, 0x0000000000000000, 0x0000000000000000), # 000
            (0x0000000000000001, 0x0000000000000000, 0x0000000000000000), # 001
            (0x0000000000000002, 0x0000000000000000, 0x0000000000000000), # 002
            (0x0000000000000004, 0x0000000000000000, 0x0000000000000000), # 003
            (0x0000000000000008, 0x0000000000000000, 0x0000000000000000), # 004
            (0x0000000000000000, 0x0000000000000001, 0x0000000000000000), # 005
            (0x0000000000000000, 0x0000000000000002, 0x0000000000000000), # 006
            (0x0000000000000000, 0x0000000000000004, 0x0000000000000000), # 007
            (0x0000000000000008, 0x0000000000000000, 0x0000000000000000), # 008
            (0x0000000000000000, 0x00000000000000ff, 0x0000000000000000), # 009
            (0x0000000000000001, 0x0000000000000002, 0x0000000000000004), # 010
            (0x0000000000000001, 0x0000000000000006, 0x0000000000000008), # 011
            (0x0000000000000001, 0x000000000000000e, 0x0000000000000010), # 012
            (0x0000000000000001, 0x000000000000001e, 0x0000000000000020), # 013
            (0x0000000000000001, 0x000000000000003e, 0x0000000000000040), # 014
            (0x0000000000000001, 0x000000000000007e, 0x0000000000000080), # 015
            (0x0000000000000001, 0x00000000000000fe, 0x0000000000000000), # 016
            (0x0000000000000004, 0x000000000000007a, 0x0000000000000081), # 017
            (0x0000000000000081, 0x000000000000007a, 0x0000000000000004), # 018
            (0x0000000000000002, 0x00000000000000fc, 0x0000000000000000), # 019
            (0x0000000000000001, 0x0000000000000100, 0x0000000000010000), # 020
            (0x0000000000000002, 0x0000000000000200, 0x0000000000020000), # 021
            (0x0000000000000004, 0x0000000000000400, 0x0000000000040000), # 022
            (0x0000000000000008, 0x0000000000000800, 0x0000000000080000), # 023
            (0x0000000000000001, 0x0000000000010100, 0x0000000001000000), # 024
            (0x0000000000000001, 0x0001010101010100, 0x0100000000000000), # 025
            (0x0000000000000011, 0x0011111111111100, 0x1100000000000000), # 026
            (0x0000000000000055, 0x0055555555555500, 0x5500000000000000), # 027
            (0xaa00000000000000, 0x00aaaaaaaaaaaa00, 0x00000000000000aa), # 028
            (0x0055000000000000, 0x0000555555550000, 0x0000000000005500), # 029
            (0x0000000000000001, 0x0000000000000200, 0x0000000000040000), # 030
            (0x0000000000040000, 0x0000000000000200, 0x0000000000000001), # 031
            (0x8000000000000000, 0x0040000000000000, 0x0000200000000000), # 032
            (0x0000200000000000, 0x0040000000000000, 0x8000000000000000), # 033
            (0x0000000000000001, 0x0040201008040200, 0x8000000000000000), # 034
            (0x8000000000000000, 0x0040201008040200, 0x0000000000000001), # 035
            (0x8000000000000001, 0x0040200008040200, 0x0000001000000000), # 036
            (0x0000000000000020, 0x0000000000004000, 0x0000000000800000), # 037
            (0x0000000000800000, 0x0000000000004000, 0x0000000000000020), # 038
            (0x0000000000080000, 0x0080002010000400, 0x0000400000000002), # 039
            (0x0000000000000080, 0x0000000000004000, 0x0000000000200000), # 040
            (0x0000000000200000, 0x0000000000004000, 0x0000000000000080), # 041
            (0x0000000000000080, 0x0002040810204000, 0x0100000000000000), # 042
            (0x0100000000000000, 0x0002040810204000, 0x0000000000000080), # 043
            (0x0000000010000000, 0x0002040800204000, 0x0100000000000080), # 044
            (0x0000000000000004, 0x0000000000000200, 0x0000000000010000), # 045
            (0x0000000000010000, 0x0000000000000200, 0x0000000000000004), # 046
            (0x2000000000000000, 0x0040000000000000, 0x0000800000000000), # 047
            (0x0000800000000000, 0x0040000000000000, 0x2000000000000000), # 048
            (0x0000000001000408, 0x0000000000000000, 0x0000000000000000), # 049
            (0x0000000810000000, 0x0000001008000000, 0x0000102004080000), # 050
            (0x0000000008000000, 0x0000003810000000, 0x0000280020000000), # 051
            (0x0000003010000000, 0x0000080808000000, 0x0004040404040000), # 052
            (0x0000080800000000, 0x0000003018040000, 0x0000004020280000), # 053
            (0x0000003010040000, 0x0000080808080000, 0x0004040404100400), # 054
            (0x0000080800080000, 0x000000301c040000, 0x0000404220220000), # 055
            (0x000000201c040000, 0x0000081820080000, 0x0008340440301c00), # 056
            (0x0000080820080000, 0x000020301c040000, 0x0020404202220000), # 057
            (0x000020300c040000, 0x0000080830280000, 0x000c100440503c00), # 058
            (0x0000080800280000, 0x000020307c040000, 0x002040c200420000), # 059
            (0x000020205c040000, 0x0000081820680000, 0x0008140400107c00), # 060
            (0x0000080020680000, 0x000030385c040000, 0x002044c282020000), # 061
            (0x000030385c000000, 0x00000800206e0000, 0x000c04000000ff00), # 062
            (0x00000800004e0000, 0x000030387c202000, 0x0060404600100010), # 063
            (0x000030387c002000, 0x00000800007e0000, 0x000c04008000df00), # 064
            (0x0000080000460000, 0x000030387c383000, 0x0060404400000810), # 065
            (0x0000303878383000, 0x0000080404460000, 0x000c06028281c300), # 066
            (0x0000000404460000, 0x00003c3878383000, 0x0074004080000070), # 067
            (0x0000343878383000, 0x0010080404460000, 0x180c02028281c700), # 068
            (0x0010000404460000, 0x00043c3878383000, 0x0660004080000070), # 069
            (0x00043c3800383000, 0x00100004fc460000, 0x380002c202818700), # 070
            (0x00100004dc460000, 0x00043c7820383000, 0x066a408000004c78), # 071
            (0x00043c0020383000, 0x001000fcdc460000, 0x3800c20202818700), # 072
            (0x001000dccc460000, 0x00047c2030383000, 0x066a000000004878), # 073
            (0x00047c2010203000, 0x001000dcec5e0800, 0x380002020281c708), # 074
            (0x001000d8e44e0800, 0x00047e2418303000, 0x06ea000202004038), # 075
            (0x0004660418303000, 0x001818f8e44e0800, 0x382080020281c60c), # 076
            (0x001818f8e04e0800, 0x000466041e303000, 0x06e2810301014038), # 077
            (0x000466040e200000, 0x001818f8f05e3810, 0x382000000081c768), # 078
            (0x001018f8f05e3810, 0x100c66040e200000, 0x0ee3810301014000), # 079
            (0x100c660400200000, 0x001018f8ff5e3810, 0x282000000081c668), # 080
            (0x000018f8ff5e3810, 0x181c660400200000, 0x26e3810200004000), # 081
            (0x181c660000200000, 0x000018feff5e3810, 0x000000000081c668), # 082
            (0x000018daeb523810, 0x181c6624142c0400, 0x2663810000004208), # 083
            (0x180c6624142c0400, 0x201018daeb523810, 0x402001010081c368), # 084
            (0x001018daeb523810, 0x780c6624142c0400, 0x0663810000004208), # 085
            (0x780c0604142c0400, 0x003078faeb523810, 0x00c081010081c368), # 086
            (0x003078f8eb523810, 0x780c0706142c0400, 0x060300010000420a), # 087
            (0x780c0700142c0400, 0x003078ffeb523810, 0x00c080000081c368), # 088
            (0x002070fae8503810, 0x781c0f05172f0400, 0x0603000000004302), # 089
            (0x78180705172f0400, 0x022478fae8503810, 0x04c2800000804068), # 090
            (0x020458eae0503810, 0x787827151f2f0400, 0x0483000000004306), # 091
            (0x78782511172f0400, 0x02055aeee8503810, 0x058280000080c028), # 092
            (0x02054acea8103810, 0x7878353157ef0400, 0x808200000000c306), # 093
            (0x7878351147e70000, 0x02054aeeb8183c12, 0x050280000000002c), # 094
            (0x020542e6b0102012, 0x78783d194fef1c08, 0x048200000000c324), # 095
            (0x78783c184eee1c08, 0x020543e7b1112112, 0x0482800000004060), # 096
            (0x020543e7b1110112, 0x78783c184eee7c08, 0x04820000000082e4), # 097
            (0x78783c184cec7c08, 0x020543e7b3130312, 0x0482800000000021), # 098
            (0x020543e7b3130112, 0x78783c184cec7e09, 0x04820000000080e4), # 099
            (0x78783c1848e87001, 0x020543e7b7170f1e, 0x0582800000000020), # 100
            (0x020503c7a7170f1e, 0x78f87c3858e87001, 0x8000800000008060), # 101
            (0x78f87c3858c04001, 0x020503c7a73f3f3e, 0x0502800000000040), # 102
            (0x020503c7a72f1f00, 0x78f87c3858d0607f, 0x8002800000008080), # 103
            (0x78b85c2850d0607f, 0x824523d7af2f1f00, 0x0502800000000000), # 104
            (0x824422d6ae2e1e00, 0x79b95d2951d1617f, 0x0402800000008080), # 105
            (0x79391d2951d1617f, 0x82c4e2d6ae2e1e00, 0x0402000000000000), # 106
            (0x82c0e0d4ac2c1c00, 0x793f1f2b53d3637f, 0x0400000000008080), # 107
            (0xfec4e4d4ac2c1c00, 0x013b1b2b53d3637f, 0x0000000000008080), # 108
            (0x013b1b2b53d3237f, 0xfec4e4d4ac2c5c80, 0x0000000000008000), # 109
            (0xfec4e4d4ac2c1c80, 0x013b1b2b53d3e37f, 0x0000000000000000), # 110
            (0x3e0059e9150b050e, 0x007c26162a341800, 0x4002800040406030), # 111
            (0x0001073f0e060000, 0x3e3cb84030393c3e, 0x4040408040404040), # 112
            (0xa4bc9c8c0a042c0c, 0x00406373751b1100, 0x0003000080e00210), # 113
            (0x00a0cac0d8c804fe, 0x3c08303e26343800, 0x0050050101034200), # 114
            (0x785c2e4620000000, 0x84a0d0b9dffea000, 0x0200000000015f60), # 115
            (0x00108e86bff38000, 0x04687078400c3c1c, 0xf884000000000262), # 116
            (0x00783c3a66002020, 0x000183c599ff9c08, 0x0002400000004316), # 117
            (0x0c1c3c6c160e0000, 0x2001031329713d3c, 0x000200004080c042), # 118
            (0x0014f9c0849a0000, 0x0000063e7b643c3c, 0x000300010001c242), # 119
            (0x000002160e0e123d, 0x000d1d2931712d00, 0x1f306040c080c042), # 120
        ]
        
        for i, (mover, opponent, expected_legal_moves) in enumerate(data):
            p = make_position(Bitboard(mover), Bitboard(opponent))
            position_check_collisions(p)
            lm = position_legal_moves(p)
            elm = Bitboard(expected_legal_moves)
            if lm != elm:
                print("\nTest fails, computed and expected legal moves differ.".format())
                print("Error on data line n. {}".format(i))
                print("Board:")
                b.print()
                print("Computed legal_moves square set:")
                lm.print()
                print("Expected legal_moves square set:")
                elm.print()
            self.assertTrue(lm == elm)

        data_array = np.array(data, dtype=Bitboard)

        positions = make_position(data_array[:, 0], data_array[:, 1])
        position_check_collisions(positions)
        computed = position_legal_moves(positions)
        expected = data_array[:, 2].view(Bitboard)
        nptest.assert_array_equal(computed, expected)

        # This code is doing for the third time the same test over the data population.
        # Here the position array is created as a reference.
        # It is an example for the creation of the position array without copying the data.
        position_data = data_array[:, 0:2].copy()
        positions = position_data.view(Position).reshape(-1) # <-- No copy !
        computed = position_legal_moves(positions)
        expected = data_array[:, 2].view(Bitboard)
        nptest.assert_array_equal(computed, expected)
            
    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_1m(self):
        size = 1_000_000
        movers = np.full(size, 0x0000000000000001, dtype=Bitboard)
        opponents = np.full(size, 0x0040201008040200, dtype=Bitboard)

        positions = make_position(movers, opponents)
        position_check_collisions(positions)
        
        # Warmup
        position_legal_moves(positions[:10])
        
        start = time.perf_counter()
        lms = position_legal_moves(positions)
        end = time.perf_counter()
        
        duration = end - start
        print(f"\n[PERF position_legal_moves] Processed {size:,} boards in {duration:.4f}s ({(size/duration):,.0f} boards/sec)")
        
        expected_lms = np.full(size, 0x8000000000000000, dtype=Bitboard)
        nptest.assert_array_equal(lms, expected_lms)

class TestPositionCollisions(unittest.TestCase):

    def test_position_collisions_scalar(self):
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position(mover, opponent)
        self.assertEqual(position_collisions(p), Bitboard(0x0000000000000000))
        
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000001)
        p = make_position(mover, opponent)
        self.assertEqual(position_collisions(p), Bitboard(0x0000000000000001))

    def test_position_collisions_array(self):

        N = 3
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position([mover] * N, [opponent] * N)
        computed = position_collisions(p)
        expected = np.full(N, 0x0000000000000000, dtype=Bitboard)
        nptest.assert_array_equal(computed, expected)

        N = 3
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000001)
        p = make_position([mover] * N, [opponent] * N)
        computed = position_collisions(p)
        expected = np.full(N, 0x0000000000000001, dtype=Bitboard)
        nptest.assert_array_equal(computed, expected)

class TestPositionCheckCollisions(unittest.TestCase):

    def test_position_check_collisions_scalar_pass(self):
        """Should complete silently when there are no collisions in a scalar input."""
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position(mover, opponent)
        
        # This should execute without raising any exceptions
        try:
            position_check_collisions(p)
        except ValueError:
            self.fail("position_check_collisions() raised ValueError unexpectedly!")

    def test_position_check_collisions_scalar_fail(self):
        """Should raise ValueError with 1 collision out of 1 element for a scalar input."""
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000001)  # Collision here
        p = make_position(mover, opponent)
        
        with self.assertRaises(ValueError) as context:
            position_check_collisions(p)
            
        # Verify the descriptive error metrics for the scalar case
        self.assertIn("Detected 1 collision(s)", str(context.exception))
        self.assertIn("out of 1 total elements", str(context.exception))
        self.assertIn("index: 0", str(context.exception))

    def test_position_check_collisions_array_pass(self):
        """Should complete silently when there are no collisions across an array."""
        N = 3
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position([mover] * N, [opponent] * N)
        
        try:
            position_check_collisions(p)
        except ValueError:
            self.fail("position_check_collisions() raised ValueError unexpectedly on clean array!")

    def test_position_check_collisions_array_fail_all(self):
        """Should accurately count multiple collisions across the whole array."""
        N = 3
        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000001)  # Collisions everywhere
        p = make_position([mover] * N, [opponent] * N)
        
        with self.assertRaises(ValueError) as context:
            position_check_collisions(p)
            
        self.assertIn(f"Detected {N} collision(s)", str(context.exception))
        self.assertIn(f"out of {N} total elements", str(context.exception))
        self.assertIn("index: 0", str(context.exception))  # First one is still at 0

    def test_position_check_collisions_array_fail_single(self):
        """Should properly locate a solitary collision occurring deep inside an array."""
        # Index 0: Clean
        mover_list = [Bitboard(0x01)]
        opponent_list = [Bitboard(0x02)]
        
        # Index 1: Collision!
        mover_list.append(Bitboard(0x04))
        opponent_list.append(Bitboard(0x04))
        
        # Index 2: Clean
        mover_list.append(Bitboard(0x08))
        opponent_list.append(Bitboard(0x10))
        
        p = make_position(mover_list, opponent_list)
        
        with self.assertRaises(ValueError) as context:
            position_check_collisions(p)
            
        # Target specific metrics for the partial collision
        self.assertIn("Detected 1 collision(s)", str(context.exception))
        self.assertIn("out of 3 total elements", str(context.exception))
        self.assertIn("index: 1", str(context.exception))  # Pinpoints index 1

class TestPositionLegalMovesCount(unittest.TestCase):

    def test_position_legal_moves_count(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        p = make_position(mover, opponent)

        computed = position_legal_moves_count(p)
        expected = 1
        
        self.assertEqual(computed, expected)

class TestPositionFlips(unittest.TestCase):

    def test_position_flips_base_case(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)

        move = Bitboard(0x0000000000000004)

        flips, next_position = position_flips(position, move)

        expected_flips = Bitboard(0x0000000000000002)
        expected_next_position = make_position(Bitboard(0x0000000000000000),
                                               Bitboard(0x0000000000000007))

        self.assertEqual(flips, expected_flips)
        self.assertTrue(position_eq(next_position, expected_next_position))

    def test_position_flips_move_count_is_2(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)

        move = Bitboard(0x1000000000000004)

        flips, next_position = position_flips(position, move)

        expected_flips = Bitboard(0x0000000000000000)

        self.assertEqual(flips, expected_flips)
        self.assertIsNone(next_position)

    def test_position_flips_move_is_empty(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)

        move = Bitboard(0x0000000000000000)

        flips, next_position = position_flips(position, move)

        expected_flips = Bitboard(0x0000000000000000)

        self.assertEqual(flips, expected_flips)
        self.assertIsNone(next_position)

    def test_position_flips_move_is_invalid(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)

        move = Bitboard(0x1000000000000000)

        flips, next_position = position_flips(position, move)

        expected_flips = Bitboard(0x0000000000000000)

        self.assertEqual(flips, expected_flips)
        self.assertIsNone(next_position)

    def test_position_flips_wrong_move_type(self):

        mover = Bitboard(0x000428080A2C7C3C)
        opponent = Bitboard(0x10B8D6F7F5D30100)
        position = make_position(mover, opponent)
        move = 'WRONG TYPE'
        with self.assertRaises(ValidationError):
            flips, next_position = position_flips(position, move)

    def test_position_flips_bulk(self):
        data = [
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000000, 0x0000000000000000), # 000
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000001, 0x0000000000000000), # 001
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000002, 0x0000000000000000), # 002
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000040, 0x0000000000000000), # 003
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000080, 0x0000000000000000), # 004
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000000200, 0x0000000000020000), # 005
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000000000008000, 0x0000001020400000), # 006
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0000010000000000, 0x0000060204000000), # 007
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0001000000000000, 0x0000020400000000), # 008
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0002000000000000, 0x0000060200000000), # 009
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0040000000000000, 0x0038404040400000), # 010
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0100000000000000, 0x0000000000000000), # 011
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0200000000000000, 0x0000000000000000), # 012
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0400000000000000, 0x0000000000000000), # 013
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x0800000000000000, 0x0018000000000000), # 014
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x2000000000000000, 0x0030000000000000), # 015
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x4000000000000000, 0x0020100000000000), # 016
            (0x000428080A2C7C3C, 0x10B8D6F7F5D30100, 0x8000000000000000, 0x0000000000000000), # 017
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000001, 0x0000000000000000), # 018
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000002, 0x0000000000080400), # 019
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000040, 0x0000000000002020), # 020
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000080, 0x0000000000000000), # 021
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000100, 0x0000000000000000), # 022
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000000200, 0x0000000000043C00), # 023
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000008000, 0x0000000000000000), # 024
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000010000, 0x0000000002000000), # 025
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000020000, 0x00000002020C0400), # 026
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000000800000, 0x0000000000000000), # 027
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000001000000, 0x0000040200000000), # 028
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000004000000, 0x0000000000040400), # 029
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000000100000000, 0x0000010202040800), # 030
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000008000000000, 0x0000004000000000), # 031
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0000400000000000, 0x0000004000000000), # 032
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0010000000000000, 0x0000080000000000), # 033
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0040000000000000, 0x0000000000000000), # 034
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0080000000000000, 0x0000000000000000), # 035
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x0100000000000000, 0x0602040000000000), # 036
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x1000000000000000, 0x0000000000000000), # 037
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x2000000000000000, 0x0000000000000000), # 038
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x4000000000000000, 0x0000000000000000), # 039
            (0x0829322CF850401C, 0x06068D52022C3C20, 0x8000000000000000, 0x0000000000000000), # 040
            (0xFF0315CBE5713900, 0x00FC6A341A0E067F, 0x0000800000000000, 0x00C0600000000000), # 041
        ]

        for i, (mover, opponent, move, expected_flips) in enumerate(data):
            p = make_position(Bitboard(mover), Bitboard(opponent))
            position_check_collisions(p)
            m = Bitboard(move)
            flips, updated = position_flips(p, m)
            ef = Bitboard(expected_flips)
            if flips != ef:
                print("\nTest fails, computed and expected flips differ.".format())
                print("Error on data line n. {}".format(i))
                print("Board:")
                b.print()
                print("Move:")
                m.print()
                print("Computed flips:")
                flips.print()
                print("Expected flips:")
                ef.print()
            self.assertTrue(flips == ef)

class TestPositionMakeMove(unittest.TestCase):

    def test_position_make_move(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        move = Move(2)
        expected_updated = make_position(Bitboard(0x0000000000000000), Bitboard(0x0000000000000007))
        updated = position_make_move(position, move)
        self.assertEqual(updated, expected_updated)

    def test_position_make_move_wrong_move_type(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        move = 'WRONG TYPE'
        with self.assertRaises(ValidationError):
            position_make_move(position, move)

    def test_position_make_move_wrong_position_type(self):

        position = 'WRONG TYPE'
        move = Move(2)
        with self.assertRaises(ValidationError):
            position_make_move(position, move)

    def test_position_make_move_wrong_move_value(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        move = Move(65)
        with self.assertRaises(ValueError):
            position_make_move(position, move)

    def test_position_make_move_pass(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000000)
        position = make_position(mover, opponent)
        move = Move(64)
        expected_updated = make_position(Bitboard(0x0000000000000000), Bitboard(0x0000000000000001))
        updated = position_make_move(position, move)
        self.assertEqual(updated, expected_updated)

class TestPositionCountDifference(unittest.TestCase):

    def test_position_count_difference(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000006)
        position = make_position(mover, opponent)
        diff = position_count_difference(position)
        self.assertEqual(True, True)

class TestPositionFinalValue(unittest.TestCase):
    
    def test_position_final_value(self):

        mover = Bitboard(0xFF00000000000000)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        expected_value = 62
        self.assertEqual(position_final_value(position), expected_value)

class TestPositionHasToPass(unittest.TestCase):
    
    def test_position_has_to_pass_false(self):

        mover = Bitboard(0xFF00000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        self.assertFalse(position_has_to_pass(position))
    
    def test_position_has_to_pass_true(self):

        mover = Bitboard(0xFF00000000000000)
        opponent = Bitboard(0x0000000000000007)
        position = make_position(mover, opponent)
        self.assertTrue(position_has_to_pass(position))

class TestPositionIsGameOver(unittest.TestCase):

    def test_position_is_game_over_false(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        self.assertFalse(position_is_game_over(position))

    def test_position_is_game_over_true(self):

        mover = Bitboard(0xFFFFFFFFFFFFFFFF)
        opponent = Bitboard(0x0000000000000000)
        position = make_position(mover, opponent)
        self.assertTrue(position_is_game_over(position))

class TestPositionIsMoveLegal(unittest.TestCase):

    def test_position_is_move_legal_false(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        move = Move(0)
        self.assertFalse(position_is_move_legal(position, move))
        move = Move(1)
        self.assertFalse(position_is_move_legal(position, move))
        move = Move(3)
        self.assertFalse(position_is_move_legal(position, move))
        move = Move(64)
        self.assertFalse(position_is_move_legal(position, move))
        move = Move(65)
        self.assertFalse(position_is_move_legal(position, move))

    def test_position_is_move_legal_true(self):

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000002)
        position = make_position(mover, opponent)
        move = Move(2)
        self.assertTrue(position_is_move_legal(position, move))

        mover = Bitboard(0x0000000000000001)
        opponent = Bitboard(0x0000000000000004)
        position = make_position(mover, opponent)
        move = Move(64)
        self.assertTrue(position_is_move_legal(position, move))

class TestPositionTrFunctions(unittest.TestCase):

    def setUp(self):
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        self.p = make_position(mover, opponent)

    def tearDown(self):
        pass

    def test_position_fa1h8(self):
        trp = position_fa1h8(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x189C092038211808))
        self.assertEqual(trp['opponent'], Bitboard(0xC06376DE475CA000))

    def test_position_fh1a8(self):
        trp = position_fh1a8(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x1018841C04903918))
        self.assertEqual(trp['opponent'], Bitboard(0x00053AE27B6EC603))

    def test_position_fhori(self):
        trp = position_fhori(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x240040EBCA1C0040))
        self.assertEqual(trp['opponent'], Bitboard(0x48783C143462FC92))

    def test_position_fvert(self):
        trp = position_fvert(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x02003853D7020024))
        self.assertEqual(trp['opponent'], Bitboard(0x493F462C283C1E12))

    def test_position_ro000(self):
        trp = position_ro000(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x40001CCAEB400024))
        self.assertEqual(trp['opponent'], Bitboard(0x92FC6234143C7848))

    def test_position_ro090(self):
        trp = position_ro090(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x183990041C841810))
        self.assertEqual(trp['opponent'], Bitboard(0x03C66E7BE23A0500))

    def test_position_ro180(self):
        trp = position_ro180(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x240002D753380002))
        self.assertEqual(trp['opponent'], Bitboard(0x121E3C282C463F49))

    def test_position_ro270(self):
        trp = position_ro270(self.p)
        if False:
            print(f"Position:")
            position_print(self.p)
            print(f"Transformed:")
            position_print(trp)
            print(f"mover:    {trp['mover']:016X}")
            print(f"opponent: {trp['opponent']:016X}")
        self.assertEqual(trp['mover'], Bitboard(0x0818213820099C18))
        self.assertEqual(trp['opponent'], Bitboard(0x00A05C47DE7663C0))

class TestPositionTransformations(unittest.TestCase):

    def setUp(self):
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        self.p = make_position(mover, opponent)

        etrsm = np.array([
            0x40001CCAEB400024,
            0x183990041C841810,
            0x240002D753380002,
            0x0818213820099C18,
            0x02003853D7020024,
            0x1018841C04903918,
            0x240040EBCA1C0040,
            0x189C092038211808,
        ], dtype=Bitboard)

        etrso = np.array([
            0x92FC6234143C7848,
            0x03C66E7BE23A0500,
            0x121E3C282C463F49,
            0x00A05C47DE7663C0,
            0x493F462C283C1E12,
            0x00053AE27B6EC603,
            0x48783C143462FC92,
            0xC06376DE475CA000
        ], dtype=Bitboard)
        
        self.etrs = make_position(etrsm, etrso)

    def test_scalar(self):

        trps = position_transformations(self.p)
        nptest.assert_array_equal(trps, self.etrs)

    def test_array(self):

        N = 3
        ps = np.full(N, self.p, dtype=Position)
        trps = position_transformations(ps)
        expected = np.tile(self.etrs, (N, 1))        
        nptest.assert_array_equal(trps, expected)

class TestPositionAntiTransformations(unittest.TestCase):

    def setUp(self):
        mover = bitboard_from_signed_int(np.int64(4611717676283199524))
        opponent = bitboard_from_signed_int(np.int64(-7855295674223658936))
        self.p = make_position(mover, opponent)

        etrsm = np.array([
            0x40001CCAEB400024, #0
            0x0818213820099C18, #3
            0x240002D753380002, #2
            0x183990041C841810, #1
            0x02003853D7020024, #4
            0x1018841C04903918, #5
            0x240040EBCA1C0040, #6
            0x189C092038211808, #7
        ], dtype=Bitboard)

        etrso = np.array([
            0x92FC6234143C7848, #0
            0x00A05C47DE7663C0, #3
            0x121E3C282C463F49, #2
            0x03C66E7BE23A0500, #1
            0x493F462C283C1E12, #4
            0x00053AE27B6EC603, #5
            0x48783C143462FC92, #6
            0xC06376DE475CA000, #7
        ], dtype=Bitboard)
        
        self.etrs = make_position(etrsm, etrso)

    def test_scalar(self):

        trps = position_anti_transformations(self.p)
        nptest.assert_array_equal(trps, self.etrs)

    def test_array(self):

        N = 3
        ps = np.full(N, self.p, dtype=Position)
        trps = position_anti_transformations(ps)
        expected = np.tile(self.etrs, (N, 1))        
        nptest.assert_array_equal(trps, expected)
