#
# test_binio.py
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
# How to use the unit tests binio module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# time PERF=0 PYTHONPATH="./py" python3 -m unittest twolm.test.test_binio
#

"""
Unit tests for the binio module.
"""

import hashlib
import io
import os
import tempfile
import unittest

import numpy as np

import twolm.binio as binio
from twolm.binio import BinaryFormatError, BinaryReader, BinaryWriter


class ScalarRoundTripTest(unittest.TestCase):
    """Each scalar type must survive a write/read round trip, at its bounds."""

    def _round_trip(self, write_name, read_name, values):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            writer = getattr(w, write_name)
            for v in values:
                writer(v)
        buf.seek(0)
        with BinaryReader(buf) as r:
            reader = getattr(r, read_name)
            for v in values:
                self.assertEqual(reader(), v)

    def test_u8(self):
        self._round_trip("write_u8", "read_u8", [0, 1, 127, 255])

    def test_i8(self):
        self._round_trip("write_i8", "read_i8", [-128, -1, 0, 127])

    def test_u16(self):
        self._round_trip("write_u16", "read_u16", [0, 1, 32768, 65535])

    def test_i16(self):
        self._round_trip("write_i16", "read_i16", [-32768, -1, 0, 32767])

    def test_u32(self):
        self._round_trip("write_u32", "read_u32", [0, 123456, 4_294_967_295])

    def test_i32(self):
        self._round_trip("write_i32", "read_i32", [-2_147_483_648, 0, 2_147_483_647])

    def test_u64(self):
        self._round_trip("write_u64", "read_u64", [0, 18_446_744_073_709_551_615])

    def test_i64(self):
        self._round_trip(
            "write_i64", "read_i64", [-9_223_372_036_854_775_808, 0, 9_223_372_036_854_775_807]
        )

    def test_f32(self):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_f32(3.5)  # exactly representable in float32
            w.write_f32(-0.0)
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_f32(), 3.5)
            self.assertEqual(r.read_f32(), -0.0)

    def test_f64(self):
        buf = io.BytesIO()
        value = 2.718281828459045
        with BinaryWriter(buf) as w:
            w.write_f64(value)
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_f64(), value)


class StringTest(unittest.TestCase):
    def test_roundtrip_ascii(self):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_string("hello world")
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_string(), "hello world")

    def test_roundtrip_multibyte_utf8(self):
        # Length must be measured in bytes, not characters.
        text = "città è caffè ☕ 日本語"
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_string(text)
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_string(), text)

    def test_empty_string(self):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_string("")
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_string(), "")

    def test_byte_length_not_char_length(self):
        # "é" is one character but two bytes in UTF-8; the byte after it must
        # still round-trip, proving the length prefix counts bytes.
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_string("é")
            w.write_u8(99)
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_string(), "é")
            self.assertEqual(r.read_u8(), 99)


class ArrayTest(unittest.TestCase):
    DTYPES = [
        np.int8, np.uint8, np.int16, np.uint16,
        np.int32, np.uint32, np.int64, np.uint64,
        np.float32, np.float64,
    ]

    def _round_trip(self, arr):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_array(arr)
        buf.seek(0)
        with BinaryReader(buf) as r:
            out = r.read_array()
        self.assertEqual(out.dtype, arr.dtype)
        self.assertEqual(out.shape, arr.shape)
        self.assertTrue(np.array_equal(out, arr))
        return out

    def test_all_dtypes_1d(self):
        for dt in self.DTYPES:
            with self.subTest(dtype=dt):
                self._round_trip(np.arange(5).astype(dt))

    def test_multidimensional(self):
        self._round_trip(np.arange(24, dtype=np.float64).reshape(2, 3, 4))

    def test_empty_array(self):
        out = self._round_trip(np.array([], dtype=np.int32))
        self.assertEqual(out.size, 0)

    def test_zero_dimensional(self):
        self._round_trip(np.array(42, dtype=np.int64))

    def test_result_is_writable(self):
        # read_array must return an owned, writable array (not a buffer view).
        out = self._round_trip(np.arange(3, dtype=np.int32))
        out[0] = 123  # must not raise
        self.assertEqual(out[0], 123)

    def test_non_contiguous_input(self):
        # A transposed (F-contiguous) view must still be stored correctly.
        base = np.arange(6, dtype=np.int32).reshape(2, 3)
        self._round_trip(base.T)

    def test_unsupported_dtype_raises(self):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            with self.assertRaises(BinaryFormatError):
                w.write_array(np.array([1 + 2j], dtype=np.complex128))


class HeaderTest(unittest.TestCase):
    def test_roundtrip(self):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_header("my dataset", version=3)
            w.write_u32(7)
        buf.seek(0)
        with BinaryReader(buf) as r:
            header = r.read_header()
            self.assertEqual(header.description, "my dataset")
            self.assertEqual(header.version, 3)
            self.assertEqual(r.read_u32(), 7)

    def test_wrong_signature_raises(self):
        buf = io.BytesIO()
        with BinaryWriter(buf, signature=0xAAAAAAAA) as w:
            w.write_header("x", version=1)
        buf.seek(0)
        with BinaryReader(buf, signature=0xBBBBBBBB) as r:
            with self.assertRaises(BinaryFormatError):
                r.read_header()


class FramingTest(unittest.TestCase):
    def test_magic_mismatch_raises(self):
        # A reader configured with a different MAGIC must reject the stream.
        buf = io.BytesIO()
        with BinaryWriter(buf, magic=0x11111111) as w:
            w.write_u32(1)
        buf.seek(0)
        with BinaryReader(buf, magic=0x22222222) as r:
            with self.assertRaises(BinaryFormatError):
                r.read_u32()

    def test_wrong_read_order_detected(self):
        # Writing a string then reading it as a scalar lands on a non-magic
        # byte sequence and must fail (framing protects against desync).
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_string("x" * 100)
        buf.seek(0)
        with BinaryReader(buf) as r:
            # The string's length prefix (100) is not the MAGIC, so the very
            # first scalar magic check fails.
            with self.assertRaises(BinaryFormatError):
                r.read_u64()
                r.read_u64()

    def test_truncated_file_raises(self):
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_u64(123)
        truncated = buf.getvalue()[:-2]  # drop part of the trailing magic
        with BinaryReader(io.BytesIO(truncated)) as r:
            with self.assertRaises(BinaryFormatError):
                r.read_u64()


class Sha256SidecarTest(unittest.TestCase):
    """Tests for the standalone sidecar functions (checksum service off)."""

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.path = os.path.join(self.dir, "data.bin")
        # checksum=False so no sidecar is auto-created; these tests drive the
        # standalone write_sha256_sidecar / verify_sha256_sidecar functions.
        with BinaryWriter(self.path, checksum=False) as w:
            w.write_header("integrity demo", version=1)
            w.write_array(np.arange(100, dtype=np.float64))

    def tearDown(self):
        for name in os.listdir(self.dir):
            os.remove(os.path.join(self.dir, name))
        os.rmdir(self.dir)

    def test_write_and_verify(self):
        sidecar = binio.write_sha256_sidecar(self.path)
        self.assertTrue(os.path.exists(sidecar))
        self.assertTrue(binio.verify_sha256_sidecar(self.path))

    def test_sidecar_format_is_sha256sum_compatible(self):
        binio.write_sha256_sidecar(self.path)
        with open(self.path + ".SHA256", encoding="ascii") as f:
            line = f.readline().strip()
        digest, name = line.split("  ", 1)
        self.assertEqual(len(digest), 64)
        self.assertEqual(name, "data.bin")
        self.assertEqual(digest, binio.compute_sha256(self.path))

    def test_detects_corruption(self):
        binio.write_sha256_sidecar(self.path)
        # Flip one byte in the payload.
        with open(self.path, "r+b") as f:
            f.seek(20)
            original = f.read(1)
            f.seek(20)
            f.write(bytes([original[0] ^ 0xFF]))
        self.assertFalse(binio.verify_sha256_sidecar(self.path))

    def test_missing_sidecar_raises(self):
        with self.assertRaises(FileNotFoundError):
            binio.verify_sha256_sidecar(self.path)

    def test_malformed_sidecar_raises(self):
        with open(self.path + ".SHA256", "w", encoding="ascii") as f:
            f.write("not-a-digest  data.bin\n")
        with self.assertRaises(BinaryFormatError):
            binio.verify_sha256_sidecar(self.path)


class AutoChecksumTest(unittest.TestCase):
    """The built-in incremental checksum service (checksum=True default)."""

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.path = os.path.join(self.dir, "data.bin")

    def tearDown(self):
        for name in os.listdir(self.dir):
            os.remove(os.path.join(self.dir, name))
        os.rmdir(self.dir)

    def test_sidecar_written_on_close(self):
        # No explicit sidecar call: it must appear automatically at close.
        with BinaryWriter(self.path) as w:
            w.write_header("auto", version=1)
            w.write_u32(7)
        self.assertTrue(os.path.exists(self.path + ".SHA256"))
        self.assertTrue(binio.verify_sha256_sidecar(self.path))

    def test_incremental_matches_reread(self):
        # The incrementally-computed digest must equal a full re-read.
        with BinaryWriter(self.path) as w:
            w.write_string("payload")
            w.write_array(np.arange(50, dtype=np.int64))
        with open(self.path + ".SHA256", encoding="ascii") as f:
            digest = f.readline().split()[0]
        self.assertEqual(digest, binio.compute_sha256(self.path))

    def test_disabled_writes_no_sidecar(self):
        with BinaryWriter(self.path, checksum=False) as w:
            w.write_u32(1)
        self.assertFalse(os.path.exists(self.path + ".SHA256"))

    def test_digest_property_on_file_object(self):
        # With a file object (no path) the digest is exposed, not a sidecar.
        buf = io.BytesIO()
        with BinaryWriter(buf) as w:
            w.write_u32(123)
            digest = w.sha256_hexdigest
        self.assertEqual(digest, hashlib.sha256(buf.getvalue()).hexdigest())

    def test_digest_property_raises_when_disabled(self):
        buf = io.BytesIO()
        with BinaryWriter(buf, checksum=False) as w:
            with self.assertRaises(ValueError):
                _ = w.sha256_hexdigest


class FullDocumentTest(unittest.TestCase):
    """End-to-end: header + mixed scalars + string + arrays, ordered read."""

    def test_mixed_document(self):
        buf = io.BytesIO()
        a32 = np.linspace(0, 1, 5, dtype=np.float32)
        a64 = np.arange(6, dtype=np.int64).reshape(2, 3)
        with BinaryWriter(buf) as w:
            w.write_header("mixed", version=2)
            w.write_string("payload")
            w.write_i16(-1000)
            w.write_u16(1000)
            w.write_f64(0.125)
            w.write_array(a32)
            w.write_array(a64)
        buf.seek(0)
        with BinaryReader(buf) as r:
            self.assertEqual(r.read_header(), ("mixed", 2))
            self.assertEqual(r.read_string(), "payload")
            self.assertEqual(r.read_i16(), -1000)
            self.assertEqual(r.read_u16(), 1000)
            self.assertEqual(r.read_f64(), 0.125)
            self.assertTrue(np.array_equal(r.read_array(), a32))
            self.assertTrue(np.array_equal(r.read_array(), a64))


if __name__ == "__main__":
    unittest.main()
