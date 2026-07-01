#
# test_usage_binio.py
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
# How to use the unit tests usage binio module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# time PERF=0 PYTHONPATH="./py" python3 -m unittest twolm.test.test_usage_binio
#

"""
Linear, copy-pasteable usage examples for twolm.binio.

These tests double as documentation. Each one is a single straight sequence
with no helper methods, showing a complete write -> read cycle.

Two styles are shown:
  1. context manager (`with`) + automatic checksum on close;
  2. manual open / close (no `with`), for when the surrounding program
     structure makes a context manager awkward.
"""

import os
import shutil
import tempfile
import unittest

import numpy as np

from twolm import binio


class UsageWithContextManagerTest(unittest.TestCase):
    def test_write_and_read_back(self):
        # 1. Temporary working directory (auto-removed at the end).
        tmpdir = tempfile.mkdtemp(prefix="binio_usage_")
        self.addCleanup(shutil.rmtree, tmpdir)
        path = os.path.join(tmpdir, "example.bin")

        # 2. Sample data: one value per supported type, plus two arrays.
        description = "binio usage example"
        version = 1
        text = "città è caffè ☕"          # multi-byte UTF-8 on purpose
        v_u8, v_i8 = 200, -50
        v_u16, v_i16 = 60000, -30000
        v_u32, v_i32 = 4_000_000_000, -1_000_000
        v_u64, v_i64 = 18_000_000_000_000_000_000, -9_000_000_000
        v_f32, v_f64 = 1.5, 3.141592653589793
        arr_i64 = np.arange(6, dtype=np.int64).reshape(2, 3)
        arr_f32 = np.linspace(0.0, 1.0, 4, dtype=np.float32)

        # 3. Write everything in order. checksum=True by default, so the
        #    .SHA256 sidecar is written automatically when the block exits.
        with binio.BinaryWriter(path) as w:
            w.write_header(description, version)
            w.write_string(text)
            w.write_u8(v_u8)
            w.write_i8(v_i8)
            w.write_u16(v_u16)
            w.write_i16(v_i16)
            w.write_u32(v_u32)
            w.write_i32(v_i32)
            w.write_u64(v_u64)
            w.write_i64(v_i64)
            w.write_f32(v_f32)
            w.write_f64(v_f64)
            w.write_array(arr_i64)
            w.write_array(arr_f32)
        # File closed, sidecar already on disk -- no explicit hashing call.

        # 4. Verify whole-file integrity.
        self.assertTrue(binio.verify_sha256_sidecar(path))

        # 5. Reopen and read everything back, in the same order.
        with binio.BinaryReader(path) as r:
            header = r.read_header()
            self.assertEqual(header.description, description)
            self.assertEqual(header.version, version)
            self.assertEqual(r.read_string(), text)
            self.assertEqual(r.read_u8(), v_u8)
            self.assertEqual(r.read_i8(), v_i8)
            self.assertEqual(r.read_u16(), v_u16)
            self.assertEqual(r.read_i16(), v_i16)
            self.assertEqual(r.read_u32(), v_u32)
            self.assertEqual(r.read_i32(), v_i32)
            self.assertEqual(r.read_u64(), v_u64)
            self.assertEqual(r.read_i64(), v_i64)
            self.assertEqual(r.read_f32(), v_f32)
            self.assertEqual(r.read_f64(), v_f64)
            self.assertTrue(np.array_equal(r.read_array(), arr_i64))
            self.assertTrue(np.array_equal(r.read_array(), arr_f32))


class UsageManualOpenCloseTest(unittest.TestCase):
    def test_write_and_read_back(self):
        # Same cycle, but without `with`. This is handy when the program
        # structure is more complex and the writer/reader must live across
        # several functions. try/finally guarantees the file is closed (and,
        # for the writer, that the checksum sidecar is written on close).
        tmpdir = tempfile.mkdtemp(prefix="binio_usage_")
        self.addCleanup(shutil.rmtree, tmpdir)
        path = os.path.join(tmpdir, "manual.bin")

        text = "no-with example"
        arr = np.arange(10, dtype=np.float64)

        # --- write ---
        writer = binio.BinaryWriter(path)
        try:
            writer.write_header("manual lifecycle", version=1)
            writer.write_string(text)
            writer.write_i32(-12345)
            writer.write_array(arr)
        finally:
            writer.close()   # writes manual.bin.SHA256 from the in-memory digest

        self.assertTrue(binio.verify_sha256_sidecar(path))

        # --- read ---
        reader = binio.BinaryReader(path)
        try:
            header = reader.read_header()
            self.assertEqual(header.description, "manual lifecycle")
            self.assertEqual(header.version, 1)
            self.assertEqual(reader.read_string(), text)
            self.assertEqual(reader.read_i32(), -12345)
            self.assertTrue(np.array_equal(reader.read_array(), arr))
        finally:
            reader.close()


if __name__ == "__main__":
    unittest.main()
