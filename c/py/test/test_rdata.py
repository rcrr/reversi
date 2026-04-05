#
# test_rdata.py
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
# How to use the domain module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest test.test_rdata
#

import unittest

import twolm
import twolm.domain
import twolm.rdata

from twolm.domain import *
from twolm.rdata import *

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import pandas as pd

import os
import tempfile
import shutil
from dotenv import load_dotenv


class TestDummy(unittest.TestCase):
    
    def test_dummy(self):
        self.assertEqual(True, True)

#
# A file .env must exist in $(REVERSI_HOME)/c
# The file must have the following definitions:
# DB_NAME=test_regab_database_name
# DB_USER=test_regab_database_user
# DB_PASSWORD=test_regab_database_password
# DB_HOST=test_regab_database_hostname
# DB_PORT=test_regab_database_tcpip_port
#
class BaseTestCase(unittest.TestCase):
    def setUp(self):
        """
        Runs before each individual test. 
        Loads credentials and initializes the database connection.
        """
        load_dotenv()
        self.dbname = os.getenv('DB_NAME')
        self.user = os.getenv('DB_USER')
        self.password = os.getenv('DB_PASSWORD')
        self.host = os.getenv('DB_HOST')
        self.port = os.getenv('DB_PORT', '5432')
        
        # Initialize the connection object
        self.rc = RegabDBConnection(
            self.dbname, self.user, self.host, self.port, self.password
        )
                
        # Assert the connection object exists
        self.assertIsNotNone(self.rc.conn, "Connection object was not created.")
        
        # Assert the psycopg2 connection is actually open (0 = open)
        self.assertEqual(self.rc.conn.closed, 0, "The database connection is closed.")


    def tearDown(self):
        """
        Runs after each individual test. 
        Ensures the database connection is properly closed.
        """
        if hasattr(self, 'rc') and self.rc.conn:
            self.rc.close()
                    
            # Assert the connection is closed (> 0 means closed)
            self.assertNotEqual(self.rc.conn.closed, 0, "The connection should be closed.")


class TestRegabDB(BaseTestCase):

    def test_connection_status(self):
        """
        Asserts that the connection to the database is active.
        In psycopg2, .closed == 0 indicates an open connection.
        """
        self.assertIsNotNone(self.rc.conn, "Connection object was not initialized.")
        self.assertEqual(self.rc.conn.closed, 0, "The database connection is not active.")

    def test_schema_search_path(self):
        """
        Asserts that the search_path was correctly set to 'reversi' during initialization.
        """
        with self.rc.conn.cursor() as curs:
            curs.execute("SHOW search_path;")
            current_path = curs.fetchone()[0]
            self.assertIn("reversi", current_path, f"Search path is set to {current_path} instead of 'reversi'.")


class TestRegabGPAsDF(BaseTestCase):

    def test_valid_query_construction(self):
        """
        Tests that the SQL query is constructed correctly with valid parameters.
        """
        bid = [7, 8]
        status = ['CMS', 'CMR']
        ec = 10
        limit = 5
        where = "game_value > 0"
        fields = ['seq', 'mover', 'opponent', 'game_value']

        df = regab_gp_as_df(self.rc, bid, status, ec, limit, where, fields)

        # Checks that the resulting DataFrame is not empty
        self.assertFalse(df.empty, "The resulting DataFrame is empty.")

    def test_invalid_bid(self):
        """
        Tests that an exception is raised for an invalid bid value.
        """
        with self.assertRaises((TypeError, ValueError)):
            regab_gp_as_df(self.rc, -1, ['CMP'], 10)

    def test_invalid_status(self):
        """
        Tests that an exception is raised for an invalid status value.
        """
        with self.assertRaises((TypeError, ValueError)):
            regab_gp_as_df(self.rc, [1], 'AAAA', 10)

    def test_invalid_ec(self):
        """
        Tests that an exception is raised for an invalid ec value.
        """
        with self.assertRaises((TypeError, ValueError)):
            regab_gp_as_df(self.rc, [1], ['CMP'], 61)

    def test_invalid_limit(self):
        """
        Tests that an exception is raised for an invalid limit value.
        """
        with self.assertRaises((TypeError, ValueError)):
            regab_gp_as_df(self.rc, 1, ['CMP'], 10, -1)

    def test_invalid_where(self):
        """
        Tests that an exception is raised for an invalid where value.
        """
        with self.assertRaises(TypeError):
            regab_gp_as_df(self.rc, 1, 'CMP', 10, 5, where=123)

    def test_invalid_fields(self):
        """
        Tests that an exception is raised for an invalid fields value.
        """
        with self.assertRaises((TypeError, ValueError)):
            regab_gp_as_df(self.rc, [1], ['CMP'], 10, 5, fields='invalid_field')

    def test_all_fields(self):
        """
        Tests that the query includes all fields when fields='*'.
        """
        df = regab_gp_as_df(self.rc, [1], ['CMS'], 20, fields='*')

        # Checks that the DataFrame contains all fields of the table
        with self.rc.conn.cursor() as curs:
            curs.execute("SELECT * FROM regab_prng_gp LIMIT 0;")
            colnames = [d[0] for d in curs.description]
        self.assertListEqual(list(df.columns), colnames, "The DataFrame fields do not match the table fields.")


class TestRegabDataSet(BaseTestCase):

    def test_valid_initialization(self):
        """
        Tests the correct initialization of the RegabDataSet class with valid parameters.
        """
        bid = [1, 2]
        status = ['CMS', 'CMR']
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [4, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })

        dataset = RegabDataSet(bid, status, ec, positions)

        self.assertEqual(dataset.bid, bid)
        self.assertEqual(dataset.status, status)
        self.assertEqual(dataset.ec, ec)
        self.assertEqual(dataset.positions.equals(positions), True)
        self.assertEqual(dataset.length, len(positions))

    def test_invalid_bid_type(self):
        """
        Tests that a TypeError is raised if bid is not a list of integers.
        """
        bid = 'not_a_list'
        status = ['CMS', 'CMR']
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [4, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })

    def test_invalid_bid_value(self):
        """
        Tests that a ValueError is raised if bid contains negative integers.
        """
        bid = [-1, 2]
        status = ['CMS', 'CMR']
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [4, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })
        with self.assertRaises(ValueError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_status_type(self):
        """
        Tests that a TypeError is raised if status is not a list of strings.
        """
        bid = [1, 2]
        status = 'not_a_list'
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [5, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })
        with self.assertRaises(TypeError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_status_length(self):
        """
        Tests that a ValueError is raised if status contains strings not of length 3.
        """
        bid = [1, 2]
        status = ['CMS', 'CM']
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [5, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })
        with self.assertRaises(ValueError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_ec_type(self):
        """
        Tests that a TypeError is raised if ec is not an integer.
        """
        bid = [1, 2]
        status = ['CMS', 'CMR']
        ec = 'not_an_int'
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [5, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })
        with self.assertRaises(TypeError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_ec_value(self):
        """
        Tests that a ValueError is raised if ec is not in the range [0, 60].
        """
        bid = [1, 2]
        status = ['CMS', 'CMR']
        ec = 61
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [4, 6]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })
        with self.assertRaises(ValueError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_positions_type(self):
        """
        Tests that a TypeError is raised if positions is not a pandas DataFrame.
        """
        bid = [1, 2]
        status = ['CMS', 'CMR']
        ec = 10
        positions = 'not_a_dataframe'
        with self.assertRaises(TypeError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_positions_columns(self):
        """
        Tests that a ValueError is raised if positions does not have exactly 3 columns.
        """
        bid = [1, 2]
        status = ['CMS', 'CMR']
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64'
        })
        with self.assertRaises(ValueError):
            RegabDataSet(bid, status, ec, positions)

    def test_invalid_positions_column_types(self):
        """
        Tests that a ValueError is raised if positions has incorrect column types.
        """
        bid = [1, 2]
        status = ['CMS', 'CMR']
        ec = 10
        positions = pd.DataFrame({
            'mover': [1, 2],
            'opponent': [3, 4],
            'game_value': [4.0, 6.0]  # Incorrect type, should be int8
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'float64'
        })
        with self.assertRaises(TypeError):
            RegabDataSet(bid, status, ec, positions)

    def test_extract_data_from_db(self):
        """
        """
        # Load test parameters
        arg_bid = os.getenv('TEST_RDATA_TEST_EXTRACTION_ARG_BID')
        arg_status = os.getenv('TEST_RDATA_TEST_EXTRACTION_ARG_STATUS')
        arg_ec = os.getenv('TEST_RDATA_TEST_EXTRACTION_ARG_EC')
        expected_count_prop = os.getenv('TEST_RDATA_TEST_EXTRACTION_EXPECTED_COUNT')
        
        # Check if required parameters are present
        if arg_bid is None:
            raise ValueError("TEST_RDATA_TEST_EXTRACTION_ARG_BID is not set in the .env file.")
        if arg_status is None:
            raise ValueError("TEST_RDATA_TEST_EXTRACTION_ARG_STATUS is not set in the .env file.")
        if arg_ec is None:
            raise ValueError("TEST_RDATA_TEST_EXTRACTION_ARG_EC is not set in the .env file.")
        
        # Convert parameters to appropriate types
        bid = [int(part) for part in arg_bid.split(',')]
        status = arg_status.split(',')
        ec = int(arg_ec)

        rds = RegabDataSet.extract_from_db(self.rc, bid, status, ec)
        positions = rds.positions
        self.assertFalse(positions.empty, "The DataFrame is empty.")
        self.assertEqual(len(positions.columns), 3, "Columns count is not proper.")

        if expected_count_prop is not None:
            expected_count = int(expected_count_prop)
            self.assertEqual(len(positions), expected_count, "The DataFrame has the wrong len.")


class TestRegabDataSetChecksum(BaseTestCase):

    def setUp(self):
        """
        Runs before each individual test. 
        Creates a sample RegabDataSet instance and a temporary directory.
        """
        super().setUp()
        self.bid = [1, 2]
        self.status = ['CMS', 'CMR']
        self.ec = 20
        self.positions = pd.DataFrame({
            'mover': [4611717676283199524, 72342959909978368],
            'opponent': [-7855295674223658936, 6952639131500418064],
            'game_value': [10, 36]
        }).astype({
            'mover': 'int64', 
            'opponent': 'int64', 
            'game_value': 'int8'
        })
        self.rds = RegabDataSet(self.bid, self.status, self.ec, self.positions)
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.filename = os.path.join(self.tmp_dir, 'test_dataset.bin')

    def tearDown(self):
        """
        Runs after each individual test. 
        Cleans up the temporary directory.
        """
        super().tearDown()
        shutil.rmtree(self.tmp_dir)

    def test_store_and_load(self):
        """
        Tests the storage and loading of RegabDataSet instances to and from a binary file.
        """
        # Store the dataset to the file
        self.rds.store_to_file(self.filename)
        
        # Load the dataset from the file
        loaded_rds = RegabDataSet.load_from_file(self.filename)
        
        # Verify the loaded dataset matches the original
        self.assertEqual(loaded_rds.bid, self.rds.bid)
        self.assertEqual(loaded_rds.status, self.rds.status)
        self.assertEqual(loaded_rds.ec, self.rds.ec)
        self.assertTrue(loaded_rds.positions.equals(self.rds.positions))
        self.assertEqual(loaded_rds.length, self.rds.length)

    def test_load_without_checksum(self):
        """
        Tests loading a RegabDataSet instance without checksum verification.
        """
        # Store the dataset to the file
        self.rds.store_to_file(self.filename)
        
        # Load the dataset from the file without checksum verification
        loaded_rds = RegabDataSet.load_from_file(self.filename, checksum=False)
        
        # Verify the loaded dataset matches the original
        self.assertEqual(loaded_rds.bid, self.rds.bid)
        self.assertEqual(loaded_rds.status, self.rds.status)
        self.assertEqual(loaded_rds.ec, self.rds.ec)
        self.assertTrue(loaded_rds.positions.equals(self.rds.positions))
        self.assertEqual(loaded_rds.length, self.rds.length)

    def test_load_missing_checksum_file(self):
        """
        Tests loading a RegabDataSet instance when the checksum file is missing.
        """
        # Store the dataset to the file
        self.rds.store_to_file(self.filename)
        
        # Remove the checksum file to simulate a missing file
        checksum_filename = self.filename + ".SHA3-256"
        os.remove(checksum_filename)
        
        # Attempt to load the dataset from the file with checksum verification
        with self.assertRaises(FileNotFoundError):
            RegabDataSet.load_from_file(self.filename)

    def test_load_mismatched_checksum(self):
        """
        Tests loading a RegabDataSet instance when the checksum does not match.
        """
        # Store the dataset to the file
        self.rds.store_to_file(self.filename)
        
        # Modify the file to change its checksum
        with open(self.filename, 'ab') as f:
            f.write(b'some extra data')
        
        # Attempt to load the dataset from the file with checksum verification
        with self.assertRaises(ValueError):
            RegabDataSet.load_from_file(self.filename)


class TestRegabIndexedDataSet(BaseTestCase):
    
    def setUp(self):
        filename = 'py/test/data/tlm/ragab_data_file_200pos_ec20.dat'
        self.rds = RegabDataSet.load_from_file(filename)
        
        edge = Pattern('EDGE', SquareSet(0x00000000000000FF))
        elle = Pattern('ELLE', SquareSet(0x0000000000000107))
        diag3 = Pattern('DIAG3', SquareSet(0x0000000000010204))
        core = Pattern('CORE', SquareSet(0x0000001818000000))
        whirl = Pattern('WHIRL', SquareSet(0x83800000000001C1))
        self.pset = PatternSet("TPS", [edge, elle, diag3, core, whirl])

    def tearDown(self):
        pass

    def test_init(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        self.assertIsNotNone(rids.rds)
        self.assertIsNotNone(rids.pset)
        self.assertIsNone(rids.indexes)
        self.assertIsNone(rids.findexes)
        self.assertIsNone(rids.lookup)
        self.assertIsNone(rids.revmap)
        self.assertIsNone(rids.pindexes)

    def test_init_invalid_rds_type(self):
        with self.assertRaises(TypeError):
            RegabIndexedDataSet(None, self.pset)

    def test_init_invalid_pset_type(self):
        with self.assertRaises(TypeError):
            RegabIndexedDataSet(self.rds, None)

    def test_compute_indexes(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_indexes()
        self.assertIsNotNone(rids.indexes)
        self.assertEqual(rids.indexes.shape, (5, 200, 8))

        m = self.rds.positions['mover'].values.view(np.uint64)
        o = self.rds.positions['opponent'].values.view(np.uint64)
        b000 = Board(SquareSet(m[0]),SquareSet(o[0]))
        b023 = Board(SquareSet(m[23]),SquareSet(o[23]))
        b199 = Board(SquareSet(m[199]),SquareSet(o[199]))

        computed_indexes_edge_b000 = rids.indexes[0, 0, :]
        expected_indexes_edge_b000 = np.array([162, 513, 1098, 117, 54, 99, 606, 351], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_edge_b000, expected_indexes_edge_b000)

        computed_indexes_elle_b000 = rids.indexes[1, 0, :]
        expected_indexes_elle_b000 = np.array([0, 0, 18, 36, 0, 18, 12, 0], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_elle_b000, expected_indexes_elle_b000)

        computed_indexes_diag3_b000 = rids.indexes[2, 0, :]
        expected_indexes_diag3_b000 = np.array([0, 6, 26, 16, 6, 26, 16, 0], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_diag3_b000, expected_indexes_diag3_b000)

        computed_indexes_core_b000 = rids.indexes[3, 0, :]
        expected_indexes_core_b000 = np.array([53, 77, 79, 71, 71, 79, 77, 53], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_core_b000, expected_indexes_core_b000)

        computed_indexes_whirl_b000 = rids.indexes[4, 0, :]
        expected_indexes_whirl_b000 = np.array([729, 81, 3, 27, 0, 0, 0, 0], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_whirl_b000, expected_indexes_whirl_b000)

        computed_indexes_edge_b023 = rids.indexes[0, 23, :]
        expected_indexes_edge_b023 = np.array([2304, 2188, 3277, 2179, 352, 2188, 2551, 2913], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_edge_b000, expected_indexes_edge_b000)

        computed_indexes_elle_b023 = rids.indexes[1, 23, :]
        expected_indexes_elle_b023 = np.array([63, 1, 10, 46, 1, 1, 13, 24], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_elle_b023, expected_indexes_elle_b023)

        computed_indexes_diag3_b023 = rids.indexes[2, 23, :]
        expected_indexes_diag3_b023 = np.array([25, 3, 4, 17, 3, 12, 25, 17], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_diag3_b023, expected_indexes_diag3_b023)

        computed_indexes_core_b023 = rids.indexes[3, 23, :]
        expected_indexes_core_b023 = np.array([50, 76, 70, 44, 70, 76, 50, 44], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_core_b023, expected_indexes_core_b023)

        computed_indexes_whirl_b023 = rids.indexes[4, 23, :]
        expected_indexes_whirl_b023 = np.array([3222, 3736, 418, 2464, 2431, 253, 2197, 2439], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_whirl_b023, expected_indexes_whirl_b023)
        
        computed_indexes_edge_b199 = rids.indexes[0, 199, :]
        expected_indexes_edge_b199 = np.array([4642, 566, 51, 3942, 4142, 6498, 2025, 178], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_edge_b199, expected_indexes_edge_b199)

        computed_indexes_elle_b199 = rids.indexes[1, 199, :]
        expected_indexes_elle_b199 = np.array([79, 26, 24, 0, 65, 72, 0, 70], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_elle_b199, expected_indexes_elle_b199)

        computed_indexes_diag3_b199 = rids.indexes[2, 199, :]
        expected_indexes_diag3_b199 = np.array([17, 17, 26, 0, 25, 26, 0, 25], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_diag3_b199, expected_indexes_diag3_b199)

        computed_indexes_core_b199 = rids.indexes[3, 199, :]
        expected_indexes_core_b199 = np.array([77, 79, 71, 53, 79, 77, 53, 71], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_core_b199, expected_indexes_core_b199)

        computed_indexes_whirl_b199 = rids.indexes[4, 199, :]
        expected_indexes_whirl_b199 = np.array([73, 1703, 2835, 4389, 1529, 2427, 6243, 2161], dtype=np.uint32)
        nptest.assert_array_equal(computed_indexes_whirl_b199, expected_indexes_whirl_b199)

    def test_flatten_indexes(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_indexes()
        rids.flatten_indexes()

        self.assertIsNotNone(rids.findexes)
        self.assertIsInstance(rids.findexes, np.ndarray)
        self.assertEqual(rids.findexes.shape, (200, 19))
        self.assertEqual(rids.findexes.dtype, np.uint32)

    def test_compute_principal_indexes(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_principal_indexes()

        self.assertIsNotNone(rids.pindexes)
        self.assertIsInstance(rids.pindexes, np.ndarray)
        self.assertEqual(rids.pindexes.shape, (200, 19))
        self.assertEqual(rids.pindexes.dtype, np.uint32)

        computed_pindexes_b000 = rids.pindexes[0, :]
        expected_pindexes_b000 = np.array([
            54, 99, 606, 117,           # EDGE [162, 513, 1098, 117, 54, 99, 606, 351] -> [min(162, 54), min(513, 99), min(1098, 606), min(117, 315)]
            0, 0, 18, 36, 0, 18, 12, 0, # ELLE [0, 0, 18, 36, 0, 18, 12, 0]
            0, 6, 26, 16,               # DIAG3 [0, 6, 26, 16, 6, 26, 16, 0] -> [min(0, 0), min(6, 6), min(26, 26), min(16, 16)]
            53,                         # CORE [53, 77, 79, 71, 71, 79, 77, 53] -> [min(53, 77, 79, 71, 71, 79, 77, 53)]
            3, 0                        # WHIRL [729, 81, 3, 27, 0, 0, 0, 0] -> [min(729, 81, 3, 27), min(0, 0, 0, 0)]
        ], dtype=np.uint32)
        nptest.assert_array_equal(computed_pindexes_b000, expected_pindexes_b000)

        computed_pindexes_b023 = rids.pindexes[23, :]
        expected_pindexes_b023 = np.array([
            352, 2188, 2551, 2179,       # EDGE [2304, 2188, 3277, 2179, 352, 2188, 2551, 2913]
            63, 1, 10, 46, 1, 1, 13, 24, # ELLE [63, 1, 10, 46, 1, 1, 13, 24]
            17, 3, 4, 17,                # DIAG3 [25, 3, 4, 17, 3, 12, 25, 17]
            44,                          # CORE [50, 76, 70, 44, 70, 76, 50, 44]
            418, 253                     # WHIRL [3222, 3736, 418, 2464, 2431, 253, 2197, 2439]
        ], dtype=np.uint32)
        nptest.assert_array_equal(computed_pindexes_b023, expected_pindexes_b023)

        computed_pindexes_b199 = rids.pindexes[199, :]
        expected_pindexes_b199 = np.array([
            4142, 566, 51, 178,           # EDGE [4642, 566, 51, 3942, 4142, 6498, 2025, 178]
            79, 26, 24, 0, 65, 72, 0, 70, # ELLE [79, 26, 24, 0, 65, 72, 0, 70]
            17, 17, 26, 0,                # DIAG3 [17, 17, 26, 0, 25, 26, 0, 25]
            53,                           # CORE [77, 79, 71, 53, 79, 77, 53, 71]
            73, 1529                      # WHIRL [73, 1703, 2835, 4389, 1529, 2427, 6243, 2161]
        ], dtype=np.uint32)
        nptest.assert_array_equal(computed_pindexes_b199, expected_pindexes_b199)

    def test_compute_lookup(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_lookup()
        
        self.assertIsNotNone(rids.lookup)
        self.assertIsInstance(rids.lookup, np.ndarray)
        self.assertEqual(rids.lookup.shape, (19, 3))
        self.assertEqual(rids.lookup.dtype, np.uint32)

        expected_lookup = np.array([[ 0, 0, 0],
                                    [ 1, 0, 1],
                                    [ 2, 0, 2],
                                    [ 3, 0, 3],
                                    [ 4, 1, 0],
                                    [ 5, 1, 1],
                                    [ 6, 1, 2],
                                    [ 7, 1, 3],
                                    [ 8, 1, 4],
                                    [ 9, 1, 5],
                                    [10, 1, 6],
                                    [11, 1, 7],
                                    [12, 2, 0],
                                    [13, 2, 1],
                                    [14, 2, 2],
                                    [15, 2, 3],
                                    [16, 3, 0],
                                    [17, 4, 0],
                                    [18, 4, 4]], dtype=np.uint32)
        
        nptest.assert_array_equal(rids.lookup, expected_lookup)

    def test_compute_revmap(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_revmap()

        self.assertIsNotNone(rids.revmap)
        self.assertIsInstance(rids.revmap, np.ndarray)
        self.assertEqual(rids.revmap.shape, (5, 8))
        self.assertEqual(rids.revmap.dtype, np.uint32)

        IV = rids.REVMAP_INVALID_VALUE
        expected_revmap = np.array([[  0,  1,  2,  3, IV, IV, IV, IV],
                                    [  4,  5,  6,  7,  8,  9, 10, 11],
                                    [ 12, 13, 14, 15, IV, IV, IV, IV],
                                    [ 16, IV, IV, IV, IV, IV, IV, IV],
                                    [ 17, IV, IV, IV, 18, IV, IV, IV]], dtype=np.uint32)
        nptest.assert_array_equal(rids.revmap, expected_revmap)

    def test_use_lookup(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_lookup()

        # Column 0 refers to EDGE pattern (0), T0
        col_number = 0
        cn_idx, p_idx, t_idx = rids.lookup[col_number]
        expected_p_idx = 0
        expected_t_idx = 0
        self.assertEqual(col_number, cn_idx)
        self.assertEqual(expected_p_idx, p_idx)
        self.assertEqual(expected_t_idx, t_idx)

        # Column 18 refers to WHIRL pattern (4), T4
        col_number = 18
        cn_idx, p_idx, t_idx = rids.lookup[col_number]
        expected_p_idx = 4
        expected_t_idx = 4
        self.assertEqual(col_number, cn_idx)
        self.assertEqual(expected_p_idx, p_idx)
        self.assertEqual(expected_t_idx, t_idx)
        
    def test_use_revmap(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_revmap()

        pattern_index = 4
        transformation_index = 4
        col_idx = rids.revmap[pattern_index, transformation_index]
        expected_col_idx = 18
        self.assertEqual(expected_col_idx, col_idx)

        pattern_index = 4
        col_idxs = rids.get_pattern_columns(pattern_index)
        expected_col_idxs = np.array([17, 18], dtype=np.uint32)
        nptest.assert_array_equal(expected_col_idxs, col_idxs)
