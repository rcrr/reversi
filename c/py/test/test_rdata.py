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
        self.pset = PatternSet("TestPatternSet", [edge, elle, diag3, core])

    def tearDown(self):
        pass

    def test_init(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        self.assertEqual(rids.level, 0)
        self.assertEqual(rids.indexes, None)

    def test_init_invalid_rds_type(self):
        with self.assertRaises(TypeError):
            RegabIndexedDataSet(None, self.pset)

    def test_init_invalid_pset_type(self):
        with self.assertRaises(TypeError):
            RegabIndexedDataSet(self.rds, None)

    def test_compute_indexes(self):
        rids = RegabIndexedDataSet(self.rds, self.pset)
        rids.compute_indexes()
        self.assertEqual(rids.level, 1)
