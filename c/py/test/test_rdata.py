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

import os
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
class TestRegabDBConnection(unittest.TestCase):

    def setUp(self):
        # Load credentials from .env
        load_dotenv()
        self.dbname = os.getenv('DB_NAME')
        self.user = os.getenv('DB_USER')
        self.password = os.getenv('DB_PASSWORD')
        self.host = os.getenv('DB_HOST')
        self.port = os.getenv('DB_PORT', '5432')

    def test_connection_is_established(self):
        # 1. Create the connection
        rc = RegabDBConnection(self.dbname, self.user, self.host, self.port, self.password)
        
        # 2. Assert the connection object exists
        self.assertIsNotNone(rc.conn, "Connection object was not created.")
        
        # 3. Assert the psycopg2 connection is actually open (0 = open)
        self.assertEqual(rc.conn.closed, 0, "The database connection is closed.")
        
        # Cleanup
        rc.close()
        
    def test_connection_closes_properly(self):
        rc = RegabDBConnection(self.dbname, self.user, self.host, self.port, self.password)
        rc.close()
        
        # Assert the connection is closed (> 0 means closed)
        self.assertNotEqual(rc.conn.closed, 0, "The connection should be closed.")


class TestRegabDB(unittest.TestCase):
    
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

    def tearDown(self):
        """
        Runs after each individual test. 
        Ensures the database connection is properly closed.
        """
        if hasattr(self, 'rc') and self.rc.conn:
            self.rc.close()

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


class TestRegabGPAsDF(unittest.TestCase):

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

    def tearDown(self):
        """
        Runs after each individual test. 
        Ensures the database connection is properly closed.
        """
        if hasattr(self, 'rc') and self.rc.conn:
            self.rc.close()

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
