#
# test_regab.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022 Roberto Corradini. All rights reserved.
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

import unittest

import reversi
import reversi.board
import reversi.pattern
import reversi.regab

from reversi.board import *
from reversi.pattern import *
from reversi.regab import *

import numpy as np
import pandas as pd

import psycopg2 as pg

class TestRegab(unittest.TestCase):

    def test_dummy(self):
        self.assertEqual(1, 1)

    def test_db_connection(self):
        # Connect to the REGAB tests database
        conn = pg.connect("dbname=tst_regab user=tst_regab host=localhost port=5432")

        # Open a cursor to perform database operations
        cur = conn.cursor()

        cur.execute("SET search_path TO reversi;")

        cur.execute("SELECT * FROM regab_prng_gp_h LIMIT 0;")
        colnames = [d[0] for d in cur.description]
        self.assertIsNotNone(colnames)
        
        cur.execute("SELECT * FROM regab_prng_gp_h;")
        res = cur.fetchall()        
        self.assertIsNotNone(res)

        # Clese the cursor
        cur.close()

        # Close communication with the database
        conn.close()

    def test_init(self):
        rc = RegabDBConnection('tst_regab', 'tst_regab', 'localhost')
        self.assertIsNotNone(rc)
        rc.close()

    def test_new_from_config(self):
        from reversi.cfg import Cfg
        rc = RegabDBConnection.new_from_config('py/test/data/regab_test.cfg', 'test')
        self.assertIsNotNone(rc)
        rc.close()
