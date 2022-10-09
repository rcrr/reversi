#
# rglm.py
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

import reversi
import reversi.board
import reversi.pattern
import reversi.cfg
import reversi.regab

from reversi.board import *
from reversi.pattern import *
from reversi.cfg import *
from reversi.regab import *

import numpy as np
import pandas as pd

#
# To do:
#
# See the ml/A2063 notebook
#
# imported packages and functions:
from sklearn.preprocessing import OneHotEncoder
import duckdb
from scipy.sparse import csr_matrix
from scipy.sparse import hstack
from sklearn.linear_model import LogisticRegression
#
# -1- We need a Model class. This class is collecting:
#     .. the REGAB query
#     .. returned RESULT SET ( a pandas Data Frame )
#     .. Feature/Pattern configuration
#     .. extended DataFrame with Feature/Pattern value/indexes
#     .. EXPANDED sparse matrix
#     .. hyperparameters
#     .. REGRESSION MODEL
#     .. computed weights
#
# -2- It is needed to HotEncoding and to collapse the four ( or 2 or 8 ) instance values
#     into a single UNION.
#     It is done by SQL (duckdb) transformations.
#
# -3- Build the X and y numpy array.
#     X is a data frame having as row the game position and as columns the GLM variables.
#     X must be sparse. No way otherwise it is huge.
#     All columns must be float32.
#     y is an array having the expected value for the game position.
#     in case of a Logistic Regression y should be 0 or 1 dpending on value set as border between
#     win or lose.
#
#     In practice we need to build the REGAB/RGML machinery in a different way.
#
#     *** what happens when a pattern configuation is missing from the data ?
#     *** run many solutions of the logit model having a different win/lose boundary
#

class Rglm:
    """
    """
    def __init__(self):
        """
        """
        self.conn = None
        self.empty_count = None
        self.batches = None
        self.stauses = None
        self.features = None
        self.patterns = None
        self.game_positions = None

    def set_conn(self, conn: RegabDBConnection) -> 'Rglm':
        if not isinstance(conn, RegabDBConnection):
            raise TypeError('Argument conn is not an instance of RegabDBConnection')
        self.conn = conn
        return self

    def close_conn(self):
        if self.conn:
            self.conn.close()

    def set_empty_count(self, ec: int) -> 'Rglm':
        self.empty_count = regab_empty_count(ec)
        return self

    def set_batches(self, bs) -> 'Rglm':
        self.batches = regab_batches(bs)
        return self

    def set_statuses(self, sts) -> 'Rglm':
        self.statses = regab_statses(sts)
        return self

    def set_features(self, fs) -> 'Rglm':
        self.features = regab_features(fs)
        return self

    def set_patterns(self, ps) -> 'Rglm':
        self.patterns = regab_patterns(ps)
        return self
