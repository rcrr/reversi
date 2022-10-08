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
# -1- It is needed to HotEncoding and to collapse the four ( or 2 or 8 ) instance values
#     into a single UNION.
#     It is done by SQL (duckdb) transformations.
# -2- Build the X and y numpy array.
#     X is a data frame having as row the game position and as columns the GLM variables.
#     X must be sparse. No way otherwise it is huge.
#     All columns must be float32.
#     y is an array having the expected value for the game position.
#     in case of a Logistic Regression y should be 0 or 1 dpending on value set as border between
#     win or lose.
#
#     In practice we need to build the REGAB/RGML machinery in a different way.
#
#     *** what happens when a pattern configuation is missing fro the data ?
#     *** run many solutions of the logit model having a different win/lose boundary
#
# -3- We need a Model class. This class is collecting:
#     .. the REGAB query
#     .. returned RESULT SET ( a pandas Data Frame )
#     .. Feature/Pattern configuration
#     .. extended DataFrame with Feature/Pattern value/indexes
#     .. EXPANDED sparse matrix
#     .. hyperparameters
#     .. REGRESSION MODEL
#     .. computed weights
#
#
#
#    f(x, y) = x2 – 4xy + 5y2 – 4y + 3 has a min value.
#    Find the value of x and y when f(x, y) is minimum.
#    This is the first time I met this question, and here is my way:
#    First I made f'(x) = 2x – 4y = 0.
#    Therefore x = 2y.
#    Then I input x = 2y to f(x, y) = y2 – 4y + 3 = (y – 2)2 – 1
#    Thus min value = -1
#
#    x = 4
#    y = 2

from scipy.optimize import minimize

def fun0(x, y):
    return x**2 -4*x*y + 5*y**2 - 4*y + 3

def fun(z):
    return fun0(z[0], z[1])

def gradient(z):
    x = z[0]
    y = z[1]
    dx = 2*x -4*y
    dy = -4*x +10*y -4
    grad = np.array([dx, dy])
    return grad

def hessian(z):
    x = z[0]
    y = z[1]
    dxx = 2
    dxy = -4
    dyx = -4
    dyy = 10
    h = np.array([[dxx, dxy], [dyx, dyy]])
    return h

def go():
    x0 = [0.0, 0.0]
    return minimize(fun, x0, method='BFGS', tol=1.e-8)

# >>> go()
#     fun: -1.0
#     hess_inv: array([[2.50000129, 1.00000052],
#                      [1.00000052, 0.50000021]])
#     jac: array([0., 0.])
#     message: 'Optimization terminated successfully.'
#     nfev: 24
#     nit: 5
#     njev: 8
#     status: 0
#     success: True
#     x: array([4.00000002, 2.        ])
#                                      

def gog():
    x0 = [0.0, 0.0]
    return minimize(fun, x0, jac=gradient, method='BFGS', tol=1.e-8)

# >>> gog()
#     fun: -1.0000000000000018
#     hess_inv: array([[2.5, 1. ],
#                      [1. , 0.5]])
#     jac: array([0., 0.])
#     message: 'Optimization terminated successfully.'
#     nfev: 6
#     nit: 4
#     njev: 6
#     status: 0
#     success: True
#     x: array([4., 2.])
#

def go_lbfgsb():
    x0 = [0.0, 0.0]
    return minimize(fun, x0, jac=gradient, method='L-BFGS-B', tol=1.e-8)

def go_newton():
    x0 = [0.0, 0.0]
    return minimize(fun, x0, jac=gradient, hess=hessian, method='Newton-CG', tol=1.e-8)

# >>> go_newton()
#     fun: -1.0
#     jac: array([0., 0.])
#     message: 'Optimization terminated successfully.'
#     nfev: 5
#     nhev: 5
#     nit: 5
#     njev: 5
#     status: 0
#     success: True
#     x: array([4., 2.])
#

def go_cg():
    x0 = [0.0, 0.0]
    return minimize(fun, x0, jac=gradient, method='CG', tol=1.e-8, options={'disp': True})

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
