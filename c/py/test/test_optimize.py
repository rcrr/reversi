#
# test_optimize.py
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
import numpy as np
import scipy
from scipy.optimize import minimize

"""
The function f has a min value at ( x = 4.0 , y = 2.0 ) equal to -1:

  f(x, y) = x^2 - 4xy + 5y^2 - 4y + 3

The Jacobian [df/dx, df/dy] is:

  df/dx = 2x -4y
  df/dy = -4x +10y -4

The Jacobian is equal to [0.0, 0.0] at [x=4, y=2] as it must be.

The Hessian [[df2/dxx, df2/dxy],[df2/dyx, df2/dyy]] is:

  df2/dxx = 2
  df2/dxy = -4
  df2/dyx = -4
  df2/dyy = 10

It is constant and definite-positive, assuring that the stationary point is a minimum.

"""

class TestOptimize(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(TestOptimize, self).__init__(*args, **kwargs)
        self.x0 = np.array([0.0, 0.0])
        self.x_min = np.array([4.0, 2.0])
    
    def fun(self, x):
        return x[0]**2 -4*x[0]*x[1] +5*x[1]**2 -4*x[1] +3

    def jacobian(self, x):
        return np.array([2*x[0] -4*x[1], -4*x[0] +10*x[1] -4])

    def hessian(self, x):
        return np.array([[2., -4.], [-4., 10.]])

    def aux_assert_result(self, result):
        self.assertEqual(True, result.success)        

        self.assertAlmostEqual(-1.0, result.fun, delta=1.e-7)
        
        self.assertAlmostEqual(0.0, result.jac[0], delta=1.e-7)
        self.assertAlmostEqual(0.0, result.jac[1], delta=1.e-7)
        
        self.assertAlmostEqual(4.0, result.x[0], delta=1.e-7)
        self.assertAlmostEqual(2.0, result.x[1], delta=1.e-7)

        if hasattr(result, 'hess_inv') and result.hess_inv is not None:
        
            if isinstance(result.hess_inv, np.ndarray):
                self.assertAlmostEqual(2.5, result.hess_inv[0][0], delta=1.e-3)
                self.assertAlmostEqual(1.0, result.hess_inv[0][1], delta=1.e-3)        
                self.assertAlmostEqual(1.0, result.hess_inv[1][0], delta=1.e-3)
                self.assertAlmostEqual(0.5, result.hess_inv[1][1], delta=1.e-3)

                if isinstance(result.hess_inv, scipy.optimize._lbfgsb_py.LbfgsInvHessProduct):
                    pass

    def test_bfgs(self):
        x0 = [0.0, 0.0]
        result = minimize(self.fun, x0, method='BFGS', tol=1.e-8)
        self.aux_assert_result(result)

    def test_bfgs_with_jac(self):
        x0 = self.x0
        result = minimize(self.fun, x0, jac=self.jacobian, method='BFGS', tol=1.e-8)
        self.aux_assert_result(result)

    def test_lbfgsb_with_jac(self):
        x0 = self.x0
        result = minimize(self.fun, x0, jac=self.jacobian, method='L-BFGS-B', tol=1.e-8)
        self.aux_assert_result(result)

    def test_newton_cg(self):
        x0 = self.x0
        result = minimize(self.fun, x0, jac=self.jacobian, hess=self.hessian, method='Newton-CG', tol=1.e-8)
        self.aux_assert_result(result)

    def test_cg(self):
        x0 = self.x0
        result = minimize(self.fun, x0, jac=self.jacobian, method='CG', tol=1.e-8)
        self.aux_assert_result(result)
