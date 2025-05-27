#
# test_optimization.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2025 Roberto Corradini. All rights reserved.
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
# From the $(REVERSI_HOME)/c directory run this test module doing:
#
# $ source py/.reversi_venv/bin/activate
# $ PYTHONPATH="./py" python3 -m unittest ./py/test/test_optimization.py 
#
# For a single test run:
#
# $ PYTHONPATH="./py" python3 -m unittest test.test_optimization.TestOptimizationBaseFunction.test_a_optimization
#

import unittest
import numpy as np

from reversi.optimization import * 

"""
The function f has a min value at ( x = -1.0 , y = 1.5 ) equal to -1.25:

  f(x, y) = x - y + 2xy + 2x^2 + y^2

The Jacobian [df/dx, df/dy] is:

  df/dx = 1 + 2y + 4x
  df/dy = -1 + 2x + 2y

The Jacobian is equal to [0.0, 0.0] at [x=-1.0, y=1.5] as it must be.

The Hessian [[df2/dxx, df2/dxy],[df2/dyx, df2/dyy]] is:

  df2/dxx = 4
  df2/dxy = 2
  df2/dyx = 2
  df2/dyy = 2

It is constant and definite-positive, assuring that the stationary point is a minimum.

"""

class TestOptimizationBaseFunction(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(TestOptimizationBaseFunction, self).__init__(*args, **kwargs)
        self.x_min = np.array([-1.0, 1.5])
    
    def fun(self, x):
        return x[0] - x[1] + 2*x[0]*x[1] + 2*x[0]**2 + x[1]**2

    def jacobian(self, x):
        return np.array([ 1 + 2*x[1] + 4*x[0], 
                      -1 + 2*x[0] + 2*x[1] ])

    def hessian(self, x):
        return np.array([[4., 2.], [2., 2.]])

    def test_a_optimization(self):
        x0 = np.array([0.0, 0.0])
        opt = Optimization(x0, self.fun, self.jacobian)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-6)

    def test_b_optimization(self):
        x0 = np.array([0.0, 0.0])
        opt = Optimization(x0, self.fun, self.jacobian, line_search_method='backtrack')
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-6)

    def test_c_optimization(self):
        x0 = np.array([0.0, 0.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='conjugate_gradient', line_search_method='strong_wolfe', verbosity=0)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-6)

    def test_d_optimization(self):
        x0 = np.array([0.0, 0.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='steepest_descent', line_search_method='backtrack')
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-6)

    def test_z_optimization(self):
        x0s = [
            np.array([ 0.0,  0.0]),
            np.array([ 1.0,  1.0]),
            np.array([-1.0, -1.0]),
            np.array([12.0,  5.7]),
            np.array([-1.0,  1.5]),
        ]
        algos = ['steepest_descent', 'conjugate_gradient']
        lsm = ['backtrack', 'strong_wolfe']
        for ls in lsm:
            for algo in algos:
                for x0 in x0s:
                    opt = Optimization(x0, self.fun, self.jacobian,
                                       algorithm=algo, line_search_method=ls,
                                       max_iters=1000)
                    actual_x_min = opt.minimize()
                    np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-6)


#
#
# Rosenbrock function
#
# f(x,y) = (a - x)**2 + b * (y - x**2)**2
#
# It ha a global minimum at (x,y) = (a,a**2) whre f(x,y) = 0
# Usually a and b are chosen such that a = 1 and b = 100
#
# https://en.wikipedia.org/wiki/Rosenbrock_function
#
# Amazing difference between the four available options:
# In order to find the solution with a tolerance of 1.e-5 th iteration count has a large deviation:
#
#   conjugate_gradient + strong_wolfe :  1,000
#   conjugate_gradient + backtrack    :    300
#   steepest_descent   + strong_wolfe :  9,000
#   steepest_descent   + backtrack    : 50,000
#
class TestOptimizationRosenbrockFunction(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(TestOptimizationRosenbrockFunction, self).__init__(*args, **kwargs)
        self.x_min = np.array([1.0, 1.0])
        self.fun_min_value = 0.0
    
    def fun(self, x):
        return (1 - x[0])**2 + 100 * (x[1] - x[0]**2)**2

    def jacobian(self, x):
        return np.array([-2 * (1 - x[0]) - 400 * (x[1] - x[0]**2)*x[0], 
                         200*(x[1] - x[0]**2) ])

    def test_a_optimization(self):
        x0 = np.array([-1.0, -1.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='conjugate_gradient', line_search_method='strong_wolfe', max_iters=1000, verbosity=0)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-5)

    def test_b_optimization(self):
        x0 = np.array([-1.0, -1.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='conjugate_gradient', line_search_method='backtrack', max_iters=1000, verbosity=0)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-5)

    def test_c_optimization(self):
        x0 = np.array([-1.0, -1.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='steepest_descent', line_search_method='strong_wolfe', max_iters=9000, verbosity=0)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-5)

    def test_d_optimization(self):
        x0 = np.array([-1.0, -1.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='steepest_descent', line_search_method='backtrack', max_iters=50000, verbosity=0)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-5)

    def test_z_optimization(self):
        x0 = np.array([-1.0, -1.0])
        opt = Optimization(x0, self.fun, self.jacobian, algorithm='conjugate_gradient', line_search_method='backtrack', c1=1e-4, max_iters=1000, verbosity=0)
        actual_x_min = opt.minimize()
        n_iter = opt.i
        actual_fun_min_value = opt.fun(actual_x_min)
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-5)
        self.assertAlmostEqual(self.fun_min_value, actual_fun_min_value, delta=1.e-7)
        if False: opt.print()

#
# Zakharov Function.
#
#
class TestOptimizationZakharovFunction(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(TestOptimizationZakharovFunction, self).__init__(*args, **kwargs)
        self.fun_min_value = 0.0

    def fun(self, x):
        d = self.d
        sum1 = 0.
        sum2 = 0.
        for i in range(0, d):
            xi = x[i]
            sum1 += xi * xi
            sum2 += 0.5 * (i + 1) * xi
        sum22 = sum2 * sum2
        y = sum1 + sum22 + sum22 * sum22
        return y

    def jacobian(self, x):
        d = self.d
        sum2 = 0.
        a = np.array([x for x in range(1, 1 + d)], dtype=np.double)
        for i in range(0, d):
            xi = x[i]
            sum2 += 0.5 * (i + 1) * xi
        sum22 = sum2 * sum2
        y = 2. * x + ( 1. + 2. * sum22 ) * sum2 * a
        return y

    def test_a_optimization(self, d=10):
        self.d = d
        self.x_min = np.zeros((d,), dtype=np.double)
        x0 = np.random.uniform(low=-5., high=5., size=(d,))

        opt = Optimization(x0, self.fun, self.jacobian,
                           algorithm='conjugate_gradient', line_search_method='strong_wolfe',
                           min_grad=(1.e-6, 3),
                           min_p_fun_decrease=(1.e-8, 3),
                           max_iters=100, verbosity=0)
        actual_x_min = opt.minimize()
        np.testing.assert_allclose(actual_x_min, self.x_min, atol=1.e-5)

