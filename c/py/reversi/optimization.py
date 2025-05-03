#
# optimization.py
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

from __future__ import annotations

import numpy as np
import time

from copy import deepcopy

#
# How to use the optimization module.
#
# -0- Open the python console.
#
# $ source py/.reversi_venv/bin/activate
# $ python3
#
# -1- Load the python module
#
# >>> exec(open("py/reversi/optimization.py").read())
#

class Optimization:
    """
    Wraps the model to be minimized, starting condition, parameters, solver, and result in a single object.

    Optimization example:
    
    The function f has a min value at ( x = -1.0 , y = 1.5 ) equal to -1.25:

      f(x, y) = x - y + 2xy + 2x^2 + y^2
    
    The Jacobian [df/dx, df/dy] is:

      df/dx = 1 + 2y + 4x
      df/dy = -1 + 2x + 2y

    The Jacobian is equal to [0.0, 0.0] at [x=-1.0, y=1.5] as it must be.
    
    How to use the class to model this example:
    
    >>> fun = lambda x : x[0] - x[1] + 2*x[0]*x[1] + 2*x[0]**2 + x[1]**2
    >>> jac = lambda x : np.array([ 1 + 2*x[1] + 4*x[0], -1 + 2*x[0] + 2*x[1] ])
    >>> x0 = np.array([0.0, 0.0])
    >>> algo = 'conjugate_gradient'
    >>> lsm = 'strong_wolfe'
    >>> opt = Optimization(x0, fun, jac, algorithm=algo, line_search_method=lsm)
    >>> opt.minimize()
    array([-1. ,  1.5])
    >>> opt.print()
    Optimization object: <__main__.Optimization object at 0x77d1e1756db0>
    x0: [0. 0.]
    function: <function <lambda> at 0x77d1deb8d3a0>
    gradient: <function <lambda> at 0x77d1e1778d60>
    c1=0.001, c2=0.49, eps=1e-06, max_iters=100, verbosity=0
    algorithm: conjugate_gradient
    line search method: strong_wolfe
    iterations=3, function_call_count=6, gradient_call_count=8
    x: [-1.   1.5]

    Attributes:
    ----------
      x0:                  the starting position vector
      fun:                 function f(x) to be minimized
      grad_fun:            the gradient of f(x) 
      c1:                  sufficient decrease parameter
      c2:                  curvature condition parameter
      eps:                 stopping tolerance such that ||grad f(x)|| < eps
      max_iters:           maximum number of iterations
      line_search_method:  must be in ['strong_wolfe', 'backtrack']
      algorithm:           must be in ['steepest_descent', 'conjugate_gradient']
      --
      x:                   the iterative solution being calculated and finally returned by calling minimize
      i:                   current iteration
      verbosity:           the higher the more verbose. 0 means no logs, up to 2 that is the max level
      function_call_count: count how many times the function f(x) has been called
      gradient_call_count: count how many calls to gradient

    Functions:
    ---------
      minimize:           call the selected algorithm for minimization
    
    """
    def __init__(self,
                 x0: numpy.ndarray,
                 fun: Callable,
                 grad_fun: Callable, 
                 c1: float =1e-3,
                 c2: float =0.49,
                 eps: float =1e-6, 
                 max_iters: int =100,
                 line_search_method: str ='strong_wolfe',
                 algorithm: str ='steepest_descent',
                 verbosity: int=0):
        
        if not isinstance(x0, np.ndarray):
            raise TypeError('Argument x0 is not an instance of numpy.ndarray')
        if not x0.dtype == 'float64':
            raise TypeError('Argument x0 must be an array having dtype equal to float64')
        if not callable(fun):
            raise TypeError('Argument fun is not callable')
        if not callable(grad_fun):
            raise TypeError('Argument grad_fun is not callable')
        
        line_search_methods = dict([('strong_wolfe', strong_wolfe), ('backtrack', backtrack)])
        if line_search_method not in line_search_methods:
            raise ValueError('Argument line_search_method is out of valid range: {}'
                             .format(list(line_search_methods)) )

        algos = dict([('steepest_descent', self._sd), ('conjugate_gradient', self._cg)])
        if algorithm not in algos:
            raise ValueError('Argument algorithm is out of valid range: {}'
                             .format(list(algos)) )

        self.x0 = x0
        self.fun = fun
        self.grad_fun = grad_fun
        self.c1 = c1
        self.c2 = c2
        self.eps = eps
        self.max_iters = max_iters
        self.line_search_method = line_search_method
        self.algorithm = algorithm
        
        self.i = 0
        self.x = deepcopy(x0)

        self.verbosity = verbosity

        self.line_search = line_search_methods[line_search_method]
        self.minimize = algos[algorithm]

        self.function_call_count = 0
        self.gradient_call_count = 0
        
    def func(self, x: numpy.ndarray) -> float:
        self.function_call_count += 1
        return self.fun(x)

    def grad_func(self, x: numpy.ndarray) -> numpy.ndarray:
        self.gradient_call_count += 1
        return self.grad_fun(x)

    def print(self) -> None:
        print("Optimization object: {}".format(self))
        print("x0: {}".format(self.x0))
        print("function: {}".format(self.fun))
        print("gradient: {}".format(self.grad_fun))
        print("c1={}, c2={}, eps={}, max_iters={}, verbosity={}".format(self.c1, self.c2, self.eps, self.max_iters, self.verbosity))
        print("algorithm: {}".format(self.algorithm))
        print("line search method: {}".format(self.line_search_method))
        print("iterations={}, function_call_count={}, gradient_call_count={}".format(self.i, self.function_call_count, self.gradient_call_count))
        print("x: {}".format(self.x))

    
    def _sd(self) -> numpy.ndarray:
        """
        Solves an unconstrained optimization problem using the steepest descent method.
        Returns an x array that minimize the f(x) function.
        """

        x = self.x
        fun = self.func
        grad_fun = self.grad_func
        line_search = self.line_search
        c1 = self.c1
        c2 = self.c2
        eps = self.eps
        max_iters = self.max_iters
        verbosity = self.verbosity
        
        for i in range(max_iters):
        
            grad = grad_fun(x)
            grad_norm = np.sqrt(np.dot(grad, grad))
            if grad_norm < eps:
                break
            p = - grad
            alpha = line_search(x, fun, grad_fun, p, c1=c1, c2=c2, verbosity=verbosity)
            x += alpha * p
            
            if self.verbosity > 0:
                fun_value = fun(x)
                print("sd iter: [{:n}/{:n}], ".format(i, max_iters - 1), end='')
                print("fun value: {:.6f}, ".format(fun_value), end='')
                print("alpha: {:.8f}, ".format(alpha), end='')
                print("grad_norm: {:.6f}, eps={:.6f}, ".format(grad_norm, eps), end='')
                print("x: {}".format(x))

        self.i += i
        return x

    #
    # Jorge Nocedal - Stephen J. Wright
    # Numerical Optimization - Second Edition
    #
    # - THE FLETCHER–REEVES METHOD
    #   Page 121
    # - Algorithm 5.4 (FR)
    #
    def _cg(self) -> numpy.ndarray:
        """
        Solves an unconstrained optimization problem using the Fletcher-Reeves 
        conjugate gradient method.
        Returns an x array that minimize the f(x) function.
        """

        x = self.x
        fun = self.func
        grad_fun = self.grad_func
        line_search = self.line_search
        c1 = self.c1
        c2 = self.c2
        eps = self.eps
        max_iters = self.max_iters
        verbosity = self.verbosity
    
        p_prev = np.zeros(len(x))

        # Iteration 0
        i = 0
        fun_value = fun(x)
        grad = grad_fun(x)
        grad_grad = np.dot(grad, grad)
        p = - grad

        for i in range(1, max_iters):
            grad_norm = np.sqrt(grad_grad)
            if grad_norm < eps:
                break

            grad_grad_prev = grad_grad
            p_prev[:] = p[:]
                        
            alpha = line_search(x, fun, grad_fun, p, c1=c1, c2=c2, verbosity=verbosity)
            x += alpha * p
            
            grad = grad_fun(x)
            grad_grad = np.dot(grad, grad)

            beta = grad_grad / grad_grad_prev
            p = - grad + beta * p_prev

            # This is an improvement over base FR algorithm.
            # When the direction p is not a descent direction then beta is set to 0.
            # in this way the algorithm falls back to a steepest descent for this
            # iteration.
            if np.dot(grad, p) >= 0.0:
                p = - grad
            
            if verbosity > 0:
                fun_value = fun(x)
                print("cg iter: [{:n}/{:n}], ".format(i, max_iters), end='')
                print("fun value: {:.10f}, ".format(fun_value), end='')
                print("alpha: {:.8f}, ".format(alpha), end='')
                print("grad_norm: {:.6f}, eps={:.8f}, ".format(grad_norm, eps), end='')
                print("x: {}".format(x))

        self.i += i
        return x

#
# End of class Optimization
#



#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - A LINE SEARCH ALGORITHM FOR THE WOLFE CONDITIONS
#   Page 60
# - Algorithm 3.5 (Line Search Algorithm)
#
def strong_wolfe(x: numpy.ndarray,
                 fun: Callable,
                 grad_fun: Callable,
                 p: numpy.ndarray,
                 c1:float =1e-3,
                 c2: float =0.49,
                 alpha: float =1.0,
                 alpha_max: float =2.0,
                 max_iters: int =10,
                 verbosity: int=0) -> float:
    """
    Returns the alpha step length satisfying the strong Wolfe conditions.
    
    The strong Wolfe conditions are among the most widely applicable and useful termination conditions.
    Parameters c1 and c2 must satisfy the condition: 0 < c1 < c2 < 1
    We assume that pk is a descent direction and that fun is bounded below along the direction pk.

    Arguments alpha and alpha_max must satisfy the conditions: 0 < alpha < alpha_max

    Arguments:
      x:         independent variable array
      fun:       function f(x) to be minimized
      grad_fun:  gradient of f(x)
      p:         search direction
      c1:        sufficient decrease parameter
      c2:        curvature condition parameter
      alpha:     initial estimate for the step length
      alpha_max: maximum value of alpha
      max_iters: maximum number of iterations
      verbosity: verbosity level
    """

    # Golden Ratio.
    gr = 1.618033988749

    # Used to report the condition found for termination.
    condition = -1

    # phi(alpha)
    phi = lambda alpha : fun(x + alpha * p)
    # phi'(alpha)
    phi_d1 = lambda alpha : np.dot(grad_fun(x + alpha * p), p)
    
    # alpha_im1 is the alpha value at alpha(i-1)
    phi_0 = phi(0.)
    phi_im1 = phi_0
    phi_d1_0 = phi_d1(0.)
    phi_d1_im1 = phi_d1_0
    i = 1
    alpha_im1 = 0.
    alpha_i = alpha
    if verbosity > 1:
        print("strong_wolfe: header. c1, c2: {}, {}".format(c1, c2))
        print("strong_wolfe: alpha_im1, alpha, alpha_max: {}, {}, {}".format(alpha_im1, alpha, alpha_max))
        print("strong_wolfe: phi_0: {}, phi_d1_0: {}".format(phi_0, phi_d1_0))

    for i in range(max_iters):
        phi_i = phi(alpha_i)
        phi_d1_i = phi_d1(alpha_i)
        if verbosity > 2:
            print("strong_wolfe, iter: {:d}, alpha_i={}, phi_i={}, phi_d1_i={}, phi_im1={}".format(i, alpha_i, phi_i, phi_d1_i, phi_im1))
            
        if phi_i > phi_0 + c1 * alpha_i * phi_d1_0 or (i > 0 and phi_i >= phi_im1):
            condition = 1
            alpha_star = zoom(phi, phi_d1, phi_0, phi_d1_0, alpha_im1, alpha_i, phi_im1, phi_i, phi_d1_im1, phi_d1_i, c1, c2)
            break
        if np.fabs(phi_d1_i) <= - c2 * phi_d1_0:
            condition = 2
            alpha_star = alpha_i
            break
        if phi_d1_i >= 0.:
            condition = 3
            alpha_star = zoom(phi, phi_d1, phi_0, phi_d1_0, alpha_i, alpha_im1, phi_i, phi_im1, phi_d1_i, phi_d1_im1, c1, c2)
            break
        
        alpha_i = gr * alpha_i
        if alpha_i >= alpha_max:
            # Line search failed. Returning alpha_max.
            if verbosity > 1:
                print("strong_wolfe: alpha_i is grown beyond alpha_max. alpha_i={}, alpha_max={}. Returning alpha_max."
                      .format(alpha_i, alpha_max))
            alpha_star = alpha_max
            break

        alpha_im1 = alpha_i
        phi_im1 = phi_i
        phi_d1_im1 = phi_d1_i
    
    if verbosity > 1:
        print("strong_wolfe: iterations={}, condition={:d}, returning alpha_star: {}".format(i + 1, condition, alpha_star))
    return alpha_star

#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - A LINE SEARCH ALGORITHM FOR THE WOLFE CONDITIONS
#   Page 61
# - Algorithm 3.6 (zoom)
#
def zoom(phi: Callable,
         phi_d1: Callable,
         phi_0: float,
         phi_d1_0: float,
         alpha_lo: float,
         alpha_hi: float,
         phi_lo: float,
         phi_hi: float,
         phi_d1_lo: float,
         phi_d1_hi: float,
         c1: float,
         c2: float,
         max_iters: int =20) -> float:
    """
    Returns a step length alpha between alpha_lo and alpha_hi
    that satisfies the strong Wolfe conditions.

    Explanation, the following three properties must be satisfied:

      (a) the interval bounded by alpha_lo and alpha_hi contains step lengths that satisfy the strong Wolfe conditions;
      (b) alpha_lo is, among all step lengths generated so far and satisfying the sufficient decrease condition,
          the one giving the smallest function value;
      (c) alpha_hi is chosen so that phi'(alpha_lo) * (alpha_hi − alpha_lo) < 0.

    Each iteration of zoom generates an iterate alpha_j between alpha_lo and alpha_hi, and then replaces one of
    these endpoints by alpha_j in such a way that the properties (a), (b), and (c) continue to hold.
    
    Arguments:
      phi:       function phi(alpha)
      phi_d1:    first derivative of function phi(alpha), phi'(alpha)
      phi_0:     value phi(alpha=0)
      phi_d1_0:  value of the first derivative phi' when alpha=0
      alpha_lo:  the bound of the interval giving the lesser value of phi
      alpha_hi:  the other bound of the interval 
      phi_lo:    value of phi(alpha) at alpha_lo
      phi_hi:    value of phi(alpha) at alpha_hi
      phi_d1_lo: value of the derivative of phi(alpha) at alpha_lo
      phi_d1_hi: value of the derivative of phi(alpha) at alpha_hi
      c1:        sufficient decrease parameter
      c2:        curvature condition parameter
      max_iters: maximum number of iterations
    """

    if (phi_lo > phi_hi):
        raise ValueError("Zoom: phi_lo must be lesser than phi_hi. alpha_lo={}, phi_lo={}, alpha_hi={}, phi_hi{}"
                         .format(alpha_lo, phi_lo, alpha_hi, phi_hi))
    
    for j in range(max_iters):
        alpha_j = interpolate(alpha_lo, phi_lo, phi_d1_lo, alpha_hi, phi_hi, phi_d1_hi)
        phi_j = phi(alpha_j)
        phi_d1_j = phi_d1(alpha_j)
        if phi_j > phi_0 + c1 * alpha_j * phi_d1_0 or phi_j >= phi_lo:
            alpha_hi = alpha_j
            phi_hi = phi_j
            phi_d1_hi = phi_d1_j
        else:
            if np.fabs(phi_d1_j) <= - c2 * phi_d1_0:
                return alpha_j
            if phi_d1_j * (alpha_hi - alpha_lo) >= 0.:
                alpha_hi = alpha_lo
                phi_hi = phi_lo
                phi_d1_hi = phi_d1_lo
            alpha_lo = alpha_j
            phi_lo = phi_j
            phi_d1_lo = phi_d1_j
    
    # It happens only if the algorithm is exceeding the max iterations threshold.
    # The textbook doesn't cover this occurrence.
    # Here we take a conservative approach by returning the lower bound for alpha.
    return alpha_lo

#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - CUBIC INTERPOLATION
#   Page 59
# - Formula 3.59
#
def interpolate(x0: float,
                y0: float,
                dy0: float,
                x1: float,
                y1: float,
                dy1: float) -> float:
    """
    Returns the x coordinate of the estimated minimum of f(x) in the range [x0,x1].
    Cubic interpolation of function f(x) is adopted.
        
    Arguments:
      x0:  x coordinates at bound a
      y0:  y coordinates at bound a
      dy0: first derivative of y at bound a
      x1:  x coordinates at bound b
      y1:  y coordinates at bound b
      dy1: first derivative of y at bound b
    """
    #print("interpolate. x0, y0, dy0, x1, y1, dy1: {}, {}, {}, {}, {}, {}".format(x0, y0, dy0, x1, y1, dy1))

    d1 = dy0 + dy1 - 3. * ( y0 - y1 ) / ( x0 - x1 )
    d2a = d1**2 - dy0 * dy1
    if d2a < 0.:
        x = 0.5 * ( x0 + x1 )
        return x
    d2 = np.sign( x1 - x0 ) * np.sqrt( d2a )
    x = x1 - ( x1 - x0 ) * ( ( dy1 + d2 - d1 ) / ( dy1 - dy0 + 2. * d2 ) )
    return x


#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - SUFFICIENT DECREASE AND BACKTRACKING
#   Page 37
# - Algorithm 3.1 (Backtracking Line Search)
#
def backtrack(x: numpy.ndarray,
              fun: Callable,
              grad_fun: Callable,
              p: numpy.ndarray,
              ro: float =.618033988749,
              alpha: float =1.0,
              c1: float =1e-3,
              c2: float = None,
              max_iters: int =10,
              alpha_min: float =0.0001,
              verbosity: int=0):
    """
    Returns a step length alpha that satisfies the sufficient decrease conditions.
    
    In case a valid value for alpha is not obtained after max_iters iteations,
    alpha_min is returned.

    Arguments:
      x: the independent variable vector
      fun: function f(x) to be minimized
      grad_fun: the gradient of f(x)
      p: the search direction
      ro: the contraction factor must be bounded by 0.0 < ro < 1.0
      alpha: the initial step length must be bounded by alpha > 0.0
      c1: must be bounded by 0.0 < c1 < 1.0
      c2: unused
      max_iters: maximum number of iterations

    Raises a ValueError exception when the given direction is not descent.
    """
    
    # Evaluate the function and gradient at the initial point x
    fun0 = fun(x)
    grad0 = grad_fun(x)
    
    # Compute the derivative of the merit function at alpha = 0.0
    gradp0 = np.dot(grad0, p)
    
    # Check for a descent direction
    if gradp0 >= 0.0:
        raise ValueError("Arguments are not giving a descent direction. fun0={:.6f}, grad0={}, p={}, gradp0={:.8f}"
                         .format(fun0, grad0, p, gradp0))
    
    for k in range(max_iters):
        # Evaluate the function at the new point
        xk = x + alpha * p
        fk = fun(xk)
        
        # Check the sufficient decrease condition
        if fk < fun0 + c1 * alpha * gradp0:
            return alpha
        
        # Set the alpha value
        alpha = ro * alpha
    
    # The line search has failed at this point
    return alpha_min
