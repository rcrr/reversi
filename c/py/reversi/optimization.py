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
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest test.test_optimization
#
# -2- Open the python console.
#
# $ python3
#
# -3- In case of changes to the code, reload the python module.
#
# >>> exec(open("py/reversi/optimization.py").read())
#
# -4- In case of need to reload the reversi code, reload all modules.
#
# >>> exec(open("py/reversi/start.py").read())
#

#
# TODO - optimization.py
#
# - Rewrite the class keeping just CG with strong Wolfe linear search.
# - Move to a single call for FUN & GRAD.
# - Test other CG beta formulas.
#   The beta parameter in conjugate gradient (CG) algorithms dictates how the new search direction is formed.
#   Different formulas for calculating beta lead to distinct CG methods, each with its own properties
#   and performance characteristics. Instead of the common Fletcher-Reeves, Polak-Ribière, and Hestenes-Stiefel methods,
#   alternatives like Dai-Yuan, Liu-Storey, and modified versions of existing methods offer potential improvements
#   in convergence and robustness.
#
#   Common CG Methods and their Beta Formulas:
#
#    * Fletcher-Reeves (FR): βk = ||g_{k+1}||² / ||g_k||². 
#    * Polak-Ribière-Polyak (PRP): βk = g_{k+1}^T y_k / ||g_k||². 
#    * Hestenes-Stiefel (HS): βk = g_{k+1}^T y_k / d_k^T y_k. 
#
#   Other Beta Formulas:
#
#    * Dai-Yuan (DY): βk = ||g_{k+1}||² / d_k^T y_k. 
#    * Liu-Storey (LS): βk = g_{k+1}^T y_k / -d_k^T g_k.
#
# - Compare with L-BFGS.
# - Revrite it using pytorch tensors.
#

#
# General Nonlinear Conjugate Gradient
#
# Evaluate:
#    g_0 = grad(x_0)
#
# Initialize:
#    d_0 = - g_0
#    k = 1
#
# while: g_(k-1) != 0; do
#
#    compute: alpha_(k-1);
#    x_(k) = x_(k-1) + alpha_(k-1) * d_(k-1);
#    evaluate g_(k) = grad(x_(k));
#    compute beta_(k);
#    d_(k) = - g_(k) + beta_(k) * d_(k-1);
#    k += 1
#
# end while;
# 
# return x_(k-1);
#

# Golden ratio.
golden_ratio = 1.618033988749

# Inverse of golden ratio.
inverse_golden_ratio = 0.618033988749

class CurrentPrevious:
    def __init__(self):
        self.current = None
        self.previous = None

    def set(self, current, previous) -> None:
        self.current = current
        self.previous = previous

    def push(self, new) -> None:
        self.previous = self.current
        self.current = new

    def print(self) -> None:
        print("current={}, previous{}".format(self.current, self.previous))

class GNCG:
    def __init__(self,
                 x0: numpy.ndarray,
                 fg: Callable,
                 c1: float =1e-2,
                 c2: float =0.3,
                 alpha_min: float =1.e-3,
                 min_grad: tuple =(1.e-6, 7),
                 min_p_fun_decrease: tuple =(1.e-14, 7),
                 max_iters: int =100,
                 beta_algo: str ="FR",
                 verbosity: int =0):
        
        if not isinstance(x0, np.ndarray):
            raise TypeError('Argument x0 is not an instance of numpy.ndarray')
        if not x0.dtype == 'float64':
            raise TypeError('Argument x0 must be an array having dtype equal to float64')
        if not callable(fg):
            raise TypeError('Argument fun is not callable')

        self.x0 = x0
        self.fg = fg
        self.c1 = c1
        self.c2 = c2
        self.alpha_min = alpha_min
        self.min_grad = min_grad
        self.min_p_fun_decrease = min_p_fun_decrease
        self.max_iters = max_iters
        self.verbosity = verbosity
        self.beta_algo = beta_algo
        
        self.i = 0
        self.n = len(x0)
        self.x = deepcopy(x0)

        self.function_call_count = 0

    def fgc(self, x: numpy.ndarray) -> (float, numpy.ndarray):
        self.function_call_count += 1
        return self.fg(x)
    
    def print(self) -> None:
        print("General Nonlinear Conjugate Gradient (GNCG) object: {}".format(self))
        print("x0: {}".format(self.x0))
        print("function/gradient: {}".format(self.fg))
        print("c1={}, c2={}, min_grad={}, max_iters={}, verbosity={}".format(self.c1, self.c2, self.min_grad, self.max_iters, self.verbosity))
        print("iterations={}, function_call_count={}".format(self.i, self.function_call_count))
        print("x: {}".format(self.x))

    def minimize(self) -> numpy.ndarray:

        def evaluate(x, f, g):
            _f, _g = self.fgc(x)
            f.push(_f)
            g.push(_g)
            return

        def beta_fr(d, g, gg) -> float:
            """
            Fletcher-Reeves (FR)
            """
            beta = gg.current / gg.previous
            return beta

        def beta_pr(d, g, gg) -> float:
            """
            Polak-Ribiere (PR)
            """
            y = g.current - g.previous
            beta = np.dot(g.current, y) / gg.previous
            return beta

        def beta_hs(d, g, gg) -> float:
            """
            Hestenes-Steifel(HS)
            """
            eps = 1.e-18
            y = g.current - g.previous
            num = np.dot(g.current, y)
            if num == 0.:
                return 0.
            div = np.dot(d.current, y)
            if div <= eps:
                return 0.
            beta = num / div
            return beta

        def beta_dy(d, g, gg) -> float:
            """
            Dai-Yuan (DY)
            """
            eps = 1.e-18
            y = g.current - g.previous
            div = np.dot(d.current, y)
            if div <= eps:
                return 0.
            beta = gg.current / div
            return beta

        def beta_hz(d, g, gg) -> float:
            """
            Hager-Zhang (hz)
            """
            eps = 1.e-18
            y = g.current - g.previous
            beta_d = np.dot(d.current, y)
            if beta_d <= eps:
                return 0.
            beta_c = np.dot(y, y) / beta_d
            beta_a =  y - 2. * d.current * beta_c
            beta_n = np.dot(beta_a, g.current)
            beta = beta_n / beta_d
            if beta_n == 0.:
                return 0.
            return beta
        
        x = self.x
        fg = self.fgc
        verbosity = self.verbosity
        max_iters = self.max_iters
        min_grad = self.min_grad
        min_grad_v, min_grad_c = self.min_grad
        min_p_fun_decrease_v, min_p_fun_decrease_c = self.min_p_fun_decrease

        compute_beta_algos = {
            "FR": beta_fr,
            "PR": beta_pr,
            "HS": beta_hs,
            "DY": beta_dy,
            "HZ": beta_hz,                              }
        
        compute_beta = compute_beta_algos.get(self.beta_algo)
        if not compute_beta:
            raise RuntimeError('beta_algo is not found. Choose among: {}'.format(compute_beta_algos.keys()))


        low_progres_count_f = 0
        low_progres_count_g = 0

        line_search = lambda x, d, initial_alpha, i : gncg_strong_wolfe(x, fg, d, alpha=initial_alpha,
                                                                        c1=self.c1, c2=self.c2, alpha_min=self.alpha_min,
                                                                        verbosity=self.verbosity, iteration=i)
        
        # Initialization
        g = CurrentPrevious()  # gradient : array
        f = CurrentPrevious()  # function : scalar
        gg = CurrentPrevious() # gradient * gradient dot product : scalar
        d = CurrentPrevious()  # direction : array
        gd = CurrentPrevious() # gradient * direction dot product : scalar
        y = CurrentPrevious()  # y.previus = g.current - g.previous
        
        # Iteration 0
        alpha = 1.0
        _f, _g = fg(x)
        g.set(_g, _g)
        f.set(_f, _f)
        _gg = np.dot(_g, _g)
        gg.set(_gg, _gg)
        d.set(- _g, - _g)
        gd.set(-1., -1.)

        for i in range(1, max_iters):
            if gg.current == 0.:
                break
            
            if gg.current < min_grad_v:
                low_progres_count_g += 1
            else:
                low_progres_count_g = 0
                
            if low_progres_count_g >= min_grad_c:
                break

            p_diff = 0.
            if f.current != 0.:
                if f.current < f.previous:
                    diff = f.previous - f.current
                    p_diff = diff / np.fabs(f.current)
                    if p_diff < min_p_fun_decrease_v:
                        low_progres_count_f += 1
                    else:
                        low_progres_count_f = 0
                else:
                    low_progres_count_f += 1
                
            if low_progres_count_f >= min_p_fun_decrease_c:
                break
            
            #
            # Jorge Nocedal - Stephen J. Wright
            # Numerical Optimization - Second Edition
            #
            # - INITIAL STEP LENGTH
            #   Page 59
            # - Formula 3.60
            #
            gd.push(np.dot(g.current, d.current))
            initial_alpha = alpha * gd.previous / gd.current
            if verbosity > 1:
                print("minimize: alpha={:.3e}, initial_alpha={:.3e}, gd.previous={:.3e}, gd.current={:.3e}".format(alpha, initial_alpha, gd.previous, gd.current))
            if initial_alpha < self.alpha_min or initial_alpha > 1.: initial_alpha = 1.
            alpha = line_search(x, d.current, initial_alpha, i)
            x += alpha * d.current
            evaluate(x, f, g)
            gg.push(np.dot(g.current, g.current))
            beta = compute_beta(d, g, gg)
            tentative_d = - g.current + beta * d.current
            d.push(tentative_d)

            if verbosity > 0:
                print("minimize iter: [{:n}/{:n}], ".format(i, max_iters), end='')
                print("f: {:.5e}, ".format(f.current), end='')
                print("a: {:.3e}, ".format(alpha), end='')
                print("b: {:.3e}, ".format(beta), end='')
                print("gg: {:.3e}, ".format(gg.current), end='')
                print("p_diff_prev: {:.3e}, low_pcf: {}, low_pcg: {}, ".format(p_diff, low_progres_count_f, low_progres_count_g), end='')
                print("x: {}".format(x))

        self.i += i
        return x

#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - A LINE SEARCH ALGORITHM FOR THE WOLFE CONDITIONS
#   Page 60
# - Algorithm 3.5 (Line Search Algorithm)
#
def gncg_strong_wolfe(x: numpy.ndarray,
                      fg: Callable,
                      p: numpy.ndarray,
                      c1:float =1e-3,
                      c2: float =0.49,
                      alpha: float =1.0,
                      alpha_min: float =1.e-3,
                      alpha_max: float =2.0,
                      max_iters: int =23,
                      verbosity: int =0,
                      iteration: int =0) -> float:
    """
    Returns the alpha step length satisfying the strong Wolfe conditions.
    
    The strong Wolfe conditions are among the most widely applicable and useful termination conditions.
    Parameters c1 and c2 must satisfy the condition: 0 < c1 < c2 < 1
    We assume that pk is a descent direction and that fun is bounded below along the direction pk.

    Arguments alpha and alpha_max must satisfy the conditions: 0 < alpha < alpha_max

    Arguments:
      x:         independent variable array
      fg:        function f(x), grad(x) to be minimized
      p:         search direction
      c1:        sufficient decrease parameter
      c2:        curvature condition parameter
      alpha:     initial estimate for the step length
      alpha_min: minimum value of alpha returned
      alpha_max: maximum value of alpha returned
      max_iters: maximum number of iterations
      verbosity: verbosity level
      iteration: used for logging and debugging
    """

    # Returned value.
    alpha_star = 0.

    # Returns a tuple having : (alpha, phi(alpha), phi'(alpha))
    def xyy1(alpha: float) -> (float, float, float):
        f, g = fg(x + alpha * p)
        return alpha, f, np.dot(g, p)

    ###### for debugging: It show the phi function. Arrange limits and step ...
    if False:
        verbosity=4
        print("gncg_strong_wolfe: initial alpha value = {}".format(alpha))
        import matplotlib.pyplot as plt
        t = np.arange(0., 0.00002, 0.0000005)
        plt.plot(t, [xyy1(y)[1] for y in t], 'bs')
        plt.show()
    ######

    # tuples are in the form:
    #
    # ( alpha, phi(alpha), phi'(alpha) )
    #
    # v0   : values at alpha=0
    # vi   : values at iteration i
    # vim1 : values at iteration i - 1
    # lo   : calling zoom, the low values
    # hi   : calling zoom, the high values
    
    # alpha_im1 is the alpha value at alpha(i-1)

    v0 = xyy1(0.)
    vim1 = v0
    vi = xyy1(alpha)

    zoom = lambda lo, hi : gncg_zoom_hat(xyy1, v0, lo, hi, c1, c2, verbosity=verbosity)
    
    if verbosity > 1:
        print("gncg_strong_wolfe: header. c1={}, c2={}, alpha={}".format(c1, c2, alpha))
        print("gncg_strong_wolfe: v0=({:.3e}, {:.3e}, {:.3e}), vim1=({:.3e}, {:.3e}, {:.3e}), vi=({:.3e}, {:.3e}, {:.3e})".format(v0[0], v0[1], v0[2], vim1[0], vim1[1], vim1[2], vi[0], vi[1], vi[2]))

    for i in range(max_iters):
        if verbosity > 2:
            print("gncg_strong_wolfe, iter: {:d}, vim1=({:.3e}, {:.3e}, {:.3e}), vi=({:.3e}, {:.3e}, {:.3e})".format(i, vim1[0], vim1[1], vim1[2], vi[0], vi[1], vi[2]))
            
        if vi[1] > v0[1] + c1 * vi[0] * v0[2] or (i > 0 and vi[1] >= vim1[1]):
            if (vi[1] > vim1[1]):
                alpha_star = zoom(vim1, vi)
            else:
                alpha_star = zoom(vi, vim1)
            break
        if np.fabs(vi[2]) <= - c2 * v0[2]:
            alpha_star = vi[0]
            break
        if vi[2] >= 0.:
            alpha_star = zoom(vi, vim1)
            break
        
        vim1 = vi
        
        alpha_i = golden_ratio * vi[0]
        if alpha_i >= alpha_max:
            # Line search failed. Returning alpha_max.
            if verbosity > 1:
                print("gncg_strong_wolfe: alpha_i is grown beyond alpha_max. alpha_i={}, alpha_max={}. Returning alpha_max."
                      .format(alpha_i, alpha_max))
            alpha_star = alpha_max
            break
        vi = xyy1(alpha_i)

    
    if alpha_star == 0.:
        alpha_star = alpha_min
        
    if verbosity > 1:
        print("gncg_strong_wolfe: iterations={}, returning alpha_star: {}".format(i + 1, alpha_star))
    return alpha_star

#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - A LINE SEARCH ALGORITHM FOR THE WOLFE CONDITIONS
#   Page 61
# - Algorithm 3.6 (zoom)
#
def gncg_zoom_hat(xyy1: Callable,
                  v0: tuple,
                  lo: tuple,
                  hi: tuple,
                  c1: float,
                  c2: float,
                  max_iters: int =30,
                  verbosity: int =0) -> float:
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
      phi:       callable function returning alpha, phi(alpha), phi'(alpha)
      v0:        a tuple having: alpha=0., phi(0.), phi'(0.)
      lo:        a tuple having: alpha, phi(alpha), phi'(alpha), where alpha is the bound of the interval giving the lesser value of phi
      hi:        a tuple having: alpha, phi(alpha), phi'(alpha), where alpha is the other bound of the interval
      c1:        sufficient decrease parameter
      c2:        curvature condition parameter
      max_iters: maximum number of iterations
      verbosity: vebosity level
    """
    
    if (lo[1] > hi[1]):
        print("gncg_zoom: v0=({:.3e}, {:.3e}, {:.3e}), lo=({:.3e}, {:.3e}, {:.3e}), hi=({:.3e}, {:.3e}, {:.3e})".format(*v0, *lo, *hi))
        print("gncg_zoom: c1={}, c2={}, max_iters={}".format(c1, c2, max_iters))
        raise ValueError("zoom: lo[1] must be lesser than hi[1].")
    
    for j in range(max_iters):
        if verbosity > 2:
            print("gncg_zoom: j={:02d}, lo=({:.3e}, {:.3e}, {:.3e}), hi=({:.3e}, {:.3e}, {:.3e})".format(j, *lo, *hi))
        alpha_j = interpolate(lo, hi)
        vj = xyy1(alpha_j)
        if vj[1] > v0[1] + c1 * vj[0] * v0[2] or vj[1] >= lo[1]:
            hi = vj
        else:
            if np.fabs(vj[2]) <= - c2 * v0[2]:
                return vj[0]
            if vj[2] * (hi[0] - lo[0]) >= 0.:
                hi = lo
            lo = vj
    
    # It happens only if the algorithm is exceeding the max iterations threshold.
    # The textbook doesn't cover this occurrence.
    # Here we take a conservative approach by returning the lower bound for alpha.
    return lo[0]

# -- -- --

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
    c1=0.001, c2=0.49, min_grad=(1e-06, 5), max_iters=100, verbosity=0
    algorithm: conjugate_gradient
    line search method: strong_wolfe
    iterations=3, function_call_count=6, gradient_call_count=8
    x: [-1.   1.5]

    Attributes:
    ----------
      x0:                  the starting position vector
      fun:                 function f(x) to be minimized, also named loss
      grad_fun:            the gradient of f(x) 
      c1:                  sufficient decrease parameter
      c2:                  curvature condition parameter
      min_grad:            stopping tolerance such that ||grad f(x)|| < min_grad_v
      min_p_fun_decrease:  minimum percentage acceptable loss decrease between two steps
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
                 min_grad: tuple =(1.e-6, 5),
                 min_p_fun_decrease: tuple =(1.e-14, 5),
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
        self.min_grad = min_grad
        self.min_p_fun_decrease = min_p_fun_decrease
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
        print("c1={}, c2={}, min_grad={}, max_iters={}, verbosity={}".format(self.c1, self.c2, self.min_grad, self.max_iters, self.verbosity))
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
        min_grad_v, min_grad_c = self.min_grad
        max_iters = self.max_iters
        verbosity = self.verbosity
        
        for i in range(max_iters):
        
            grad = grad_fun(x)
            grad_norm = np.sqrt(np.dot(grad, grad))
            if grad_norm < min_grad_v:
                break
            p = - grad
            alpha = line_search(x, fun, grad_fun, p, c1=c1, c2=c2, verbosity=verbosity)
            x += alpha * p
            
            if self.verbosity > 0:
                fun_value = fun(x)
                print("sd iter: [{:n}/{:n}], ".format(i, max_iters - 1), end='')
                print("fun value: {:.6f}, ".format(fun_value), end='')
                print("alpha: {:.8f}, ".format(alpha), end='')
                print("grad_norm: {:.6f}, min_grad={:.6f}, ".format(grad_norm, min_grad), end='')
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

        low_progres_count_d = 0
        low_progres_count_g = 0

        x = self.x
        fun = self.func
        grad_fun = self.grad_func
        line_search = self.line_search
        c1 = self.c1
        c2 = self.c2
        min_grad = self.min_grad
        min_grad_v, min_grad_c = self.min_grad
        min_p_fun_decrease_v, min_p_fun_decrease_c = self.min_p_fun_decrease
        max_iters = self.max_iters
        verbosity = self.verbosity
    
        p_prev = np.zeros(len(x))

        # PRP
        grad_prev = np.zeros(len(x))

        # Iteration 0
        i = 0
        alpha=1.0
        fun_value = fun(x)
        grad = grad_fun(x)
        grad_grad = np.dot(grad, grad)
        p = - grad
        grad_p = np.dot(grad, p)
        fun_value_prev = fun_value * 1.20
        p_diff = None

        for i in range(1, max_iters):
            grad_norm = np.sqrt(grad_grad)
            if grad_norm == 0.:
                break
            
            if grad_norm < min_grad_v:
                low_progres_count_g += 1
            else:
                low_progres_count_g = 0

            if low_progres_count_g >= min_grad_c:
                break

            if fun_value != 0.:
                if fun_value < fun_value_prev:
                    diff = fun_value_prev - fun_value
                    p_diff = diff / np.fabs(fun_value)
                    if p_diff < min_p_fun_decrease_v:
                        low_progres_count_d += 1
                    else:
                        low_progres_count_d = 0
                else:
                    low_progres_count_d += 1
            if low_progres_count_d >= min_p_fun_decrease_c:
                break

            # PRP
            grad_prev[:] = grad[:]
            
            grad_grad_prev = grad_grad
            p_prev[:] = p[:]
            grad_p_prev = grad_p
            alpha_p = alpha

            #
            # Jorge Nocedal - Stephen J. Wright
            # Numerical Optimization - Second Edition
            #
            # - INITIAL STEP LENGTH
            #   Page 59
            # - Formula 3.60
            #
            grad_p = np.dot(grad, p)
            initial_alpha = alpha_p * grad_p_prev / grad_p
            if verbosity > 2:
                print("_cg: initial_alpha={}, alpha_p={}, grad_p_prev={}, grad_p{}".format(initial_alpha, alpha_p, grad_p_prev, grad_p))
            alpha = line_search(x, fun, grad_fun, p, alpha=initial_alpha, c1=c1, c2=c2, verbosity=verbosity, iteration=i)
            x += alpha * p

            fun_value_prev = fun_value
            fun_value = fun(x)
            grad = grad_fun(x)
            grad_grad = np.dot(grad, grad)

            # Fletcher-Reeves (FR) beta formula.
            beta = grad_grad / grad_grad_prev
            
            # Polak-Ribiere-Polyak (PRP) beta formula.
            #beta_prp = np.dot(grad, grad - grad_prev) / grad_grad
            
            # PRP
            #print("beta: FR={}, PRP={}".format(beta, beta_prp))

            # [711/3000], F: 907.1538947882
            p = - grad + beta * p_prev
            #p = - grad + beta_prp * p_prev

            # This is an improvement over base FR algorithm.
            # When the direction p is not a descent direction then beta is set to 0.
            # in this way the algorithm falls back to a steepest descent for this
            # iteration.
            if np.dot(grad, p) >= 0.0:
                p = - grad
            
            if verbosity > 0:
                print("cg iter: [{:n}/{:n}], ".format(i, max_iters), end='')
                print("F: {:.10f}, ".format(fun_value), end='')
                print("a: {:.8f}, ".format(alpha), end='')
                print("grad_norm: {:.6f}, min_grad={}, ".format(grad_norm, min_grad), end='')
                print("p_diff={}, low_progres_count_d: {}, ".format(p_diff, low_progres_count_d), end='')
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
                 alpha_min: float =0.001,
                 alpha_max: float =2.0,
                 max_iters: int =12,
                 verbosity: int =0,
                 iteration: int =0) -> float:
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
      alpha_min: minimum value of alpha returned
      alpha_max: maximum value of alpha returned
      max_iters: maximum number of iterations
      verbosity: verbosity level
      iteration: used for logging and debugging
    """

    # Returned value.
    alpha_star = 0.

    # Collable function phi(alpha)
    phi = lambda alpha : fun(x + alpha * p)
    # Callable function phi'(alpha)
    phi_d1 = lambda alpha : np.dot(grad_fun(x + alpha * p), p)

    ###### for debugging: It show the phi function. Arrange limits and step ...
    #if False:
    #    verbosity=4
    #    print("strong_wolfe: initial alpha value = {}".format(alpha))
    #    import matplotlib.pyplot as plt
    #    t = np.arange(0., 0.0002, 0.00001)
    #    plt.plot(t, [phi(y) for y in t], 'bs')
    #    plt.show()
    ######

    # tuples are in the form:
    #
    # ( alpha, phi(alpha), phi'(alpha) )
    #
    # v0   : values at alpha=0
    # vi   : values at iteration i
    # vim1 : values at iteration i - 1
    # lo   : calling zoom, the low values
    # hi   : calling zoom, the high values
    
    # alpha_im1 is the alpha value at alpha(i-1)

    v0 = 0., phi(0.), phi_d1(0.)
    vim1 = v0
    vi = alpha, phi(alpha), phi_d1(alpha)

    zoom = lambda lo, hi : zoom_hat(phi, phi_d1, v0, lo, hi, c1, c2, verbosity=verbosity)
    
    if verbosity > 1:
        print("strong_wolfe: header. c1, c2: {}, {}".format(c1, c2))
        print("strong_wolfe: v0={}, vim1={}, vi={}".format(v0, vim1, vi))

    for i in range(max_iters):
        if verbosity > 2:
            print("strong_wolfe, iter: {:d}, vim1={}, vi={}".format(i, vim1, vi))
            
        if vi[1] > v0[1] + c1 * vi[0] * v0[2] or (i > 0 and vi[1] >= vim1[1]):
            if (vi[1] > vim1[1]):
                alpha_star = zoom(vim1, vi)
            else:
                alpha_star = zoom(vi, vim1)
            break
        if np.fabs(vi[2]) <= - c2 * v0[2]:
            alpha_star = vi[0]
            break
        if vi[2] >= 0.:
            alpha_star = zoom(vi, vim1)
            break
        
        vim1 = vi
        
        alpha_i = golden_ratio * vi[0]
        if alpha_i >= alpha_max:
            # Line search failed. Returning alpha_max.
            if verbosity > 1:
                print("strong_wolfe: alpha_i is grown beyond alpha_max. alpha_i={}, alpha_max={}. Returning alpha_max."
                      .format(alpha_i, alpha_max))
            alpha_star = alpha_max
            break
        vi = alpha_i, phi(alpha_i), phi_d1(alpha_i)

    
    if alpha_star == 0.:
        alpha_star = alpha_min
        
    if verbosity > 1:
        print("strong_wolfe: iterations={}, returning alpha_star: {}".format(i + 1, alpha_star))
    return alpha_star

#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - A LINE SEARCH ALGORITHM FOR THE WOLFE CONDITIONS
#   Page 61
# - Algorithm 3.6 (zoom)
#
def zoom_hat(phi: Callable,
             phi_d1: Callable,
             v0: tuple,
             lo: tuple,
             hi: tuple,
             c1: float,
             c2: float,
             max_iters: int =20,
             verbosity: int =0) -> float:
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
      v0:        a tuple having: alpha=0., phi(0.), phi'(0.)
      lo:        a tuple having: alpha, phi(alpha), phi'(alpha), where alpha is the bound of the interval giving the lesser value of phi
      hi:        a tuple having: alpha, phi(alpha), phi'(alpha), where alpha is the other bound of the interval
      c1:        sufficient decrease parameter
      c2:        curvature condition parameter
      max_iters: maximum number of iterations
      verbosity: vebosity level
    """
    
    if (lo[1] > hi[1]):
        print("zoom: v0={}, lo={}, hi={}".format(v0, lo, hi))
        print("zoom: c1={}, c2={}, max_iters={}".format(c1, c2, max_iters))
        raise ValueError("zoom: lo[1] must be lesser than hi[1].")
    
    for j in range(max_iters):
        if verbosity > 2:
            print("zoom: j={}, lo={}, hi={}".format(j, lo, hi))
        alpha_j = interpolate(lo, hi)
        vj = alpha_j, phi(alpha_j), phi_d1(alpha_j)
        if vj[1] > v0[1] + c1 * vj[0] * v0[2] or vj[1] >= lo[1]:
            hi = vj
        else:
            if np.fabs(vj[2]) <= - c2 * v0[2]:
                return vj[0]
            if vj[2] * (hi[0] - lo[0]) >= 0.:
                hi = lo
            lo = vj
    
    # It happens only if the algorithm is exceeding the max iterations threshold.
    # The textbook doesn't cover this occurrence.
    # Here we take a conservative approach by returning the lower bound for alpha.
    return lo[0]

#
# Jorge Nocedal - Stephen J. Wright
# Numerical Optimization - Second Edition
#
# - CUBIC INTERPOLATION
#   Page 59
# - Formula 3.59
#
def interpolate(a: tuple,
                b: tuple,
                min_delta_x: float=1.e-12) -> float:
    """
    Returns the x coordinate of the estimated minimum of f(x) in the range [xa,xb].
    Cubic interpolation of function f(x) is adopted if possible, othervise bisection
    is adopted.
        
    Arguments:
      a: a tuple in the form (x, y, y') for the a bound
      b: a similar tuple for the b bound
    """
    xa, ya, dya = a
    xb, yb, dyb = b
    xm = 0.5 * ( xa + xb )
    if xa - xb < min_delta_x:
        return xm
    d1 = dya + dyb - 3. * ( ya - yb ) / ( xa - xb )
    d2a = d1**2 - dya * dyb
    if d2a < 0.:
        return xm
    d2 = np.sign( xb - xa ) * np.sqrt( d2a )
    x = xb - ( xb - xa ) * ( ( dyb + d2 - d1 ) / ( dyb - dya + 2. * d2 ) )
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
              ro: float =inverse_golden_ratio,
              alpha: float =1.0,
              c1: float =1e-3,
              c2: float = None,
              max_iters: int =30,
              alpha_min: float =0.0001,
              verbosity: int=0,
              iteration: int =0):
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
        if alpha < alpha_min:
            break
        
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
