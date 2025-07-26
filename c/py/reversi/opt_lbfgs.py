#
# opt_lbfgs.py
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

import math
import numpy as np
from collections import namedtuple
import matplotlib.pyplot as plt

# FG : Function , Gradient named tuple
FG = namedtuple('FG', ['fun', 'grad'])

__all__ = ['lbfgs']

def  _hermite_cubic_interp_min(x0: float,
                               y0: float,
                               dy0: float,
                               x1: float,
                               y1: float,
                               dy1: float) -> tuple[float, float, float, float]:
    dx = x1 - x0
    dy = y1 - y0
    
    a = - 6 * dy + 3 * dx * (     dy0 + dy1 )
    b =   6 * dy - 2 * dx * ( 2 * dy0 + dy1 )
    c =                dx *       dy0

    if a == 0.:
        return None

    discriminant = b * b - 4 * a * c
    if discriminant <= 0. :
        return None
    sd = math.sqrt(discriminant)

    t_stationary_0 = ( - b + sd ) / ( 2 * a )
    t_stationary_1 = ( - b - sd ) / ( 2 * a )

    x_stationary_0 = x0 + t_stationary_0 * dx
    x_stationary_1 = x0 + t_stationary_1 * dx

    stationary_point_0 = _hermite_cubic_interp(x0, y0, dy0, x1, y1, dy1, x_stationary_0)
    stationary_point_1 = _hermite_cubic_interp(x0, y0, dy0, x1, y1, dy1, x_stationary_1)

    if stationary_point_0[3] > 0.:
        minimum_point = stationary_point_0
    else:
        minimum_point = stationary_point_1
    
    return minimum_point

def  _hermite_cubic_interp(x0: float,
                           y0: float,
                           dy0: float,
                           x1: float,
                           y1: float,
                           dy1: float,
                           x: float) -> tuple[float, float, float, float]:
    dx = x1 - x0
    dy = y1 - y0
    t = (x - x0) / dx
    t2 = t * t
    t3 = t2 * t
    #          t3  .  . . t2  .  . . t1  .  . . t0 
    h0  =  2 * t3  -  3 * t2             +      1
    h1  = -2 * t3  +  3 * t2
    h2  =      t3  -  2 * t2  +      t
    h3  =      t3  -      t2
    #.
    h01 =             6 * t2  -  6 * t
    h11 =          -  6 * t2  +  6 * t
    h21 =             3 * t2  -  4 * t   +      1
    h31 =             3 * t2  -  2 * t
    #.
    h02 =                       12 * t   -      6
    h12 =                     - 12 * t   +      6
    h22 =                        6 * t   -      4
    h32 =                        6 * t   -      2

    h   =   y0 * h0  + y1 * h1  + dy0 * dx * h2  + dy1 * dx * h3
    dh  = ( y0 * h01 + y1 * h11 + dy0 * dx * h21 + dy1 * dx * h31 ) / dx
    d2h = ( y0 * h02 + y1 * h12 + dy0 * dx * h22 + dy1 * dx * h32 ) / (dx * dx)
    
    return x, h, dh, d2h

def _display_line_search(phi: Callable[[float], [float, float]],
                         c1: float,
                         c2: float,
                         phi0: float,
                         phi_a1: float,
                         derphi0: float,
                         derphi_a1: float,
                         window: float =3.,
                         dots: int =30,
                         alpha0: float =0.,
                         alpha1: float =1.,
                         title: str='Function phi(alpha)') -> None:
    t0 = alpha0
    t1 = window * alpha1
    step = (t1 - t0) / dots
    t = np.arange(t0, t1, step)
    fgvl = [phi(a) for a in t]
    f = [x[0] for x in fgvl]
    g = [x[1] for x in fgvl]
    abs_g = [abs(x) for x in g]
    curv_value = c2 * abs(derphi0)
    curvature_cond = [x <= curv_value for x in abs_g]
    min_f = min(f)
    min_i = f.index(min_f)
    min_a = t[min_i]
    max_f = max(f)
    max_i = f.index(max_f)
    armijo_line = phi0 + c1 * t * derphi0
    interpolation_results = _hermite_cubic_interp_min(alpha0, phi0, derphi0, alpha1, phi_a1, derphi_a1)
    if interpolation_results is None:
        i_min_x, i_min_y, i_min_dy, i_min_d2y = None, None, None, None
    else:
        i_min_x, i_min_y, i_min_dy, i_min_d2y = interpolation_results
    plt.title(title)
    plt.xlabel("alpha")
    plt.ylabel("phi")
    plt.plot(t, f, linestyle='-', color='blue')
    plt.axvline(x=alpha1, color='grey', linewidth=1)
    if i_min_x is not None:
        real_value = phi(i_min_x)
        plt.axvline(x=i_min_x, color='red', linewidth=1)
        plt.plot(i_min_x, i_min_y, marker='o', color='red', markersize=7)
        plt.plot(i_min_x, real_value[0], marker='o', color='green', markersize=7)
    plt.plot(t, armijo_line, '--', label='Armijo (c1) threshold', color='green', linewidth=1)
    plt.plot(alpha0, phi0, marker='o', color='black', markersize=6)
    plt.plot(alpha1, phi_a1, marker='o', color='black', markersize=6)
    plt.annotate('{:.3e}'.format(phi_a1), (alpha1, phi_a1), textcoords="offset points",
                 arrowprops=dict(arrowstyle='->', color='black', linewidth=1),
                 bbox=dict(boxstyle="round,pad=0.2", fc="white", ec="none"),
                 xytext=(26, 14), ha='left', color='black', fontfamily='monospace', fontsize=9)
    plt.plot(t[min_i], min_f, marker='o', color='black', markersize=6)
    for u in range(len(t)):
        if curvature_cond[u]:
            plt.plot(t[u], f[u], marker='o', color='green', markersize=4)
        delta_y = - (phi_a1 - phi0)
        if i_min_x is not None:
            extra_y = - (i_min_y - phi_a1)
            with np.errstate(divide='ignore', invalid='ignore'):
                gain = extra_y / delta_y
            multi_line_label = (
                "phi_a0  = {:.3e}\n"
                "phi_a1  = {:.3e}\n"
                "phi_min = {:.3e}\n"
                "a_opt   = {:.3e}\n"
                "interp=({:.3e}, {:.3e})\n"
                "delta_y = {:.3e}\n"
                "extra_y = {:.3e}\n"
                "gain    = {:.3e}\n"
            )
            label = multi_line_label.format(phi0, phi_a1, min_f, min_a, i_min_x, i_min_y, delta_y, extra_y, gain)
        else:
            multi_line_label = (
                "phi_a0  = {:.3e}\n"
                "phi_a1  = {:.3e}\n"
                "phi_min = {:.3e}\n"
                "a_opt   = {:.3e}\n"
            )
            label = multi_line_label.format(phi0, phi_a1, min_f, min_a)
        plt.text((t1 - t0) / 2, max_f, label,
                 fontfamily='monospace', fontsize=9, ha='left', va='top',
                 bbox=dict(boxstyle="round,pad=0.2", fc="white", ec="none"))
    
    plt.show()
    return

def _two_loop_recursion(g: np.ndarray,
                        s: list,
                        y: list,
                        rho: list) -> np.ndarray:
    """
    Perform the L-BFGS two-loop recursion to compute H_k * q efficiently.

    Parameters:
    - g: numpy array, gradient vector
    - s: list of numpy arrays, stored s vectors (x_{k+1} - x_k)
    - y: list of numpy arrays, stored y vectors (grad_{k+1} - grad_k)
    - rho: list of scalars precomputed as 1 / (y_i^T s_i)

    Returns:
    - r: numpy array, the product H_k * q (negated search direction)
    """
    m = len(s)
    alpha = np.zeros(m)

    delta = g.copy()
    for i in reversed(range(m)):
        alpha[i] = rho[i] * np.dot(s[i], delta)
        delta -= alpha[i] * y[i]

    if m > 0:
        last_s = s[-1]
        last_y = y[-1]
        gamma = np.dot(last_s, last_y) / np.dot(last_y, last_y)
    else:
        gamma = 1.0

    r = gamma * delta

    for i in range(m):
        beta = rho[i] * np.dot(y[i], r)
        r += s[i] * (alpha[i] - beta)

    return r

def _strong_wolfe_line_search(fg: Callable[[np.ndarray], FG],
                              x: np.ndarray,
                              p: np.ndarray,
                              fgv0: FG,
                              c1: float =1e-4,
                              c2: float =0.9,
                              alpha_max: float =10.0,
                              max_iters: int =3) -> (float, FG):
    """
    Strong Wolfe line search implementation with full gradient caching.
    
    Parameters:
    - fg: function returning (f, g)
    - x: current point
    - p: search direction
    - fgv0: tuple (f, g) at x
    - c1, c2: Wolfe condition constants (0 < c1 < c2 < 1)
    - alpha_max: maximum step length
    - max_iters: maximum number of iterations

    Returns:
    - alpha: step length satisfying strong Wolfe conditions
    - fg_val: FG namedtuple at new point
    """

    # Use this flag for debugging purposes.
    DISPLAY_PHI_FUN = False
    
    cache = {}

    def phi(alpha):
        if alpha not in cache:
            f, grad = fg(x + alpha * p)
            cache[alpha] = FG(f, grad)
        fg_val = cache[alpha]
        return fg_val.fun, np.dot(fg_val.grad, p)

    alpha0 = 0.0
    phi0, derphi0 = fgv0.fun, np.dot(fgv0.grad, p)
    cache[alpha0] = fgv0

    alpha1 = 1.0
    phi_a1, derphi_a1 = phi(alpha1)

    i = 0

    while True:
        if DISPLAY_PHI_FUN:
            disp_title = "Function phi(alpha), iter #{}".format(i)
            _display_line_search(phi, c1, c2, phi0, phi_a1, derphi0, derphi_a1,
                                 window=(alpha1*3), alpha1=alpha1,
                                 title=disp_title)
        if (phi_a1 > phi0 + c1 * alpha1 * derphi0) or (i > 0 and phi_a1 >= phi_prev):
            return _zoom(phi, alpha0, alpha1, phi0, derphi0, c1, c2, cache)
        if abs(derphi_a1) <= -c2 * derphi0:
            return alpha1, cache[alpha1]
        if derphi_a1 >= 0:
            return _zoom(phi, alpha1, alpha0, phi0, derphi0, c1, c2, cache)
        i += 1
        if i >= max_iters:
            return alpha1, cache[alpha1]
        alpha_prev = alpha1
        phi_prev = phi_a1
        #alpha1 = min(alpha1 * 2, alpha_max)
        interpolated_min = _hermite_cubic_interp_min(alpha0, phi0, derphi0, alpha1, phi_a1, derphi_a1)
        i_alpha, i_phi, i_derphi, i_der2phi = interpolated_min
        alpha1 = min(i_alpha, alpha_max)
        phi_a1, derphi_a1 = phi(alpha1)

def _zoom(phi: Callable[[float], (float, float)],
          alo: float,
          ahi: float,
          phi0: float,
          derphi0: float,
          c1: float,
          c2: float,
          cache: dict[float, FG]) -> (float, FG):
    """
    Zoom phase of strong Wolfe line search.

    Parameters:
    - phi: function returning (f, directional_derivative) at alpha
    - alo, ahi: bracketing interval for alpha
    - phi0: function value at alpha=0
    - derphi0: directional derivative at alpha=0
    - c1, c2: Wolfe constants
    - cache: dictionary caching (f, grad) for evaluated alphas
    
    Returns:
    - alpha: step length satisfying strong Wolfe conditions
    - fg_val: FG namedtuple at new point
    """

    # Use this flag for debugging purposes.
    DISPLAY_PHI_FUN = False
    
    max_iters = 20
    for i in range(max_iters):
        phi_alo, derphi_alo = phi(alo)
        phi_ahi, derphi_ahi = phi(ahi)
        
        if alo < ahi:
            x0, y0, dy0 = alo, phi_alo, derphi_alo
            x1, y1, dy1 = ahi, phi_ahi, derphi_ahi
        else:
            x0, y0, dy0 = ahi, phi_ahi, derphi_ahi
            x1, y1, dy1 = alo, phi_alo, derphi_alo

        interpolated_min = _hermite_cubic_interp_min(x0, y0, dy0, x1, y1, dy1)
        if interpolated_min is None or interpolated_min[0] < x0 or interpolated_min[0] > x1:
            alpha, i_phi_a, i_derphi_a, i_der2phi_a = 0.5 * (x0 + x1), None, None, None
        else:
            alpha, i_phi_a, i_derphi_a, i_der2phi_a = interpolated_min
        phi_a, derphi_a = phi(alpha)
        
        if DISPLAY_PHI_FUN:
            title = "_zoom ({:02d}) - Function phi(alpha): ".format(i)
            _display_line_search(phi, c1, c2, phi0, phi_a, derphi0, derphi_a, alpha1=alpha, title=title)

        if (phi_a > phi0 + c1 * alpha * derphi0) or (phi_a >= phi_alo):
            ahi = alpha
        else:
            if abs(derphi_a) <= -c2 * derphi0:
                return alpha, cache[alpha]
            if derphi_a * (ahi - alo) >= 0:
                ahi = alo
            alo = alpha
    # If no suitable alpha found, return midpoint and cached values
    return alpha, cache[alpha]

def lbfgs(fg: Collable[[np.ndarray], (float, np.ndarray)],
          x0: np.ndarray,
          max_iters: int =100,
          m: int =10,
          tol: float =1e-5,
          verbosity: int =0) -> np.ndarray:
    """
    Limited-memory BFGS optimizer with strong Wolfe line search and restart logic.

    Parameters:
    - fg: function that takes x and returns (function_value, gradient)
    - x0: initial guess (numpy array)
    - max_iters: maximum number of iterations
    - m: memory size (number of stored vector pairs)
    - tol: tolerance for gradient norm to stop optimization
    - verbosity: 0="no messages", 1 or more means an increasing verbosity

    Returns:
    - x: optimized parameters
    """
    
    CURVATURE_THRESHOLD = 1.e-10

    fg_call_count = 0
    fg_call_count_last = 0
    
    def fgc(x: np.ndarray) -> FG:
        """
        Compute FG namedtuple (fun, grad) at x and increment call count.
        """
        nonlocal fg_call_count
        fg_call_count += 1
        fgv = fg(x)
        return FG(*fgv)
    
    x = x0.copy()
    sl = []
    yl = []
    rho = []
    fgv = fgc(x) # fgv : function gradient value at x

    for k in range(max_iters):
        g_norm = np.linalg.norm(fgv.grad)
        if g_norm < tol:
            if verbosity > 0:
                print(f"Converged at iteration {k}, gradient norm {g_norm:.3e}")
            break

        # Compute search direction using two-loop recursion
        p = - _two_loop_recursion(fgv.grad, sl, yl, rho)

        # Restart if direction is not descent
        if np.dot(p, fgv.grad) >= 0.:
            if verbosity > 0:
                print(f"Restart at iteration {k}: direction not descent, resetting memory")
            sl.clear()
            yl.clear()
            rho.clear()
            p = -fgv.grad  # steepest descent direction

        # Strong Wolfe line search, pass current function and gradient
        alpha, fgv_new = _strong_wolfe_line_search(fgc, x, p, fgv)

        x_new = x + alpha * p

        s = x_new - x
        y = fgv_new.grad - fgv.grad

        # Update memory if curvature condition satisfied
        if np.dot(y, s) > CURVATURE_THRESHOLD:
            if len(sl) == m:
                sl.pop(0)
                yl.pop(0)
                rho.pop(0)
            sl.append(s)
            yl.append(y)
            rho.append(1.0 / np.dot(y, s))

        x = x_new
        fgv = fgv_new

        if verbosity > 0:
            count = fg_call_count - fg_call_count_last
            print(f"[{k:04d}/{max_iters:04d}]: f = {fgv.fun:.8e}, ||g|| = {np.linalg.norm(fgv.grad):.3e}, "
                  f"alpha = {alpha:.3e}, m = [{len(sl):02d}/{m:02d}], fgc = {count:d}")
            fg_call_count_last = fg_call_count

    print("fgc (count of call to fg function) = {:d}".format(fg_call_count))
    return x
