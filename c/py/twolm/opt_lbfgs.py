#
# opt_lbfgs.py
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

# twolm/opt_lbfgs.py
from __future__ import annotations

import math
from collections import namedtuple
from typing import Callable, Tuple, List, Optional

import numpy as np

# FG : Function , Gradient named tuple
FG = namedtuple('FG', ['fun', 'grad'])



__all__ = ['lbfgs', 'FG']



def _display_line_search(phi: Callable[[float], Tuple[float, float]],
                         c1: float, c2: float,
                         phi0: float, phi_a1: float,
                         derphi0: float, derphi_a1: float,
                         window: float = 3., dots: int = 30,
                         alpha0: float = 0., alpha1: float = 1.,
                         title: str = 'Function phi(alpha)') -> None:
    """Displays a plot of the line search function phi(alpha)."""
    try:
        import matplotlib.pyplot as plt
    except ImportError:
        print("L-BFGS DEBUG: Matplotlib is not installed. Cannot display plot.")
        return

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
    armijo_line = phi0 + c1 * t * derphi0
    interpolation_results = _hermite_cubic_interp_min(alpha0, phi0, derphi0, alpha1, phi_a1, derphi_a1)
    if interpolation_results is None:
        i_min_x, i_min_y = None, None
    else:
        i_min_x, i_min_y, _, _ = interpolation_results

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
    plt.plot(t[min_i], min_f, marker='o', color='black', markersize=6)
    for u in range(len(t)):
        if curvature_cond[u]:
            plt.plot(t[u], f[u], marker='o', color='green', markersize=4)
    
    plt.show()
    return

def _hermite_cubic_interp_min(x0: float, y0: float, dy0: float,
                               x1: float, y1: float, dy1: float) -> Optional[Tuple[float, float, float, float]]:
    """
    Finds the minimum of a cubic Hermite interpolant within the interval [x0, x1].
    Returns (x_min, y_min, dy_min, d2y_min) or None if no minimum is found.
    """
    dx = x1 - x0
    dy = y1 - y0
    
    a = - 6 * dy + 3 * dx * (dy0 + dy1)
    b =   6 * dy - 2 * dx * (2 * dy0 + dy1)
    c =                dx *       dy0

    if a == 0.:
        return None

    discriminant = b * b - 4 * a * c
    if discriminant <= 0.:
        return None
    sd = math.sqrt(discriminant)

    t_stationary_0 = (-b + sd) / (2 * a)
    t_stationary_1 = (-b - sd) / (2 * a)

    x_stationary_0 = x0 + t_stationary_0 * dx
    x_stationary_1 = x0 + t_stationary_1 * dx

    stationary_point_0 = _hermite_cubic_interp(x0, y0, dy0, x1, y1, dy1, x_stationary_0)
    stationary_point_1 = _hermite_cubic_interp(x0, y0, dy0, x1, y1, dy1, x_stationary_1)

    if stationary_point_0[3] > 0.:
        return stationary_point_0
    else:
        return stationary_point_1


def _hermite_cubic_interp(x0: float, y0: float, dy0: float,
                          x1: float, y1: float, dy1: float,
                          x: float) -> Tuple[float, float, float, float]:
    """
    Evaluates the cubic Hermite interpolant and its first two derivatives at x.
    """
    dx = x1 - x0
    t = (x - x0) / dx
    t2 = t * t
    t3 = t2 * t
    
    h0  =  2 * t3 - 3 * t2 + 1
    h1  = -2 * t3 + 3 * t2
    h2  =      t3 - 2 * t2 + t
    h3  =      t3 -     t2
    
    h01 =             6 * t2 - 6 * t
    h11 =          - 6 * t2 + 6 * t
    h21 =             3 * t2 - 4 * t + 1
    h31 =             3 * t2 - 2 * t
    
    h02 =                       12 * t - 6
    h12 =                     - 12 * t + 6
    h22 =                        6 * t - 4
    h32 =                        6 * t - 2

    h   =   y0 * h0  + y1 * h1  + dy0 * dx * h2  + dy1 * dx * h3
    dh  = ( y0 * h01 + y1 * h11 + dy0 * dx * h21 + dy1 * dx * h31 ) / dx
    d2h = ( y0 * h02 + y1 * h12 + dy0 * dx * h22 + dy1 * dx * h32 ) / (dx * dx)
    
    return x, h, dh, d2h


def _two_loop_recursion(g: np.ndarray, s: List[np.ndarray], y: List[np.ndarray], rho: List[float]) -> np.ndarray:
    """
    Perform the L-BFGS two-loop recursion to compute H_k * q efficiently.
    
    Parameters
    ----------
    g : np.ndarray
        Gradient vector at current x.
    s : List[np.ndarray]
        Stored s vectors (x_{k+1} - x_k).
    y : List[np.ndarray]
        Stored y vectors (grad_{k+1} - grad_k).
    rho : List[float]
        Scalars precomputed as 1 / (y_i^T s_i).

    Returns
    -------
    np.ndarray
        The product H_k * g (negated search direction).
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
                              x: np.ndarray, p: np.ndarray, fgv0: FG,
                              c1: float, c2: float, alpha_max: float = 10.0,
                              max_iters: int = 3,
                              debug_plot: bool = False) -> Tuple[float, FG]:
    """
    Strong Wolfe line search implementation with full gradient caching.
    """
    cache = {}

    def phi(alpha: float) -> Tuple[float, float]:
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
    phi_prev = phi_a1

    while True:
        if debug_plot:
            disp_title = "Function phi(alpha), iter #{}".format(i)
            _display_line_search(phi, c1, c2, phi0, phi_a1, derphi0, derphi_a1,
                                 window=(alpha1*3), alpha1=alpha1, title=disp_title)
        
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
        
        interpolated_min = _hermite_cubic_interp_min(alpha0, phi0, derphi0, alpha1, phi_a1, derphi_a1)
        if interpolated_min is not None:
            i_alpha = interpolated_min[0]
        else:
            i_alpha = alpha1 * 2
            
        alpha1 = min(i_alpha, alpha_max)
        phi_a1, derphi_a1 = phi(alpha1)


def _zoom(phi: Callable[[float], Tuple[float, float]],
          alo: float, ahi: float, phi0: float, derphi0: float,
          c1: float, c2: float, cache: dict,
          debug_plot: bool = False) -> Tuple[float, FG]:
    """
    Zoom phase of strong Wolfe line search.
    """
    max_iters = 20
    alpha = 0.0
    
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
            alpha = 0.5 * (x0 + x1)
        else:
            alpha = interpolated_min[0]
            
        phi_a, derphi_a = phi(alpha)
        
        if debug_plot:
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

    return alpha, cache[alpha]


def lbfgs(fg: Callable[[np.ndarray], Tuple[float, np.ndarray]],
          x0: np.ndarray,
          c1: float = 1e-4,
          c2: float = 0.9,
          min_grad: Tuple[float, int] = (1e-5, 3),
          min_p_fun_decrease: Tuple[float, int] = (1e-14, 7),
          max_iters: int = 100,
          m: int = 10,
          log_every_n: int = 0,
          logger: Callable[[str], None] = print,
          save_every_n: int = 0,
          save_fn: Optional[Callable[[np.ndarray, List[np.ndarray], List[np.ndarray], List[float], int], None]] = None,
          initial_sl: Optional[List[np.ndarray]] = None,
          initial_yl: Optional[List[np.ndarray]] = None,
          initial_rho: Optional[List[float]] = None,
          debug_plot: bool = False) -> np.ndarray:
    """
    Limited-memory BFGS optimizer with strong Wolfe line search and restart logic.
    Supports logging, checkpointing, and warm starting.

    Parameters
    ----------
    fg : Callable[[np.ndarray], Tuple[float, np.ndarray]]
        Function that takes x and returns (function_value, gradient).
    x0 : np.ndarray
        Initial guess.
    c1, c2 : float
        Wolfe condition constants (0 < c1 < c2 < 1).
    min_grad : Tuple[float, int]
        Stopping tolerance for gradient norm. Stops if ||grad f(x)|| < min_grad[0] 
        for min_grad[1] consecutive iterations.
    min_p_fun_decrease : Tuple[float, int]
        Minimum percentage acceptable loss decrease. Stops if relative decrease 
        is < min_p_fun_decrease[0] for min_p_fun_decrease[1] consecutive iterations.
    max_iters : int
        Maximum number of iterations.
    m : int
        Memory size (number of stored vector pairs).
    log_every_n : int
        Frequency of logging. 0 = no log, 1 = every iteration, 10 = every 10 iterations.
    logger : Callable[[str], None]
        Logging function (defaults to print).
    save_every_n : int
        Frequency of saving checkpoints. 0 = no save.
    save_fn : Callable
        Function to save state: save_fn(x, sl, yl, rho, k).
    initial_sl, initial_yl, initial_rho : List
        Initial state for warm starting (resuming an optimization).

    Returns
    -------
    np.ndarray
        Optimized parameters.
    """
    
    if not isinstance(x0, np.ndarray):
        raise TypeError('L-BFGS - Error: argument x0 is not an instance of numpy.ndarray')
    
    supported_types = [np.float32, np.float64]
    if x0.dtype not in supported_types:
        raise TypeError(f'L-BFGS - Error: argument x0 has unsupported dtype {x0.dtype}')    
    
    if not callable(fg):
        raise TypeError('L-BFGS - Error: argument fg is not callable')
    
    CURVATURE_THRESHOLD = 1.e-10
    
    min_grad_value, min_grad_count = min_grad
    min_p_fun_decrease_value, min_p_fun_decrease_count = min_p_fun_decrease

    fg_call_count = 0
    fg_call_count_last = 0
    
    low_progres_count_f = 0
    low_progres_count_g = 0

    def fgc_plain(x: np.ndarray) -> FG:
        """Compute FG namedtuple (fun, grad) at x and increment call count."""
        nonlocal fg_call_count
        fg_call_count += 1
        return FG(*fg(x))
    
    # Initialize state
    x = x0.copy()
    sl = initial_sl if initial_sl is not None else []
    yl = initial_yl if initial_yl is not None else []
    rho = initial_rho if initial_rho is not None else []
    
    fgv = fgc_plain(x)
    fgv_fun_prev = fgv.fun + np.fabs(fgv.fun)

    for k in range(max_iters):
        
        # --- Stopping criteria ---
        g_norm = np.linalg.norm(fgv.grad)
        
        if g_norm < min_grad_value:
            low_progres_count_g += 1
        else:
            low_progres_count_g = 0
            
        if low_progres_count_g >= min_grad_count:
            if log_every_n > 0:
                logger(f"L-BFGS - Converged at iteration {k}, gradient norm {g_norm:.3e}")
            break
            
        if fgv.fun != 0.:
            if fgv.fun < fgv_fun_prev:
                diff = fgv_fun_prev - fgv.fun
                p_diff = diff / np.fabs(fgv.fun)
                if p_diff < min_p_fun_decrease_value:
                    low_progres_count_f += 1
                else:
                    low_progres_count_f = 0
            else:
                low_progres_count_f += 1
                
        if low_progres_count_f >= min_p_fun_decrease_count:
            if log_every_n > 0:
                logger(f"L-BFGS - Stopped at iteration {k}, low function progress")
            break

        # --- Compute search direction ---
        p = - _two_loop_recursion(fgv.grad, sl, yl, rho)

        # Restart if direction is not a descent direction
        if np.dot(p, fgv.grad) >= 0.:
            if log_every_n > 0:
                logger(f"L-BFGS - Restart at iteration {k}: direction not descent, resetting memory")
            sl.clear()
            yl.clear()
            rho.clear()
            p = -fgv.grad  # Steepest descent direction

        # --- Line Search ---
        alpha, fgv_new = _strong_wolfe_line_search(fgc_plain, x, p, fgv, c1, c2, debug_plot=debug_plot)

        x_new = x + alpha * p

        # --- Update Memory ---
        s = x_new - x
        y = fgv_new.grad - fgv.grad

        if np.dot(y, s) > CURVATURE_THRESHOLD:
            if len(sl) == m:
                sl.pop(0)
                yl.pop(0)
                rho.pop(0)
            sl.append(s)
            yl.append(y)
            rho.append(1.0 / np.dot(y, s))

        # --- State Update ---
        x = x_new
        fgv_fun_prev = fgv.fun
        fgv = fgv_new

        # --- Logging ---
        if log_every_n > 0 and (k % log_every_n == 0 or k == max_iters - 1):
            count = fg_call_count - fg_call_count_last
            logger(f"L-BFGS - [{k:04d}/{max_iters:04d}]: f = {fgv.fun:.8e}, ||g|| = {np.linalg.norm(fgv.grad):.3e}, "
                   f"alpha = {alpha:.3e}, m = [{len(sl):02d}/{m:02d}], fgc = {count:d}")
            fg_call_count_last = fg_call_count

        # --- Checkpointing ---
        if save_every_n > 0 and save_fn is not None and (k % save_every_n == 0):
            save_fn(x, sl, yl, rho, k)

    if log_every_n > 0:
        logger(f"L-BFGS - fgc (total calls to fg function) = {fg_call_count:d}")
    
    return x
