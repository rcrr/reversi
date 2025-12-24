#
# load_rglm_model.py
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

import sys
import argparse

import numpy as np

import cProfile
import pstats
from pstats import SortKey

import reversi
import reversi.board
import reversi.pattern
import reversi.cfg
import reversi.regab
import reversi.rglm
import reversi.optimization
import reversi.opt_lbfgs

from reversi.rglm import *

from line_profiler import LineProfiler

#
# From the $(REVERSI_HOME)/c directory run this test module doing:
#
# $ source py/.reversi_venv/bin/activate
# $ PYTHONPATH="./py" python3 py/perf/load_rglm_model.py --filename rglmdata/A2050_01.dat
#

# 01-08-2025
#
# Line #      Hits         Time  Per Hit   % Time  Line Contents
# ==============================================================
#     67                                               def fg(w):
#     68         1  113231451.0 1.13e+08     40.4          linear_predictor = x @ w
#     69         1   22662186.0 2.27e+07      8.1          yh = rglm_sigmoid(linear_predictor)
#     70         1    3601366.0  3.6e+06      1.3          dyh = yh * (1. - yh)
#     71         1    3167303.0 3.17e+06      1.1          rn = yh - y
#     72         1     630336.0 630336.0      0.2          norm_rn = np.dot(rn, rn)
#     73         1     359953.0 359953.0      0.1          norm_w = np.dot(w, w)
#     74         1       2975.0   2975.0      0.0          f = 0.5 * (norm_rn + c * norm_w)
#     75         1    3623547.0 3.62e+06      1.3          dyh_rn = dyh * rn
#     76         1  131453049.0 1.31e+08     46.9          g0 = xt @ dyh_rn
#     77         1     841818.0 841818.0      0.3          g1 = c * w
#     78         1     449453.0 449453.0      0.2          g = g0 + g1
#     79         1       1282.0   1282.0      0.0          return f, g
#
#
# Reduced from 61s for 100 calls to 27s !!!
# Key actions has been to use rn instead of r, removing in this way the negated gradient formula.
# Move to np.dot(x, x) from sum(x**2) to compute the norm of arrays.
#
# Running the lbfgs algo at 32bit works.
# It is 100% faster ( 2x the speed compared to 64bit )
# It produces the same optimization path.
# It reaches a stopping point where it is no longer able to lower the loss function.
# It finds a value right at the fifth decimal digit.
#
# It so fast now that the bottleneck is computing the pattern index values ....
# Would it be better to write a new workflow ... saving X and Y on disk, and retrieve them ...
#

verbose = True

c = 0.01

iterations = 1

def fg_builder(x, y, c):
    
    xt = x.transpose()

    def fg(w):
        linear_predictor = x @ w
        yh = rglm_sigmoid(linear_predictor)
        dyh = yh * (1. - yh)
        rn = yh - y
        norm_rn = np.dot(rn, rn)
        norm_w = np.dot(w, w)
        f = 0.5 * (norm_rn + c * norm_w)
        dyh_rn = dyh * rn
        g0 = xt @ dyh_rn
        g1 = c * w
        g = g0 + g1
        return f, g

    return fg


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--filename', help="RGLM filename to load")
    args, remainder = parser.parse_known_args()
    rglm_file_to_load = args.filename

    print("load_rglm_model: START")

    m = Rglm()
    
    with cProfile.Profile() as pr:
        m.read_from_binary_file(rglm_file_to_load, verbose)
        
    ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
    ps.print_stats()

    print("load_rglm_model: RGLM loaded")

    fg = fg_builder(m.x, m.y, c)
    w = copy.deepcopy(m.w)
    
    with cProfile.Profile() as pr:
        for i in range(iterations):
            f, g = fg(w)
    
    ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
    ps.print_stats()
    
    print("f = {}".format(f))
    print("g = {}".format(g))    

    print("--- Line Profiler : start ---")
    lp = LineProfiler()
    lp.add_function(fg)
    lp.enable()
    f, g = fg(w)
    lp.disable()
    lp.print_stats()
    
    print("--- Line Profiler : stop ---")
    
    print("f = {}".format(f))
    print("g = {}".format(g))    

    print("load_rglm_model: 32 bit ...")

    x32 = m.x.astype(np.float32)
    y32 = m.y.astype(np.float32)
    c32 = np.array(c, dtype=np.float32)

    fg32 = fg_builder(x32, y32, c32)
    w32 = copy.deepcopy(m.w.astype(np.float32))
    
    with cProfile.Profile() as pr:
        for i in range(iterations):
            f32, g32 = fg32(w32)
    
    ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
    ps.print_stats()
    
    print("f32 = {}".format(f32))
    print("g32 = {}".format(g32))    

    print("--- Line Profiler : start ---")
    lp = LineProfiler()
    lp.add_function(fg32)
    lp.enable()
    f32, g32 = fg32(w32)
    lp.disable()
    lp.print_stats()
    
    print("--- Line Profiler : stop ---")
    
    print("f32 = {}".format(f32))
    print("g32 = {}".format(g32))    

    print("load_rglm_model: STOP")
