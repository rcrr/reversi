#
# load_rglm_model_torch.py
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

import torch
import torch.profiler
from torch.profiler import profile, record_function, ProfilerActivity
import scipy.sparse

from reversi.rglm import *

from line_profiler import LineProfiler

import warnings

warnings.filterwarnings('ignore', '.*Sparse CSR tensor support is in beta state.*')

#
# From the $(REVERSI_HOME)/c directory run this test module doing:
#
# $ source py/.reversi_venv/bin/activate
# $ PYTHONPATH="./py" python3 py/perf/load_rglm_model_torch.py --filename abc.txt
#


verbose = True

print("torch.get_num_threads() = {}".format(torch.get_num_threads()))
torch.set_num_threads(4)
torch.set_num_interop_threads(4)
print("torch.get_num_threads() = {}".format(torch.get_num_threads()))

ridge_regularization = 0.01

iterations = 1

def csr_to_torch_sparse_csr(csr_mat: scipy.sparse._csr.csr_matrix):
    csr_mat = csr_mat.astype('float32')
    crow_indices = torch.tensor(csr_mat.indptr, dtype=torch.int64)
    col_indices = torch.tensor(csr_mat.indices, dtype=torch.int64)
    values = torch.tensor(csr_mat.data, dtype=torch.float32)
    shape = csr_mat.shape
    sparse_tensor = torch.sparse_csr_tensor(crow_indices, col_indices, values, size=shape)
    return sparse_tensor


def fg_builder(x: torch.Tensor,
               y: torch.Tensor,
               c: torch.Tensor):

    xt = x.transpose(0, 1).to_sparse_csr()

    def fg(w: torch.Tensor):
        with record_function("linear_predictor"):
            linear_predictor = torch.matmul(x, w)
        with record_function("sigmoid"):
            yh = torch.sigmoid(linear_predictor)
        with record_function("gradient_parts"):
            dyh = yh * (1. - yh)
            rn = yh - y
            dyh_rn = dyh * rn
        with record_function("loss_computation"):
            norm_rn = torch.dot(rn, rn)
            norm_w = torch.dot(w, w)
            f = 0.5 * (norm_rn + c * norm_w)
        with record_function("gradient_computation"):
            g0 = torch.matmul(xt, dyh_rn)
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
        
    #ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
    #ps.print_stats()

    print("load_rglm_model: RGLM loaded")

    #device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    device = torch.device('cuda')
    
    x = csr_to_torch_sparse_csr(m.x)
    y = torch.from_numpy(m.y).float()
    c = torch.tensor(ridge_regularization)
    
    x_t = x.to(device)
    y_t = y.to(device)
    c_t = c.to(device)

    fg = fg_builder(x_t, y_t, c_t)
    w = torch.from_numpy(m.w.to_numpy()).float().to(device)
    
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

    print("--- Torch Profiler : start ---")
    with torch.profiler.profile(activities=[torch.profiler.ProfilerActivity.CPU,
                                            torch.profiler.ProfilerActivity.CUDA],
                                record_shapes=True) as prof:
        with record_function("model_loss_and_grad"):
            f, g = fg(w)

    print(prof.key_averages().table(sort_by="cuda_time_total", row_limit=12))
    prof.export_chrome_trace("trace.json")
    print("--- Torch Profiler : stop ---")

    print("load_rglm_model: STOP")
