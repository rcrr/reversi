#
# compute_fg.py
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
import reversi.tensor_utils as rtu

from line_profiler import LineProfiler

from typing import Any, List, Tuple, Callable

#
# From the $(REVERSI_HOME)/c directory run this test module doing:
#
# $ source py/.reversi_venv/bin/activate
# $ PYTHONPATH="./py" python3 py/perf/compute_fg.py --filename abc.txt --iterations 10 --cuda-mem-size 32
# $ PYTHONPATH="./py" time python3 py/perf/compute_fg.py -i 100 -c 128 -f out/H2051_00.dat
#
# The performance test has the following steps:
#
# -0- Imports the required libraries like reversi and torch.
# -1- Loads the RGLM data file into a Rglm python class, the model.
# -2- Extracts from the model the X CSR matrix, Y and W arrays, as well as the C constant.
# -3- Converts to PyTorch Tensor the X, Y, W data. 
# -4- Builds the fg() function.
# -5- Calls the fg() function once for testing.
# -6- Calls the fg() function time iterations, tracing the execution.
# -7- Print the timings.
#
# The sequence is run different times, using a different fg builder.
#
# -a- Torch CUDA/GPU sliced algo
# -b- Torch CPU algo
# -c- Torch CUDA/GPU plain algo
#
# The fg() function is used to optimize the RGLM model ...
# Devo controllare bene che i tensori W, F, G partano sempre dalla CPU e arrivino alla CPU ...
# HERE - SIAMO QUI 


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

def fg_builder_torch_gpu(x: torch.Tensor,
                         y: torch.Tensor,
                         c: torch.Tensor) -> Callable[[torch.Tensor], Tuple[torch.Tensor, torch.Tensor]]:
    """
    Returns the function that computes value and gradient for the loss.
    """
    assert isinstance(x, torch.Tensor), f"Argument x must be a torch.Tensor, got {type(x)}"
    assert x.layout == torch.sparse_csr, "Argument x must be a tensor having CSR layout"
    assert x.ndim == 2, f"Argument x must have ndim equal to 2, got {x.ndim}"
    assert isinstance(y, torch.Tensor), f"Argument y must be a torch.Tensor, got {type(y)}"
    assert y.ndim == 1, f"Argument y must have ndim equal to 1, got {y.ndim}"
    assert x.size()[0] == y.size()[0], f"Arguments x and y are not consistent, got x:{x.size()}, y:{y.size()}"
    assert isinstance(c, torch.Tensor), f"Argument c must be a torch.Tensor, got {type(c)}"
    assert c.ndim == 0, f"Argument c must have ndim equal to 0, got {c.ndim}"
    
    device = torch.device('cuda')

    if not x.device.type == 'cpu':
        raise RuntimeError(f"The x argument is a Tensor having device {x.device}, the function is designed for x hosted on CPU.")
    
    xt = x.transpose(0, 1).to_sparse_csr()

    if not y.device.type == 'cpu':
        raise RuntimeError(f"The y argument is a Tensor having device {y.device}, the function is designed for y hosted on CPU.")

    if not c.device.type == 'cpu':
        raise RuntimeError(f"The c argument is a Tensor having device {c.device}, the function is designed for c hosted on CPU.")

    x = x.to(device)
    xt = xt.to(device)
    y = y.to(device)
    c = c.to(device)

    def fg(w: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        assert isinstance(w, torch.Tensor), f"Argument w must be a torch.Tensor, got {type(w)}"
        if not w.device.type == 'cpu':
            raise RuntimeError(f"The w argument is a Tensor having device {w.device}, the function is designed for w hosted on CPU.")
        w = w.to(device)
        linear_predictor = torch.matmul(x, w)
        yh = torch.sigmoid(linear_predictor)
        dyh = yh * (1. - yh)
        rn = yh - y
        dyh_rn = dyh * rn
        norm_rn = torch.dot(rn, rn)
        norm_w = torch.dot(w, w)
        f = 0.5 * (norm_rn + c * norm_w)
        g0 = torch.matmul(xt, dyh_rn)
        g1 = c * w
        g = g0 + g1
        return f.to('cpu'), g.to('cpu')

    return fg

def fg_builder_torch_cpu(x: torch.Tensor,
                         y: torch.Tensor,
                         c: torch.Tensor) -> Callable[[torch.Tensor], Tuple[torch.Tensor, torch.Tensor]]:
    """
    Returns the function that computes value and gradient for the loss.
    """
    assert isinstance(x, torch.Tensor), f"Argument x must be a torch.Tensor, got {type(x)}"
    assert x.layout == torch.sparse_csr, "Argument x must be a tensor having CSR layout"
    assert x.ndim == 2, f"Argument x must have ndim equal to 2, got {x.ndim}"
    assert isinstance(y, torch.Tensor), f"Argument y must be a torch.Tensor, got {type(y)}"
    assert y.ndim == 1, f"Argument y must have ndim equal to 1, got {y.ndim}"
    assert x.size()[0] == y.size()[0], f"Arguments x and y are not consistent, got x:{x.size()}, y:{y.size()}"
    assert isinstance(c, torch.Tensor), f"Argument c must be a torch.Tensor, got {type(c)}"
    assert c.ndim == 0, f"Argument c must have ndim equal to 0, got {c.ndim}"

    device = torch.device('cpu')

    if not x.device.type == 'cpu':
        raise RuntimeError(f"The x argument is a Tensor having device {x.device}, the function is designed for x hosted on CPU.")
    
    xt = x.transpose(0, 1).to_sparse_csr()

    if not y.device.type == 'cpu':
        raise RuntimeError(f"The y argument is a Tensor having device {y.device}, the function is designed for y hosted on CPU.")

    if not c.device.type == 'cpu':
        raise RuntimeError(f"The c argument is a Tensor having device {c.device}, the function is designed for c hosted on CPU.")

    def fg(w: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        assert isinstance(w, torch.Tensor), f"Argument w must be a torch.Tensor, got {type(w)}"
        if not w.device.type == 'cpu':
            raise RuntimeError(f"The w argument is a Tensor having device {w.device}, the function is designed for w hosted on CPU.")
        linear_predictor = torch.matmul(x, w)
        yh = torch.sigmoid(linear_predictor)
        dyh = yh * (1. - yh)
        rn = yh - y
        dyh_rn = dyh * rn
        norm_rn = torch.dot(rn, rn)
        norm_w = torch.dot(w, w)
        f = 0.5 * (norm_rn + c * norm_w)
        g0 = torch.matmul(xt, dyh_rn)
        g1 = c * w
        g = g0 + g1
        return f, g

    return fg

def fg_builder_torch_gpu_sliced(x: torch.Tensor,
                                y: torch.Tensor,
                                c: torch.Tensor,
                                block_mem_size: int) -> Callable[[torch.Tensor], Tuple[torch.Tensor, torch.Tensor]]:
    """
    Returns the function that computes value and gradient for the loss.
    """
    assert isinstance(x, torch.Tensor), f"Argument x must be a torch.Tensor, got {type(x)}"
    assert x.layout == torch.sparse_csr, "Argument x must be a tensor having CSR layout"
    assert x.ndim == 2, f"Argument x must have ndim equal to 2, got {x.ndim}"
    assert isinstance(y, torch.Tensor), f"Argument y must be a torch.Tensor, got {type(y)}"
    assert y.ndim == 1, f"Argument y must have ndim equal to 1, got {y.ndim}"
    assert x.size()[0] == y.size()[0], f"Arguments x and y are not consistent, got x:{x.size()}, y:{y.size()}"
    assert isinstance(c, torch.Tensor), f"Argument c must be a torch.Tensor, got {type(c)}"
    assert c.ndim == 0, f"Argument c must have ndim equal to 0, got {c.ndim}"

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    if x.device.type == 'cuda':
        raise RuntimeError(f"The x argument is a Tensor having device {x.device}, the function is designed for x hosted on CPU.")
    
    xt = x.transpose(0, 1).to_sparse_csr()

    if not y.device.type == 'cuda':
        y = y.to(device)

    if not c.device.type == 'cuda':
        c = c.to(device)

    xblocks_by_mu = rtu.precompute_csr_slices_by_mem_usage(x, block_mem_size)    
    xtblocks_by_mu = rtu.precompute_csr_slices_by_mem_usage(xt, block_mem_size)
    
    def fg(w: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        assert isinstance(w, torch.Tensor), f"Argument w must be a torch.Tensor, got {type(w)}"
        if not w.device.type == 'cpu':
            raise RuntimeError(f"The w argument is a Tensor having device {w.device}, the function is designed for w hosted on CPU.")
        w = w.to(device)
        linear_predictor = rtu.matmul_by_slices(x, w, xblocks_by_mu, device, device)
        yh = torch.sigmoid(linear_predictor)
        dyh = yh * (1. - yh)
        rn = yh - y
        dyh_rn = dyh * rn
        norm_rn = torch.dot(rn, rn)
        norm_w = torch.dot(w, w)
        f = 0.5 * (norm_rn + c * norm_w)
        g0 = rtu.matmul_by_slices(xt, dyh_rn, xtblocks_by_mu, device, device)
        g1 = c * w
        g = g0 + g1
        return f.to('cpu'), g.to('cpu')
    
    return fg

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

ridge_regularization = 0.01

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-f', '--filename', required=True, help="RGLM filename to load")
    parser.add_argument('-i', '--iterations', required=True, type=int, help="Number of iterations")
    parser.add_argument('-c', '--cuda-mem-size', required=True, type=int, help="Size in MB of the mem block copied from CPU to CUDA device")
    args, remainder = parser.parse_known_args()
    rglm_file_to_load = args.filename
    iterations = args.iterations
    cuda_mem_size = args.cuda_mem_size

    print("compute_fg: START")

    verbose = True

    m = Rglm()
    m.read_from_binary_file(rglm_file_to_load, verbose)

    print("compute_fg: RGLM loaded")
    
    print("compute_fg: ##### #### #### #### #### Start of torch cuda sliced algo #### #### #### #### ####")

    if not torch.cuda.is_available():
        print("CUDA device is not available on this system.")
        status = 1
        sys.exit(status)
    device = torch.device('cuda')

    x = rtu.csr_matrix_scipy_to_torch(m.x).pin_memory()
    y = torch.from_numpy(m.y).float()
    c = torch.tensor(ridge_regularization)
    w = torch.from_numpy(m.w.to_numpy()).float()

    print("compute_fg: PyTorch Tensors x, y, c and w  are computed and loaded on CPU")

    block_mem_size = cuda_mem_size * 1024 * 1024
    fg = fg_builder_torch_gpu_sliced(x, y, c, block_mem_size)

    print("compute_fg: Function fg() generated.")

    if not w.device.type == 'cpu':
        raise RuntimeError(f"The w argument is a Tensor having device {w.device}, the function is designed for w hosted on CPU.")
    f, g = fg(w)
    if not f.device.type == 'cpu':
        raise RuntimeError(f"The f argument is a Tensor having device {f.device}, the function is designed for f hosted on CPU.")
    if not g.device.type == 'cpu':
        raise RuntimeError(f"The g argument is a Tensor having device {g.device}, the function is designed for g hosted on CPU.")

    print("compute_fg: Function fg() executed.")
    
    print("f = {}".format(f))
    print("g = {}".format(g))
    
    with cProfile.Profile() as pr:
        for i in range(iterations):
            f, g = fg(w)
    
    ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
    ps.print_stats()

    del device, x, y, c, w, fg, f, g, pr, ps
    
    print("compute_fg: ##### #### #### #### #### End of torch cuda sliced algo #### #### #### #### ####")

    print("compute_fg: ##### #### #### #### #### Start of torch CPU algo #### #### #### #### ####")

    verbose = True
    device = torch.device('cpu')

    x = rtu.csr_matrix_scipy_to_torch(m.x)
    y = torch.from_numpy(m.y).float()
    c = torch.tensor(ridge_regularization)
    w = torch.from_numpy(m.w.to_numpy()).float()

    print("compute_fg: PyTorch Tensors x, y, c, w are computed and loaded on CPU")

    fg = fg_builder_torch_cpu(x, y, c)

    print("compute_fg: Function fg() generated.")

    if not w.device.type == 'cpu':
        raise RuntimeError(f"The w argument is a Tensor having device {w.device}, the function is designed for w hosted on CPU.")
    f, g = fg(w)
    if not f.device.type == 'cpu':
        raise RuntimeError(f"The f argument is a Tensor having device {f.device}, the function is designed for f hosted on CPU.")
    if not g.device.type == 'cpu':
        raise RuntimeError(f"The g argument is a Tensor having device {g.device}, the function is designed for g hosted on CPU.")

    print("compute_fg: Function fg() executed.")
    
    print("f = {}".format(f))
    print("g = {}".format(g))
    
    with cProfile.Profile() as pr:
        for i in range(iterations):
            f, g = fg(w)
    
    ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
    ps.print_stats()

    del device, x, y, c, w, fg, f, g, pr, ps

    print("compute_fg: ##### #### #### #### #### End of torch CPU algo #### #### #### #### ####")

    print("compute_fg: ##### #### #### #### #### Start of torch GPU algo #### #### #### #### ####")

    verbose = True
    device = torch.device('cuda')

    run_gpu_only_algo = True

    if run_gpu_only_algo:
        
        x = rtu.csr_matrix_scipy_to_torch(m.x)
        y = torch.from_numpy(m.y).float()
        c = torch.tensor(ridge_regularization)
        w = torch.from_numpy(m.w.to_numpy()).float()

        print("compute_fg: PyTorch Tensors x, y, c, w are computed and loaded on CPU")

        fg = fg_builder_torch_gpu(x, y, c)

        print("compute_fg: Function fg() generated.")

        if not w.device.type == 'cpu':
            raise RuntimeError(f"The w argument is a Tensor having device {w.device}, the function is designed for w hosted on CPU.")
        f, g = fg(w)
        if not f.device.type == 'cpu':
            raise RuntimeError(f"The f argument is a Tensor having device {f.device}, the function is designed for f hosted on CPU.")
        if not g.device.type == 'cpu':
            raise RuntimeError(f"The g argument is a Tensor having device {g.device}, the function is designed for g hosted on CPU.")

        print("compute_fg: Function fg() executed.")
    
        print("f = {}".format(f))
        print("g = {}".format(g))
    
        with cProfile.Profile() as pr:
            for i in range(iterations):
                f, g = fg(w)
    
            ps = pstats.Stats(pr).sort_stats(SortKey.CUMULATIVE)
            ps.print_stats()
                
        del device, x, y, c, w, fg, f, g, pr, ps

    print("compute_fg: ##### #### #### #### #### End of torch GPU algo #### #### #### #### ####")
    
    print("compute_fg: STOP")
