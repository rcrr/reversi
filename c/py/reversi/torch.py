#
# torch.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2024, 2025 Roberto Corradini. All rights reserved.
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

import reversi
import reversi.board
import reversi.pattern
import reversi.cfg
import reversi.regab

import torch
import time

from copy import deepcopy


#
# How to use this module.
#
# -0- Open the python console.
#
# $ source py/.reversi_venv/bin/activate
# $ python3
#
# -1- From python console run:
#
# >>> exec(open("py/reversi/start.py").read())
# >>> m = rglm_workflow(test_run_0)
# >>> m.opt_res
#
# -2- Load the reversi.torch module (this file).
#
# >>> exec(open("py/reversi/torch.py").read())
#
# -2- Compute the loss using pytorch
#
# >>> rc = 0.1
# >>> tw = torch.from_numpy(m.w)
# >>> ty = torch.from_numpy(m.y)
# >>> tx = torch.sparse_csr_tensor(torch.from_numpy(m.x.indptr), torch.from_numpy(m.x.indices), torch.from_numpy(m.x.data))
# >>> loss = loss_function_generator(tx, ty, rc)
# >>> tl = loss(tw)
# >>> tl
#

# Common value for the Ridge Regularization Coefficient.
ridge_reg_coeff = 0.1


#
# It is not completed ... it is not working ...
#
def set_device(activate_cuda: boolean = False) -> str:
    """
    Sets the device. CUDA if available, CPU otherwise
    """
    device = 'cpu'
    if activate_cuda:
        device = "cuda" if torch.cuda.is_available() else "cpu"
    return device


def rglm_tsigmoid(x: torch.tensor) -> torch.tensor:
    """
    Computes the sigmoid (logistic) function on the given tensor of float values.
    """
    return 1. / (1. + torch.exp(-x))

def loss_function_generator(x: torch.tensor, y: torch.tensor, rc: float) -> Callable:
    """
    Returns a callable function loss(tw) that computes the loss of the model.
      Parameters:
        x  : matrix of correlations between the observations and the independent variables.
        y  : epsilon is the vector of the true game position values.
        rc : ridge regularization coefficient.
    """
    def loss (w: torch.tensor) -> torch.tensor:
        """
        Returns a scalar (zero-dimensional tensor) evaluating the loss function at w.
          Parameters:
            w : weights is the vector of independent variables.
        """
        # lp : linear predictor.
        lp = torch.mv(x, w)
        # yh : epsilon-hat is the vector of estimated game position values.
        yh = rglm_tsigmoid(lp)
        # r : residual
        r = y - yh
        # lv : loss value
        lv = 0.5 * (sum(r**2) + rc * sum(w**2))
        return lv
    return loss

#
# >>> m.opt_res
#  message: CONVERGENCE: RELATIVE REDUCTION OF F <= FACTR*EPSMCH
#  success: True
#   status: 0
#      fun: 894.074342366716
#        x: [-6.522e-01  3.752e+00 ... -1.815e-01  1.161e-01]
#      nit: 416
#      jac: [-2.086e-01 -1.181e-01 ... -7.732e-04 -3.063e-02]
#     nfev: 433
#     njev: 433
# hess_inv: <2985x2985 LbfgsInvHessProduct with dtype=float64>
#
# iter_elapsed_time=3.254402, loss=894.0735937144741, w=tensor([-0.6521,  3.7518, -1.9018,  ..., -0.0114, -0.1815,  0.1161],
# minimize iteration 4999 of 5000
# iter_elapsed_time=3.180163, loss=894.0737673390495, w=tensor([-0.6063,  3.7572, -1.9133,  ..., -0.0231, -0.1932,  0.1044],
#       device='cuda:0', dtype=torch.float64, requires_grad=True)
#

def rglm_minimize(loss: Callable,
                  initial_weights: torch.tensor,
                  correlation_matrix: torch.tensor,
                  true_game_pos_values: torch.tensor,
                  ridge_reg_coeff: float,
                  num_iter: int,
                  lr: float) -> torch.tensor:
    start_time = time.perf_counter()
    w = deepcopy(initial_weights)
    w.requires_grad_()
    loss_f = loss_function_generator(correlation_matrix, true_game_pos_values, ridge_reg_coeff)
    optimizer = torch.optim.Adam([w], lr=lr, betas=(0.9, 0.999))
    for i in range(num_iter):
        start_iter_time = time.perf_counter()
        print("minimize iteration {} of {}".format(i, num_iter))
        optimizer.zero_grad()
        loss = loss_f(w)
        loss.backward()
        optimizer.step()
        end_iter_time = time.perf_counter()
        iter_elapsed_time = end_iter_time - start_iter_time
        print("iter_elapsed_time={:.6f}, loss={}, w={}".format(iter_elapsed_time, loss, w))
    total_time = iter_elapsed_time - start_time
    print("total_time={:.6f}".format(total_time))
    return w
