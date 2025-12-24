#
# test_tensors.py
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
# $ PYTHONPATH="./py" python3 -m unittest ./py/test/test_tensors.py 
#
# For a single test run:
#
# $ PYTHONPATH="./py" python3 -m unittest test.test_tensors.TestDummy
#

import unittest
import numpy as np
import numpy.testing as npt

from scipy.sparse import csr_matrix

import torch
import warnings

from typing import Any, List, Tuple

import reversi.tensor_utils as rtu


"""
Given the matrix X and the arrays W and Z:

  X(m,n)
  W(n,)
  Z(m,)

We can define LP, XT and G as foolow:

  LP(m,) = X @ W
  XT(n,m)) = X_t
  G(n,) = XT @ Z

Where @ is the matrix multiplication operator, and _t means matrix transposition.
Between brackets there are the dimensions (shape) by rows and columns.
"""


class TestTensors(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(TestTensors, self).__init__(*args, **kwargs)

    def setUp(self):
        self.m = 8
        self.n = 3
        self.x = np.array([[0.,  1.,  2.],
                           [1.,  0.,  0.],
                           [0.,  2.,  4.],
                           [1.,  0.,  0.],
                           [0.,  4.,  8.],
                           [1.,  0.,  0.],
                           [0.,  8., 16.],
                           [1.,  0.,  0.]],
                          dtype='float32')
        self.x_data = np.array([ 1.,  2.,  1.,  2.,  4.,  1.,  4.,  8.,  1.,  8., 16.,  1.],
                               dtype='float32')
        self.x_indices = np.array([1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0], dtype='int32')
        self.x_indptr = np.array([0, 2, 3, 5, 6, 8, 9, 11, 12], dtype='int32')
        self.x_dtype = np.float32
        self.x_nnz = 12
        self.w = np.array([1., 3., 5.], dtype='float32')
        self.lp = np.array([13., 1., 26., 1., 52., 1., 104., 1.], dtype='float32')
        self.xt = np.array([[ 0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.],
                            [ 1.,  0.,  2.,  0.,  4.,  0.,  8.,  0.],
                            [ 2.,  0.,  4.,  0.,  8.,  0., 16.,  0.]], dtype='float32')
        self.xt_data = np.array([ 1.,  1.,  1.,  1.,  1.,  2.,  4.,  8.,  2.,  4.,  8., 16.],
                               dtype='float32')
        self.xt_indices = np.array([1, 3, 5, 7, 0, 2, 4, 6, 0, 2, 4, 6], dtype='int32')
        self.xt_indptr = np.array([ 0,  4,  8, 12], dtype='int32')
        self.z = np.array([7., 6., 5., 4., 3., 2., 1., 0.], dtype='float32')
        self.g = np.array([12., 37., 74.], dtype='float32')
        
        
    def tearDown(self):
        del self.m
        del self.n
        del self.x
        del self.x_data, self.x_indices, self.x_indptr, self.x_dtype, self.x_nnz
        del self.w, self.lp, self.xt
        del self.xt_data, self.xt_indices, self.xt_indptr
        del self.z, self.g
    
    def test_00_dummy(self):
        actual = 0
        desired = 0
        npt.assert_equal(actual, desired)

    def test_01_shapes(self):
        # X rows
        actual = self.x.shape[0]
        desired = self.m
        npt.assert_equal(actual, desired)
        # X colums
        actual = self.x.shape[1]
        desired = self.n
        npt.assert_equal(actual, desired)
        # W size
        actual = self.w.shape[0]
        desired = self.n
        npt.assert_equal(actual, desired)

    def test_02_multiply(self):
        actual = np.matmul(self.x, self.w)
        desired = self.lp
        npt.assert_array_equal(actual, desired)
        # Using alternative syntax
        actual = self.x @ self.w
        desired = self.lp
        npt.assert_array_equal(actual, desired)

    def test_03_transpose(self):
        actual = self.x.transpose()
        desired = self.xt
        npt.assert_array_equal(actual, desired)

    def test_04_tr_multiply(self):
        actual = self.xt @ self.z
        desired = self.g
        npt.assert_array_equal(actual, desired)

    def test_05_transform_to_csr(self):

        verbose = False
        
        # To convert a NumPy matrix (dense array) to CSR (Compressed Sparse Row) format,
        # we can use the csr_matrix function from the scipy.sparse module.
        x_csr = csr_matrix(self.x)

        npt.assert_equal(x_csr.shape, (self.m, self.n))
        if verbose: print("x_csr.shape = {}".format(x_csr.shape))

        npt.assert_equal(x_csr.data, self.x_data)
        if verbose: print("x_csr.data = {}".format(x_csr.data))

        npt.assert_equal(x_csr.indices, self.x_indices)
        if verbose: print("x_csr.indices = {}".format(x_csr.indices))

        npt.assert_equal(x_csr.indptr, self.x_indptr)
        if verbose: print("x_csr.indptr = {}".format(x_csr.indptr))

        npt.assert_equal(x_csr.dtype, self.x_dtype)
        if verbose: print("x_csr.dtype = {}".format(x_csr.dtype))

        npt.assert_equal(x_csr.nnz, self.x_nnz)
        if verbose: print("x_csr.nnz = {}".format(x_csr.nnz))

        npt.assert_equal(x_csr.ndim, 2)
        if verbose: print("x_csr.ndim = {}".format(x_csr.ndim))

        # Verify that also the transposed matrix is well formed.
        xt_csc = x_csr.transpose()
        
        npt.assert_equal(xt_csc.shape, (self.n, self.m))
        if verbose: print("xt_csc.shape = {}".format(xt_csc.shape))

        npt.assert_equal(xt_csc.data, self.x_data)
        if verbose: print("xt_csc.data = {}".format(xt_csc.data))

        npt.assert_equal(xt_csc.indices, self.x_indices)
        if verbose: print("xt_csc.indices = {}".format(x_csr.indices))

        npt.assert_equal(xt_csc.indptr, self.x_indptr)
        if verbose: print("xt_csc.indptr = {}".format(xt_csc.indptr))

        npt.assert_equal(xt_csc.dtype, self.x_dtype)
        if verbose: print("xt_csc.dtype = {}".format(xt_csc.dtype))

        npt.assert_equal(xt_csc.nnz, self.x_nnz)
        if verbose: print("xt_csc.nnz = {}".format(xt_csc.nnz))

        npt.assert_equal(xt_csc.ndim, 2)
        if verbose: print("xt_csc.ndim = {}".format(xt_csc.ndim))

        # Verify that the transposed matrix, converted to CSR is well formed.
        xt_csr = csr_matrix(xt_csc)
        
        npt.assert_equal(xt_csr.shape, (self.n, self.m))
        if verbose: print("xt_csr.shape = {}".format(xt_csr.shape))

        npt.assert_equal(xt_csr.data, self.xt_data)
        if verbose: print("xt_csr.data = {}".format(xt_csr.data))

        npt.assert_equal(xt_csr.indices, self.xt_indices)
        if verbose: print("xt_csr.indices = {}".format(xt_csr.indices))

        npt.assert_equal(xt_csr.indptr, self.xt_indptr)
        if verbose: print("xt_csr.indptr = {}".format(xt_csr.indptr))

        npt.assert_equal(xt_csr.dtype, self.x_dtype)
        if verbose: print("xt_csr.dtype = {}".format(xt_csr.dtype))

        npt.assert_equal(xt_csr.nnz, self.x_nnz)
        if verbose: print("xt_csr.nnz = {}".format(xt_csr.nnz))

        npt.assert_equal(xt_csr.ndim, 2)
        if verbose: print("xt_csr.ndim = {}".format(xt_csr.ndim))

    def test_06_transform_to_tensor(self):
        
        verbose = False

        x_tensor = torch.from_numpy(self.x)

        npt.assert_equal(list(x_tensor.size()), [self.m, self.n])
        if verbose: print("x_tensor.size() = {}".format(x_tensor.size()))

        npt.assert_equal(x_tensor.dtype, torch.float32)
        if verbose: print("x_tensor.dtype = {}".format(x_tensor.dtype))

        npt.assert_equal(x_tensor.device, torch.device(type='cpu'))
        if verbose: print("x_tensor.device = {}".format(x_tensor.device))

        npt.assert_equal(x_tensor.ndim, 2)
        if verbose: print("x_tensor.ndim = {}".format(x_tensor.ndim))

        npt.assert_equal(x_tensor.numel(), 24)
        if verbose: print("x_tensor.numel() = {}".format(x_tensor.numel()))

        npt.assert_equal(x_tensor.requires_grad, False)
        if verbose: print("x_tensor.requires_grad = {}".format(x_tensor.requires_grad))
        
        w_tensor = torch.from_numpy(self.w)

        npt.assert_equal(list(w_tensor.size()), [self.n])
        if verbose: print("w_tensor.size() = {}".format(w_tensor.size()))

        npt.assert_equal(w_tensor.dtype, torch.float32)
        if verbose: print("w_tensor.dtype = {}".format(w_tensor.dtype))

        npt.assert_equal(w_tensor.device, torch.device(type='cpu'))
        if verbose: print("w_tensor.device = {}".format(w_tensor.device))

        npt.assert_equal(w_tensor.ndim, 1)
        if verbose: print("w_tensor.ndim = {}".format(w_tensor.ndim))

        npt.assert_equal(w_tensor.numel(), 3)
        if verbose: print("w_tensor.numel() = {}".format(w_tensor.numel()))

        npt.assert_equal(w_tensor.requires_grad, False)
        if verbose: print("w_tensor.requires_grad = {}".format(w_tensor.requires_grad))
        

    def test_07_multiply_tensor(self):
        
        verbose = False

        x = torch.from_numpy(self.x)
        w = torch.from_numpy(self.w)

        lp = torch.mv(x, w)

        npt.assert_equal(list(lp.size()), [self.m])
        if verbose: print("lp.size() = {}".format(lp.size()))

        npt.assert_equal(lp.dtype, torch.float32)
        if verbose: print("lp.dtype = {}".format(lp.dtype))

        npt.assert_equal(lp.device, torch.device(type='cpu'))
        if verbose: print("lp.device = {}".format(lp.device))

        npt.assert_equal(lp.ndim, 1)
        if verbose: print("lp.ndim = {}".format(lp.ndim))

        npt.assert_equal(lp.numel(), 8)
        if verbose: print("lp.numel() = {}".format(lp.numel()))

        npt.assert_equal(lp.requires_grad, False)
        if verbose: print("lp.requires_grad = {}".format(lp.requires_grad))

        actual = lp.numpy()
        desired = self.lp
        npt.assert_array_equal(actual, desired)

    def test_08_transpose_tensor(self):

        verbose = False

        x = torch.from_numpy(self.x)
        xt = x.transpose(0, 1)

        npt.assert_equal(list(xt.size()), [self.n, self.m])
        if verbose: print("xt.size() = {}".format(xt.size()))

        npt.assert_equal(xt.dtype, torch.float32)
        if verbose: print("xt.dtype = {}".format(xt.dtype))

        npt.assert_equal(xt.device, torch.device(type='cpu'))
        if verbose: print("xt.device = {}".format(xt.device))

        npt.assert_equal(xt.ndim, 2)
        if verbose: print("xt.ndim = {}".format(xt.ndim))

        npt.assert_equal(xt.numel(), 24)
        if verbose: print("xt.numel() = {}".format(xt.numel()))

        npt.assert_equal(xt.requires_grad, False)
        if verbose: print("xt.requires_grad = {}".format(xt.requires_grad))
        
        actual = xt.numpy()
        desired = self.xt
        npt.assert_array_equal(actual, desired)

        z = torch.from_numpy(self.z)
        
        g = torch.mv(xt, z)
        
        actual = g.numpy()
        desired = self.g
        npt.assert_array_equal(actual, desired)

    def test_09_tensor_on_gpu(self):

        verbose = False

        cuda_found = torch.cuda.is_available()
        if verbose: print("CUDA/GPU found: {}".format(cuda_found))

        if cuda_found:
            device = 'cuda'
            x = torch.from_numpy(self.x).to(device)
            w = torch.from_numpy(self.w).to(device)

            lp = torch.mv(x, w)

            npt.assert_equal(list(x.size()), [self.m, self.n])
            if verbose: print("x.size() = {}".format(x.size()))

            npt.assert_equal(x.dtype, torch.float32)
            if verbose: print("x.dtype = {}".format(x.dtype))

            npt.assert_equal(x.device, torch.device(type='cuda', index=0))
            if verbose: print("x.device = {}".format(x.device))

            npt.assert_equal(x.ndim, 2)
            if verbose: print("x.ndim = {}".format(x.ndim))

            npt.assert_equal(x.numel(), 24)
            if verbose: print("x.numel() = {}".format(x.numel()))

            npt.assert_equal(x.requires_grad, False)
            if verbose: print("x.requires_grad = {}".format(x.requires_grad))

            npt.assert_equal(list(w.size()), [self.n])
            if verbose: print("w.size() = {}".format(w.size()))

            npt.assert_equal(w.dtype, torch.float32)
            if verbose: print("w.dtype = {}".format(w.dtype))

            npt.assert_equal(w.device, torch.device(type='cuda', index=0))
            if verbose: print("w.device = {}".format(w.device))

            npt.assert_equal(w.ndim, 1)
            if verbose: print("w.ndim = {}".format(w.ndim))

            npt.assert_equal(w.numel(), 3)
            if verbose: print("w.numel() = {}".format(w.numel()))

            npt.assert_equal(w.requires_grad, False)
            if verbose: print("w.requires_grad = {}".format(w.requires_grad))

            npt.assert_equal(list(lp.size()), [self.m])
            if verbose: print("lp.size() = {}".format(lp.size()))

            npt.assert_equal(lp.dtype, torch.float32)
            if verbose: print("lp.dtype = {}".format(lp.dtype))

            npt.assert_equal(lp.device, torch.device(type='cuda', index=0))
            if verbose: print("lp.device = {}".format(lp.device))

            npt.assert_equal(lp.ndim, 1)
            if verbose: print("lp.ndim = {}".format(lp.ndim))

            npt.assert_equal(lp.numel(), 8)
            if verbose: print("lp.numel() = {}".format(lp.numel()))

            npt.assert_equal(lp.requires_grad, False)
            if verbose: print("lp.requires_grad = {}".format(lp.requires_grad))

            actual = lp.cpu().numpy()
            desired = self.lp
            npt.assert_array_equal(actual, desired)
                        
    def test_10_csr_tensor_on_gpu(self):

        verbose = False

        cuda_found = torch.cuda.is_available()
        if verbose: print("CUDA/GPU found: {}".format(cuda_found))

        if cuda_found:
            device = 'cuda'
            tdevice = torch.device('cuda')
            x_csr = csr_matrix(self.x)
            x = rtu.csr_matrix_scipy_to_torch(x_csr, dev=tdevice)

            npt.assert_equal(list(x.size()), [self.m, self.n])
            if verbose: print("x.size() = {}".format(x.size()))

            npt.assert_equal(x.dtype, torch.float32)
            if verbose: print("x.dtype = {}".format(x.dtype))

            npt.assert_equal(x.device, torch.device(type='cuda', index=0))
            if verbose: print("x.device = {}".format(x.device))

            npt.assert_equal(x.ndim, 2)
            if verbose: print("x.ndim = {}".format(x.ndim))

            npt.assert_equal(x.numel(), self.m * self.n)
            if verbose: print("x.numel() = {}".format(x.numel()))

            npt.assert_equal(x.requires_grad, False)
            if verbose: print("x.requires_grad = {}".format(x.requires_grad))

            npt.assert_equal(x.layout, torch.sparse_csr)
            if verbose: print("x.layout = {}".format(x.layout))
            
            w = torch.from_numpy(self.w).to(device)

            npt.assert_equal(list(w.size()), [self.n])
            if verbose: print("w.size() = {}".format(w.size()))

            npt.assert_equal(w.dtype, torch.float32)
            if verbose: print("w.dtype = {}".format(w.dtype))

            npt.assert_equal(w.device, torch.device(type='cuda', index=0))
            if verbose: print("w.device = {}".format(w.device))

            npt.assert_equal(w.ndim, 1)
            if verbose: print("w.ndim = {}".format(w.ndim))

            npt.assert_equal(w.numel(), self.n)
            if verbose: print("w.numel() = {}".format(w.numel()))

            npt.assert_equal(w.requires_grad, False)
            if verbose: print("w.requires_grad = {}".format(w.requires_grad))

            lp = torch.matmul(x, w)

            npt.assert_equal(list(lp.size()), [self.m])
            if verbose: print("lp.size() = {}".format(lp.size()))

            npt.assert_equal(lp.dtype, torch.float32)
            if verbose: print("lp.dtype = {}".format(lp.dtype))

            npt.assert_equal(lp.device, torch.device(type='cuda', index=0))
            if verbose: print("lp.device = {}".format(lp.device))

            npt.assert_equal(lp.ndim, 1)
            if verbose: print("lp.ndim = {}".format(lp.ndim))

            npt.assert_equal(lp.numel(), self.m)
            if verbose: print("lp.numel() = {}".format(lp.numel()))

            npt.assert_equal(lp.requires_grad, False)
            if verbose: print("lp.requires_grad = {}".format(lp.requires_grad))
            
            actual = lp.cpu().numpy()
            desired = self.lp
            npt.assert_array_equal(actual, desired)

    def test_20_slice_algo_memo(self):

        cuda_found = torch.cuda.is_available()

        if cuda_found:
            device = 'cuda'
            x_csr = csr_matrix(self.x)
            x = rtu.csr_matrix_scipy_to_torch(x_csr)
            w = torch.from_numpy(self.w).to(device)

            length = x.size(0)
            for block_size in range(1, length + 2):
                xblocks = rtu.precompute_csr_slices(x, block_size)
                lp = rtu.matmul_by_slices(x, w, xblocks, device, torch.device('cpu'))
                actual = lp.numpy()
                desired = self.lp
                npt.assert_array_equal(actual, desired)
