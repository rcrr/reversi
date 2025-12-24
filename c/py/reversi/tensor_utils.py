#
# tensor_utils.py
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

import numpy as np
from scipy.sparse import csr_matrix

import torch
import warnings

from typing import Any, List, Tuple

__all__ = ['csr_matrix_scipy_to_torch',
           'precompute_csr_slices',
           'matmul_by_slices']


"""
Module: reversi.tensor_utils
Provides utilities for processing CSR sparse tensors with PyTorch.

Functions:
- csr_matrix_scipy_to_torch: Convert scipy CSR matrix to PyTorch sparse CSR tensor.
- precompute_csr_slices: Precompute row slice info for block processing.
- matmul_by_slices: Multiply CSR tensor with dense tensor in blocks.

Usage:
import reversi.tensor_utils as rtu
m = rtu.csr_matrix_scipy_to_torch(m_csr)
mblocks = rtu.precompute_csr_slices(m, block_size)
r = rtu.matmul_by_slices(m, a, mblocks, 'cuda')
"""


def csr_matrix_scipy_to_torch(csr_mat: csr_matrix,
                              dt: torch.dtype =torch.float32,
                              dev: torch.device =torch.device('cpu')) -> torch.Tensor:
    """
    Creates PyTorch CSR tensor using scipy CSR components.
    """
    warnings.filterwarnings('ignore', '.*Sparse CSR tensor support is in beta state.*')
    crow_indices = torch.tensor(csr_mat.indptr, dtype=torch.int64)
    col_indices = torch.tensor(csr_mat.indices, dtype=torch.int64)
    values = torch.tensor(csr_mat.data, dtype=dt)
    shape = csr_mat.shape
    csr_tensor = torch.sparse_csr_tensor(crow_indices, col_indices, values, size=shape, dtype=dt, device=dev)
    return csr_tensor


def precompute_csr_slices_by_mem_usage(csr_tensor: torch.Tensor,
                                       block_mem_size: int,
                                       verbose: bool =False) -> List[Tuple[int, int, torch.Tensor, Tuple[int, int], int, int]]:
    """
    Prepares and returns a list of slice_info tuples.
    Each slice_info embeds the information needed to build the corresponding slice of the CSR tensor.
    The slice_info tuple is used as argument for the call to _csr_tensor_slice function.
    Slices are prepared collecting a variable count of rows, rows are added to each slice until the size of the slice is not
    reaching the given block_mem_size limit.
    If one or more rows are using more memory than the block_mem_size limit, the function raises a runtime error.

    Returns:
      List of tuples with:
      - start_idx (int): starting index in col_indices/values for the block
      - end_idx (int): ending index (exclusive)
      - crow_slice_adjusted (torch.Tensor): adjusted crow_indices for block starting at 0
      - new_shape (Tuple[int,int]): shape of block tensor (rows, cols)
      - start_row (int): starting row index of block
      - end_row (int): ending row index (exclusive) of block
    """
    assert isinstance(csr_tensor, torch.Tensor), f"Expected torch.Tensor, got {type(csr_tensor)}"
    assert csr_tensor.layout == torch.sparse_csr, "Tensor must have CSR layout"
    assert isinstance(block_mem_size, int), f"Argument block_size  must be int, got {type(block_mem_size)}"
    assert block_mem_size > 0, f"Argument block_size  must be greather than zero, got {block_mem_size}"
    crow_indices = csr_tensor.crow_indices()
    col_indices = csr_tensor.col_indices()
    values = csr_tensor.values()
    row_n = csr_tensor.size(0)
    bytes_per_element_cri = crow_indices.element_size()
    bytes_per_element_coi = col_indices.element_size()
    bytes_per_element_val = values.element_size()
    counts_per_row = crow_indices[1:] - crow_indices[:-1]
    element_n = crow_indices[-1].item()
    mem_usage_per_row = counts_per_row * (bytes_per_element_coi + bytes_per_element_val)
    max_counts_per_row = max(counts_per_row)
    max_mem_usage_per_row = max(mem_usage_per_row)
    len_crow_indices = len(crow_indices)
    csr_tensor_mem_usage = bytes_per_element_cri * (row_n + 1) + (bytes_per_element_coi + bytes_per_element_val) * element_n
    if verbose:
        print("precompute_csr_slices_by_mem_usage: csr_tensor.size()={}, element_n={}, block_mem_size={}".format(csr_tensor.size(), element_n, block_mem_size))
        print(" CSR tensor, crow_indices: size={}, bytes_per_element={}, mem_usage={}".format(len_crow_indices, bytes_per_element_cri, len_crow_indices * bytes_per_element_cri))
        print(" CSR tensor, col_indices: size={}, bytes_per_element={}, mem_usage={}".format(element_n, bytes_per_element_coi, element_n * bytes_per_element_coi))
        print(" CSR tensor, values: size={}, bytes_per_element={}, mem_usage={}".format(element_n, bytes_per_element_val, element_n * bytes_per_element_val))
        print(" CSR tensor, total mem usage = {} ({:.2f} kB, {:.2f} MB)".format(csr_tensor_mem_usage, csr_tensor_mem_usage / 1024, csr_tensor_mem_usage / (1024 * 1024)))
        print(" CSR tensor, max_counts_per_row = {}, largest row mem consumption = {} ({:.2f} kB, {:.2f} MB)".format(max_counts_per_row, max_mem_usage_per_row,
                                                                                                                     max_mem_usage_per_row / 1024,
                                                                                                                     max_mem_usage_per_row / (1024 * 1024)))
    if (max_mem_usage_per_row > block_mem_size):
        print("max_mem_usage_per_row = {}, block_mem_size = {}".format(max_mem_usage_per_row, block_mem_size))
        raise RuntimeError("max_mem_usage_per_row is greater than block_mem_size, partitioning the CSR table is not possible.")

    slices = []
    curr_start_row = 0
    while curr_start_row < row_n:
        curr_end_row = curr_start_row
        # current memory usage for this slice, start with zero
        curr_mem_usage = 0
        while curr_end_row < row_n:
            row_mem = mem_usage_per_row[curr_end_row].item()
            # if adding this row exceeds block size, stop expanding slice
            if (curr_mem_usage + row_mem) > block_mem_size:
                break
            curr_mem_usage += row_mem
            curr_end_row += 1
        # if no rows can fit, raise the error
        if curr_end_row == curr_start_row:
            raise RuntimeError(f"Single row memory usage {mem_usage_per_row[curr_start_row].item()} exceeds block_mem_size {block_mem_size}")

        # Prepare crow slice for the current rows
        crow_slice = crow_indices[curr_start_row:(curr_end_row + 1)]
        start_idx = crow_slice[0].item()
        end_idx = crow_slice[-1].item()
        crow_slice_adjusted = crow_slice - crow_slice[0]
        new_shape = (curr_end_row - curr_start_row, csr_tensor.size(1))
        slices.append((start_idx, end_idx, crow_slice_adjusted, new_shape, curr_start_row, curr_end_row))
        # Move to the next slice starting point
        curr_start_row = curr_end_row

    return slices

def precompute_csr_slices(csr_tensor: torch.Tensor,
                          block_size: int) -> List[Tuple[int, int, torch.Tensor, Tuple[int, int], int, int]]:
    """
    Prepares and returns a list of slice_info tuples.
    Each slice_info embeds the information needed to build the corresponding slice of the CSR tensor.
    The slice_info tuple is used as argument for the call to _csr_tensor_slice function.

    Returns:
      List of tuples with:
      - start_idx (int): starting index in col_indices/values for the block
      - end_idx (int): ending index (exclusive)
      - crow_slice_adjusted (torch.Tensor): adjusted crow_indices for block starting at 0
      - new_shape (Tuple[int,int]): shape of block tensor (rows, cols)
      - start_row (int): starting row index of block
      - end_row (int): ending row index (exclusive) of block
    """
    assert isinstance(csr_tensor, torch.Tensor), f"Expected torch.Tensor, got {type(csr_tensor)}"
    assert csr_tensor.layout == torch.sparse_csr, "Tensor must have CSR layout"
    assert isinstance(block_size, int), f"Argument block_size  must be int, got {type(block_size)}"
    assert block_size > 0, f"Argument block_size  must be greather than zero, got {block_size}"
    crow = csr_tensor.crow_indices()
    nrows = csr_tensor.size(0)
    slices = []
    for start_row in range(0, nrows, block_size):
        end_row = min(start_row + block_size, nrows)
        crow_slice = crow[start_row:(end_row+1)]
        start_idx = crow_slice[0].item()
        end_idx = crow_slice[-1].item()
        crow_slice_adjusted = crow_slice - crow_slice[0]
        new_shape = (end_row - start_row, csr_tensor.size(1))
        slices.append((start_idx, end_idx, crow_slice_adjusted, new_shape, start_row, end_row))
    return slices


def _csr_tensor_slice(csr_tensor: torch.Tensor,
                      slice_info: Tuple[int, int, torch.Tensor, Tuple[int, int], int, int]) -> torch.Tensor:
    """
    Returns a slice of the PyTorch CSR tensor accordingly with the slice_info argument.
    """
    assert isinstance(csr_tensor, torch.Tensor), f"Expected torch.Tensor, got {type(csr_tensor)}"
    assert csr_tensor.layout == torch.sparse_csr, "Tensor must have CSR layout"
    assert isinstance(slice_info, tuple), f"Expected tuple, got {type(slice_info)}"
    assert len(slice_info) == 6, f"Expected tuple of length 6, got length {len(slice_info)}"
    assert isinstance(slice_info[0], int), f"First element of slice_info must be int, got {type(slice_info[0])}"
    assert isinstance(slice_info[1], int), f"Second element of slice_info must be int, got {type(slice_info[1])}"
    assert isinstance(slice_info[2], torch.Tensor), f"Third  element of slice_info must be torch.Tensor, got {type(slice_info[2])}"
    assert isinstance(slice_info[3], tuple), f"Third  element of slice_info must be tuple, got {type(slice_info[3])}"
    assert len(slice_info[3]) == 2, f"Expected tuple of length 2, got length {len(slice_info[3])}"
    assert isinstance(slice_info[3][0], int), f"First element of slice_info[3] must be int, got {type(slice_info[3][0])}"
    assert isinstance(slice_info[3][1], int), f"Second element of slice_info[3] must be int, got {type(slice_info[3][1])}"
    start_idx, end_idx, crow_slice_adjusted, new_shape, _, _ = slice_info
    col = csr_tensor.col_indices()
    values = csr_tensor.values()
    col_slice = col[start_idx:end_idx]
    values_slice = values[start_idx:end_idx]
    csr_slice = torch.sparse_csr_tensor(crow_slice_adjusted, col_slice, values_slice, size=new_shape, device='cpu')
    return csr_slice

def matmul_by_slices(m: torch.Tensor,
                     a: torch.Tensor,
                     slices: List[Tuple[int, int, torch.Tensor, Tuple[int, int], int, int]],
                     device: torch.device,
                     ret_device: torch.device) -> torch.Tensor:
    """
    Executes the matrix multiplication R = M @ A where M is a PyTorch CSR matrix, and A is a PyTorch array.
    Being M and A PyTorch Tensors. A CSR Matrix and an Array repectively.
    Tensor M must be hosted on CPU.
    Tensor A must be hosted on device.
    The returned tensor R is hosted on ret_device.
    """
    length = m.size(0)
    r = torch.empty(length, dtype=torch.float32, device=ret_device)
    for slice_info in slices:
        start_idx, end_idx, crow_slice_adjusted, new_shape, start_row, end_row = slice_info
        m_block = _csr_tensor_slice(m, slice_info)
        m_block_on_device = m_block.to(device, non_blocking=True)
        r_block_on_device = torch.matmul(m_block_on_device, a)
        r[start_row:end_row] = r_block_on_device.to(ret_device)
    return r
