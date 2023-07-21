#
# rglm.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022, 2023 Roberto Corradini. All rights reserved.
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

from reversi.board import *
from reversi.pattern import *
from reversi.cfg import *
from reversi.regab import *

import numpy as np
import pandas as pd

from scipy.sparse import csr_matrix
from scipy.optimize import minimize
import datetime

import pickle

import ctypes as ct

import inspect

# time_t is defined as int64_t in the C library.
c_time_t = ct.c_uint64

# rglmdf_file_data_format_type_t is an enum in the C source code (int).
c_rglmdf_file_data_format_type_t = ct.c_int
c_rglmdf_file_data_format_type_is_general = 0
c_rglmdf_file_data_format_type_is_positions = 1

# board_feature_id_t is an enum in the C source code (int).
c_board_feature_id_t = ct.c_int

# board_pattern_id_t is an enum in the C source code (int).
c_board_pattern_id_t = ct.c_int

# see RGLMDF_POSITION_STATUS_BUF_SIZE macro into C source code.
# The two values must be equal.
c_rglmdf_position_status_buf_size = 4


#
# To do:
#
# -00- [done] Implement the Ridge regularization.
#
# -01- Add one more Feature ( MOBILITYX ) and one more Pattern : 2X6COR having 12 squares.
#      [done] Added new pattern 2X6COR
#
# -02- Introduce the Lasso regularization.
#      Here we do not have a solution yet, options are:
#      . Use the (Orthant-Wise Limited-memory Quasi-Newton (OWL-QN) method: https://www.chokkan.org/software/liblbfgs/
#      . "double the variables" and use the constraint version L-BFGS-C of the algorithm
#      . Try something more fancy ...
#
# -03- More data analytics.
#      [done] Parameters profiling ... each variable/weight should have min, max, mean, std game values.
#      [done] There should be a second data set for validation.
#         Actions:
#         . Add the vid column to evmap, could we name it evid (expanded variable id) to make it clear it is a different thing. 
#         . Add e new function compute_vld_gpxpidf
#         . Add a new function compute_vld_x
#         . Finally compute logit(vld_x @ ew) as the vld_yh. Compare it with vld_y ...
#      There should be a final report ... with KPI used to compare different models.
#      Prepare an info(verbosity) method that give back Model Info.
#
# -04- [done] Setup a kind of workflow.
#             rglm_test() should be renamed to something like ... execute work-flow
#             input data should be provided by means of a dictionary
#
# -05- [done] Read and write to file ...
#             In practice we need to build the REGAB/RGML machinery in a different way.
#
# -06- Complete documentation ....
#
# -07- Code tests ....
#
# -08- [done] Read/Write the RGLM Model Weights file format ...
#      - read works
#      - remember to free the mw helper object !!!
#      - reorganize he methods as _CT class methods ...
#
# -09- There is a missing piece of code:
#      The RGLM Model file ( C format, as written by the rglm executable ) must be saved also by the PYTHON code.
#      [done] READ the C binary format, and load it as the Rgml python object
#      WRITE the C binary format, converting the Rgml python object.
#
# -10- Add a few new ideas of PATTERNS, without having to build the REGAB database classified game positions.
#
#

#
# Tested hash function ... gives same result as the libreversi C version ...
#
#def sha256sum(filename):
#    h  = hashlib.sha3_256()
#    b  = bytearray(128*1024)
#    mv = memoryview(b)
#    with open(filename, 'rb', buffering=0) as f:
#        while n := f.readinto(mv):
#            h.update(mv[:n])
#            return h.hexdigest()
#

class _RglmWeightRecordCTHelper(ct.Structure):
    """
    This is the record definition of the weights array.
    """
    
    _fields_ = [
        ("entity_class", ct.c_int16),
        ("entity_id", ct.c_int16),
        ("index_value", ct.c_int32),
        ("principal_index_value", ct.c_int32),
        ("glm_variable_id", ct.c_int32),
        ("total_cnt", ct.c_int64),
        ("relative_frequency", ct.c_double),
        ("theoretical_probability", ct.c_double),
        ("weight", ct.c_double),
    ]
    
class _RglmModelWeightsCTHelper(ct.Structure):
    """
    Load and store the rglmdf_model_weights_t C data structure as defined in the
    rglm_data_files.h C header file.
    """
    
    _fields_ = [
        ("file_creation_time", ct.c_uint64),
        ("general_data_checksum", ct.c_char_p),
        ("gp_sample_size", ct.c_int64),
        ("empty_count", ct.c_uint8),
        ("feature_cnt", ct.c_size_t),
        ("features", ct.c_void_p),
        ("pattern_cnt", ct.c_size_t),
        ("patterns", ct.c_void_p),
        ("weight_cnt", ct.c_size_t),
        ("weights", ct.c_void_p),
        ("reverse_map_mw_a", ct.c_void_p),
        ("reverse_map_mw_b", ct.c_void_p),
    ]

    def __init__(self):
        f = libreversi.rglmdf_model_weights_init
        f.restype = None
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper)]
        ct_rglmdf_model_weights_p = ct.byref(self)
        f(ct_rglmdf_model_weights_p)

    def __del__(self):
        f = libreversi.rglmdf_model_weights_release
        f.restype = None
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper)]
        f(ct.byref(self))
    
    def allocate_memory_for_arrays(self, feature_cnt : int, pattern_cnt : int, weight_cnt : int,
                                   feature_record_size : int, pattern_record_size : int, weight_record_size : int):
        f = libreversi.rglmdf_model_weights_allocate_memory
        f.restype = ct.c_int
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper),
                      ct.c_size_t, ct.c_size_t, ct.c_size_t,
                      ct.c_size_t, ct.c_size_t, ct.c_size_t]
        ct_rglmdf_model_weights_p = ct.byref(self)
        ret = f(ct_rglmdf_model_weights_p, feature_cnt, pattern_cnt, weight_cnt,
                feature_record_size, pattern_record_size, weight_record_size)
        if ret != 0:
            raise Exception('Return code is not zero')
        
    def print_summary(self):
        f = libreversi.rglmdf_model_weights_summary_to_stream
        f.restype = None
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper), cstdout]
        f(self, cstdout)
        
    def load(self, filename : str, verbose : bool, check_digest : bool):
        f = libreversi.rglmdf_model_weights_read_from_binary_file
        f.restype = ct.c_int
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper), ct.c_char_p, ct.c_bool, ct.c_bool]
        ret = f(ct.byref(self), filename.encode('utf-8'), verbose, check_digest)
        if ret != 0:
            raise Exception('Return code is not zero')
        
    def write(self, filename, time=None):
        if time is None:
            dt = datetime.datetime.now()
            ts = datetime.datetime.timestamp(dt)
            time = int(ts)
        f = libreversi.rglmdf_model_weights_write_to_binary_file
        f.restype = ct.c_int
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper), ct.c_char_p, ct.c_uint64]
        ret = f(ct.byref(self), filename.encode('utf-8'), time)
        if ret != 0:
            raise Exception('Return code is not zero')

class RglmModelWeights:
    """
    RGLM Model Weight class collects the weight array for a given model.
    Most of the work is done by the C implementation.
    """
    def __init__(self):
        self._CTHelper = _RglmModelWeightsCTHelper()

    def __del__(self):
        self._CTHelper = None

    def allocate_memory_for_arrays(self, feature_cnt : int, pattern_cnt : int, weight_cnt : int,
                                   feature_record_size : int, pattern_record_size : int, weight_record_size : int):
        self._CTHelper.allocate_memory_for_arrays(feature_cnt, pattern_cnt, weight_cnt,
                                                  feature_record_size, pattern_record_size, weight_record_size)
    
    def load(self, filename : str, verbose : bool, check_digest : bool):
        self._CTHelper.load(filename, verbose, check_digest)

    def write(self, filename, time=None):
        self._CTHelper.write(filename, time)

    def print_summary(self):
        self._CTHelper.print_summary()

    def get_features_as_numpy_array(self):
        features_p = ct.cast(self._CTHelper.features, ct.POINTER(ct.c_int32))
        features = np.ctypeslib.as_array(features_p, shape=(self._CTHelper.feature_cnt,))
        return features

    def get_patterns_as_numpy_array(self):
        patterns_p = ct.cast(self._CTHelper.patterns, ct.POINTER(ct.c_int32))
        patterns = np.ctypeslib.as_array(patterns_p, shape=(self._CTHelper.pattern_cnt,))
        return patterns

    def get_weights_as_numpy_array(self):
        weights_p = ct.cast(self._CTHelper.weights, ct.POINTER(reversi.rglm._RglmWeightRecordCTHelper))
        weights = np.ctypeslib.as_array(weights_p, shape=(self._CTHelper.weight_cnt,))
        return weights

# --- --- --- --- --- --- --- --- --- #

class _RglmdfSolvedAndClassifiedGpRecord(ct.Structure):
    """
    Reversi GLM data file record definition for the solved and classified game position table.
    """
    _fields_ = [
        ("row_n", ct.c_int64),
        ("gp_id", ct.c_int64),
        ("mover", ct.c_int64),
        ("opponent", ct.c_int64),
        ("game_value", ct.c_int8),
        ("game_value_transformed", ct.c_float),
        ("evaluation_function", ct.c_float),
        ("residual", ct.c_float),
    ]

class _RglmdfSolvedAndClassifiedGpTable(ct.Structure):
    """
    Reversi GLM data file table holding the game positons being solved and classified.
    """
    _fields_ = [
        ("ntuples", ct.c_size_t),
        ("n_fvalues_per_record", ct.c_size_t),
        ("n_index_values_per_record", ct.c_size_t),
        ("records", ct.POINTER(_RglmdfSolvedAndClassifiedGpRecord)),
        ("farray", ct.POINTER(ct.c_double)),
        ("i0array", ct.POINTER(ct.c_int32)),
        ("i1array", ct.POINTER(ct.c_int32)),
        ("i2array", ct.POINTER(ct.c_int32)),
    ]

class _RglmdfEntityFreqSummaryRecord(ct.Structure):
    """
    Reversi GLM data file record definition for the entity frequency summary table.
    """
    _fields_ = [
        ("glm_variable_id", ct.c_int32),
        ("entity_class", ct.c_int16),
        ("entity_id", ct.c_int16),
        ("principal_index_value", ct.c_int32),
        ("total_cnt", ct.c_int64),
        ("relative_frequency", ct.c_double),
        ("theoretical_probability", ct.c_double),
        ("weight", ct.c_double),
    ]

class _RglmdfEntityFreqSummaryTable(ct.Structure):
    """
    Reversi GLM data file table holding the summary of entity frequencies.
    The table contains the count of feature/pattern occurrencies grouped by (entity_class, entity_id, principal_index_value).
    The value of 'ntuples' must be equal to the sum of 'glm_f_variable_cnt' and 'glm_p_variable_cnt'.
    """
    _fields_ = [
        ("glm_f_variable_cnt", ct.c_size_t),
        ("glm_p_variable_cnt", ct.c_size_t),
        ("ntuples", ct.c_size_t),
        ("records", ct.POINTER(_RglmdfEntityFreqSummaryRecord)),
    ]

class _RglmdfPositionSummaryRecord(ct.Structure):
    """
    Reversi GLM data file record definition for the position summary table.
    """
    _fields_ = [
        ("batch_id", ct.c_int32),
        ("status", ct.c_char*c_rglmdf_position_status_buf_size),
        ("game_position_cnt", ct.c_int64),
        ("classified_cnt", ct.c_int64),
    ]
    
class _RglmdfPositionSummaryTable(ct.Structure):
    """
    Reversi GLM data file table holding the summary of game positions.
    The table contains the count of game positions grouped by batch_is and status.
    """
    _fields_ = [
        ("ntuples", ct.c_size_t),
        ("records", ct.POINTER(_RglmdfPositionSummaryRecord)),
    ]

class _RglmdfGeneralDataCTHelper(ct.Structure):
    """
    Reversi GLM data file general data structure.
    The type contains all the info saved/retrieved from a RGLM Data File.
    """
    _fields_ = [
        ("file_digest", ct.c_char_p),
        ("file_creation_time", c_time_t),
        ("format", c_rglmdf_file_data_format_type_t),
        ("batch_id_cnt", ct.c_size_t),
        ("batch_ids", ct.POINTER(ct.c_uint64)),
        ("empty_count", ct.c_uint8),
        ("position_status_cnt", ct.c_size_t),
        ("position_status_buffer", ct.c_char_p),
        ("position_statuses", ct.POINTER(ct.c_char_p)),
        ("feature_cnt", ct.c_size_t),
        ("features", ct.POINTER(c_board_feature_id_t)),
        ("pattern_cnt", ct.c_size_t),
        ("patterns", ct.POINTER(c_board_pattern_id_t)),
        ("position_summary", _RglmdfPositionSummaryTable),
        ("entity_freq_summary", _RglmdfEntityFreqSummaryTable),
        ("positions", _RglmdfSolvedAndClassifiedGpTable),
        ("reverse_map_a_f", ct.POINTER(ct.POINTER(ct.c_int32))),
        ("reverse_map_a_p", ct.POINTER(ct.POINTER(ct.c_int32))),
        ("reverse_map_b", ct.POINTER(ct.c_int32)),
    ]

    def __init__(self):
        f = libreversi.rglmdf_general_data_init
        f.restype = None
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper)]
        ct_rglmdf_general_data_p = ct.byref(self)
        f(ct_rglmdf_general_data_p)
        
    def __del__(self):
        f = libreversi.rglmdf_general_data_release
        f.restype = None
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper)]
        f(ct.byref(self))

    def read_from_binary_file(self, filename : str, verbose : bool):
        f = libreversi.rglmdf_read_general_data_from_binary_file
        f.restype = ct.c_int
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.c_char_p, ct.c_bool]
        ret = f(ct.byref(self), filename.encode('utf-8'), verbose)
        if ret != 0:
            raise Exception('Return code is not zero')

    def write_to_binary_file(self, filename : str, time=None):
        if time is None:
            dt = datetime.datetime.now()
            ts = datetime.datetime.timestamp(dt)
            time = int(ts)
        f = libreversi.rglmdf_write_general_data_to_binary_file
        f.restype = ct.c_int
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.c_char_p, c_time_t]
        ret = f(ct.byref(self), filename.encode('utf-8'), time)
        if ret != 0:
            raise Exception('Return code is not zero')

    def set_batch_ids(self, batch_ids : list):
        if not isinstance(batch_ids, list):
            raise TypeError('Argument batch_ids is not an instance of list')
        if not all([isinstance(e, int) for e in batch_ids]):
            raise TypeError('Argument batch_ids must have all elements belonging to int type')
        if not all([e >= 0 for e in batch_ids]):
            raise TypeError('Argument batch_ids must have all elements being positive')

        cnt = len(batch_ids)
        arr_dtype = ct.c_uint64*cnt
        arr = arr_dtype()
        for i in range(cnt):
            arr[i] = batch_ids[i]
        
        f = libreversi.rglmdf_set_batch_ids
        f.restype = ct.c_size_t
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.POINTER(ct.c_uint64), ct.c_size_t]
        ret = f(ct.byref(self), arr, cnt)
        if ret != cnt:
            raise Exception('Return code is invalid')

    def set_position_statuses(self, position_statuses : list):
        if not isinstance(position_statuses, list):
            raise TypeError('Argument position_statuses is not an instance of list')
        if not all([isinstance(e, str) for e in position_statuses]):
            raise TypeError('Argument position_statuses must have all elements belonging to str type')
        if not all([len(e) == (c_rglmdf_position_status_buf_size - 1) for e in position_statuses]):
            raise TypeError('Argument position_statuses must have all elements haaving a given lenght')

        cnt = len(position_statuses)
        arr = (ct.c_char_p*cnt)()
        arr[:] = [s.encode('utf-8') for s in position_statuses]
        
        f = libreversi.rglmdf_set_position_statuses
        f.restype = ct.c_size_t
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.POINTER(ct.c_char_p), ct.c_size_t]
        ret = f(ct.byref(self), arr, cnt)
        if ret != cnt:
            raise Exception('Return code is invalid')

    def set_features(self, features : list):
        if not isinstance(features, list):
            raise TypeError('Argument features is not an instance of list')
        if not all([isinstance(e, Feature) for e in features]):
            raise TypeError('Argument features must have all elements belonging to Feature type')

        cnt = len(features)
        arr = (c_board_feature_id_t*cnt)()
        arr[:] = [f.id for f in features]

        f = libreversi.rglmdf_set_features
        f.restype = ct.c_size_t
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.POINTER(c_board_feature_id_t), ct.c_size_t]
        ret = f(ct.byref(self), arr, cnt)
        if ret != cnt:
            raise Exception('Return code is invalid')

    def set_patterns(self, patterns : list):
        if not isinstance(patterns, list):
            raise TypeError('Argument patterns is not an instance of list')
        if not all([isinstance(e, Pattern) for e in patterns]):
            raise TypeError('Argument patterns must have all elements belonging to Pattern type')

        cnt = len(patterns)
        arr = (c_board_pattern_id_t*cnt)()
        arr[:] = [f.id for f in patterns]

        f = libreversi.rglmdf_set_patterns
        f.restype = ct.c_size_t
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.POINTER(c_board_pattern_id_t), ct.c_size_t]
        ret = f(ct.byref(self), arr, cnt)
        if ret != cnt:
            raise Exception('Return code is invalid')


    def set_position_summary_table(self, position_summary_table : pd.DataFrame):
        t = position_summary_table
        t_len = len(t)
        record_size = ct.sizeof(_RglmdfPositionSummaryRecord)

        # Calling C function: rglmdf_set_position_summary_ntuples
        f = libreversi.rglmdf_set_position_summary_ntuples
        f.restype = ct.c_size_t
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.c_size_t]
        ret = f(ct.byref(self), t_len)
        if ret != t_len:
            raise Exception('Return code from function rglmdf_set_position_summary_ntuples is invalid')
        
        # psta: position summary table array
        psta = np.empty(t_len, dtype=[('batch_id', '<i4'),
                                      ('status', 'S4'),
                                      ('game_position_cnt', '<i8'),
                                      ('classified_cnt', '<i8')])
        
        if record_size != psta.itemsize:
            raise Exception('The record size of the summary table must be equal to the size of _RglmdfPositionSummaryRecord')

        # Loading the data into the numpy array.
        psta['batch_id'] = t['batch_id']
        psta['status'] = t['status']
        psta['game_position_cnt'] = t['count']
        psta['classified_cnt'] = t['count']

        # Transfering to the C object.
        ct.memmove(self.position_summary.records, psta.ctypes.data, t_len * record_size)

        
    def set_entity_freq_summary_table(self, entity_freq_summary_table : pd.DataFrame):
        
        t = entity_freq_summary_table
        if not isinstance(t, pd.DataFrame):
            raise Exception('The argument entity_freq_summary_table is not a DataFrame')

        required_columns = ['vid', 'etype']
        if not pd.Series(required_columns).isin(t.columns).all():
            raise Exception('The argument entity_freq_summary_table must have a list of required columns')
        
        ntuples = len(t)
        record_size = ct.sizeof(_RglmdfEntityFreqSummaryRecord)

        etype_feature = 0
        etype_pattern = 1

        df = t[['vid', 'etype']].groupby(['etype']).count().rename(columns= {'vid': 'count'})

        if sum(df.index == etype_feature) == 0:
            feature_ntuples = 0
        else:
            feature_ntuples = df.loc[etype_feature]['count']

        if sum(df.index == etype_pattern) == 0:
            pattern_ntuples = 0
        else:
            pattern_ntuples = df.loc[etype_pattern]['count']

        if ntuples != feature_ntuples + pattern_ntuples:
            raise Exception('There is a mismatch into entity_freq_summary_table')
            
        # extern size_t
        # rglmdf_set_entity_freq_summary_ntuples (rglmdf_general_data_t *gd,
        #                                         size_t feature_ntuples,
        #                                         size_t pattern_ntuples,
        #                                         size_t ntuples);
        
        # Calling C function: rglmdf_set_entity_freq_summary_ntuples
        f = libreversi.rglmdf_set_entity_freq_summary_ntuples
        f.restype = ct.c_size_t
        f.argtypes = [ct.POINTER(_RglmdfGeneralDataCTHelper), ct.c_size_t, ct.c_size_t, ct.c_size_t]
        ret = f(ct.byref(self), feature_ntuples, pattern_ntuples, ntuples)
        if ret != ntuples:
            raise Exception('Return code from function rglmdf_set_entity_freq_summary_ntuples is invalid')

        # >>> m.vmap
        #        vid  etype  eid  idx   count  min  max      mean        std  perc10  perc25  perc50  perc75  perc90    oprobs    eprobs    weight
        #        0        0      0    0    0  199932  -64   64 -1.391293  26.406980   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000 -0.652285
        #        1        1      0    3    0  199932  -64   64 -1.391293  26.406980   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000  3.754061
        #        2        2      0    3    1  199932  -64   64 -1.391293  26.406980   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000 -1.906323
        #        3        3      0    3    2  199932  -64   64 -1.391293  26.406980   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000  0.424061
        #        4        4      1    0    0   17912  -64   64 -2.076262  27.835930   -38.0   -22.0    -2.0    18.0    36.0  0.022398  0.020310 -0.104893
        #        ...    ...    ...  ...  ...     ...  ...  ...       ...        ...     ...     ...     ...     ...     ...       ...       ...       ...
        #        2980  2980      1   12   16   13435  -64   64  2.475921  25.261466   -30.0   -16.0     2.0    20.0    36.0  0.016799  0.017455 -0.042112
        #        2981  2981      1   12   17   43117  -64   64  1.432474  26.432125   -34.0   -18.0     2.0    22.0    36.0  0.053915  0.052857  0.046707
        #        2982  2982      1   12   20   15451  -64   64 -1.338425  26.634843   -36.0   -22.0    -2.0    18.0    36.0  0.019320  0.018632 -0.011399
        #        2983  2983      1   12   23   12698  -64   64 -5.289967  25.947034   -38.0   -24.0    -6.0    14.0    30.0  0.015878  0.015555 -0.181499
        #        2984  2984      1   12   26   43568  -64   64 -2.641710  27.245382   -38.0   -22.0    -2.0    18.0    34.0  0.054479  0.051893  0.116055
        #
        #        [2985 rows x 17 columns]
        #
        
        # efsta:  entity_freq_summary_table array
        efsta = np.empty(ntuples, dtype=np.dtype([('glm_variable_id', '<i4'),
                                                  ('entity_class', '<i2'),
                                                  ('entity_id', '<i2'),
                                                  ('principal_index_value', '<i4'),
                                                  ('total_cnt', '<i8'),
                                                  ('relative_frequency', '<d'),
                                                  ('theoretical_probability', '<d'),
                                                  ('weight', '<d')], align=True))
        
        if record_size != efsta.itemsize:
            raise Exception('The record size of the freq summary table must be equal to the size of _RglmdfEntityFreqSummaryRecord')

        # Loading the data into the numpy array.
        efsta['glm_variable_id'] = t['vid']
        efsta['entity_class'] = t['etype']
        efsta['entity_id'] = t['eid']
        efsta['principal_index_value'] = t['idx']
        efsta['total_cnt'] = t['count']
        efsta['relative_frequency'] = t['oprobs']
        efsta['theoretical_probability'] = t['eprobs']
        efsta['weight'] = t['weight']

        # Transfering to the C object.
        ct.memmove(self.entity_freq_summary.records, efsta.ctypes.data, ntuples * record_size)

        
    # HERE

        
class Rglm:
    """
    RGLM - Reversi Generalized Linear Model

    The class implements a full workflow starting from data selection up to model optimization.

    Steps are:

        - Create the Rglm object:
            m = Rglm()
        - Assign a database connection to the REGAB data base:
            m.set_conn(RegabDBConnection(dbname, user, host))
        - Set the query parameters values for empty count, batches and statuses:
            m.set_empty_count(20)
            m.set_batches(7)
            m.set_statuses('CMR,CMS')
        - Execute the query to retrieve the game positions from the REGAB data base:
            m.retrieve_game_positions()
          The query populates the game_position field with the resulting data frame
        - Select the model features and patterns:
            m.set_features('INTERCEPT,MOBILITY3')
            m.set_patterns('EDGE,DIAG8')
          The operations populate the fields:
          features, patterns, flabel_dict, plabel_dict_i0 and plabel_dict_i1
        - Compute the feature values and pattern indexes on the game position selection.
          Then combines game_positions, feature_values and indexes into a single gpdf data frame:
            m.compute_indexes()
            m.compute_feature_values()
            m.combine_gps_features_patterns()
        - Compute the rglm variable id <-> entity type, entity, index reference maps.
            m.compute_vmaps()
        - Compute the game positions X matrix as an integer data frame.
            m.compute_gpxpidf()
        - Compute the correlation matrix
            m.compute_x()
        - Compute the expected values
            m.compute_y()
        - Run the optimization procedure
            m.optimize()


    Attributes
    ----------
    conn : RegabDBConnection
      Connection to the REGAB database
    empty_count : int
      Clause used to query the REGAB database for game positions
    batches : list
      Clause used to query the REGAB database for game positions
    statuses : list
      Clause used to query the REGAB database for game positions
    features : list
      Selected features characterizing the model
    flabel_dict : dict
      Dictionary of the labels belonging to a selected feature.
      The feature id is the key, the value is a list of strings collecting the
      labels used to name the columns of the game_positions data frame
    patterns : list
      Selected patterns characterizing the model
    plabel_dict_i0 : dict
      Dictionary of the labels of indexes belonging to a selected pattern.
    plabel_dict_i1 : dict
      Dictionary of the labels of principal indexes belonging to a selected pattern.
    game_positions : pandas.DataFrame
      Game positions extracted from the REGAB database
    indexes : pandas.DataFrame
      Computed indexes belonging to selected patterns assigned to game positions
    feature_values : pandas.DataFrame
      Computed values belonging to selected features assigned to game positions
    gpdf : pandas.DataFrame
      Join of game_positions, feature_values, indexes
    vmap: pandas.DataFrame
      It has the cardinality of the RGLM variables.
      Fields are:
        - vid : int64
          RGLM variable id
        - etype : int64
          Entity type - 0 for feature, 1 for pattern
        - eid : int64
          Entity id - Pattern or feature id
        - idx : int64
          Index value - It is the pattern configuration index, or for features the index of the enumeration
    ivmap: pandas.DataFrame
      Inverse Variable Map, given etype, eid and idx gives the variable id value.
      Like vmap, it has the cardinality of the RGLM variables.
    gpxpidf: pandas.DataFrame
      Game Positions X matrix for Patterns (Integer values) Data Frame.
      Fields are:
        - vid : int64
          RGLM variable id
        - etype : int64
          Entity type - 0 for feature, 1 for pattern
        - eid : int64
          Entity id - Pattern or feature id
        - idx : int64
          Index value - It is the pattern configuration index, or for features the index of the enumeration
        - gpid : int64
          Game position id
        - counter : int64
          Count of times the index is present in the game position
      The table key is (vid, gpid), the value is counter. The fields (etype, eid, idx) are an alias for vid.
    x: scipy.sparse._csr.csr_matrix
      Matrix of correlations between the observations (valued and classified game positions) and the independent variables (weights).
      The matrix product X * weights is named the linear predictor.
    xt: scipy.sparse._csc.csc_matrix
      Transposed matrix of correlations.
    y: numpy.ndarray
      Epsilon is the vector of the true game position values.
    yh: numpy.ndarray
      Epsilon-hat is the vector of estimated game position values.
    w: numpy.ndarray
      Vector of weights. It is the result of the optimization.
    r: numpy.ndarray
      Vector of residuals. It is computed as: r = y - yh
    opt_res: scipy.optimize._optimize.OptimizeResult
      Result of the optimization.
    """
    def __init__(self):
        """
        Init all the relevant attributes.
        """
        self._CTHelper = _RglmdfGeneralDataCTHelper()
        self.conn = None
        self.empty_count = None
        self.batches = None
        self.vld_batches = None
        self.statuses = None
        self.vld_statuses = None
        self.game_positions = None
        self.vld_game_positions = None
        self.position_summary_table = None
        self.vld_position_summary_table = None
        self.features = None
        self.flabel_dict = None
        self.patterns = None
        self.feature_values = None
        self.vld_feature_values = None
        self.indexes = None
        self.vld_indexes = None
        self.plabel_dict_i0 = None
        self.plabel_dict_i1 = None
        self.gpdf = None
        self.vld_gpdf = None
        self.vmap = None
        self.ivmap = None
        self.gpxpidf = None
        self.x = None
        self.xt = None
        self.y = None
        self.yh = None
        self.w = None
        self.r = None
        self.opt_res = None
        self.wmeans = None
        self.evmap = None
        self.ievmap = None
        self.ew = None
        self.vld_gpxpidf = None
        self.vld_x = None
        self.vld_xt = None
        self.vld_y = None
        self.vld_yh = None
        self.vld_r = None
        self.vld_summary = None        
        self._mover_field = 'mover'
        self._opponent_field = 'opponent'

    def __del__(self):
        self._CTHelper = None

    def read_from_binary_file(self, filename : str, verbose : bool):
        c = self._CTHelper
        c.read_from_binary_file(filename, verbose)
        self.set_empty_count(c.empty_count)
        self.set_batches(np.ctypeslib.as_array(c.batch_ids, shape=(c.batch_id_cnt,)).astype('int').tolist())
        self.set_statuses([bytes.decode(x, 'utf-8') for x in c.position_statuses[:c.position_status_cnt]])
        self.load_game_positions()
        self.set_features([features_as_list[x] for x in c.features[:c.feature_cnt]])
        self.set_patterns([patterns_as_list[x] for x in c.patterns[:c.pattern_cnt]])
        self.compute_feature_values()
        self.compute_indexes()
        self.combine_gps_features_patterns()
        self.compute_vmaps()
        self.compute_gpxpidf()
        self.compute_x()
        self.compute_y()
        self.compute_analytics()
        self.retrieve_expected_probabilities_from_rglm_data_file()
        #
        # workflow:
        #
        # m = timed_run(Rglm, "m = Rglm()")
        # m = timed_run(m.set_conn, "m = m.set_conn({})", conn)
        # m = timed_run(m.set_empty_count, "m = m.set_empty_count({})", ec)
        # m = timed_run(m.set_batches, "m = m.set_batches({})", batches)
        # m = timed_run(m.set_vld_batches, "m = m.set_vld_batches({})", vld_batches)
        # m = timed_run(m.set_statuses, "m = m.set_statuses({})", statuses)
        # m = timed_run(m.set_vld_statuses, "m = m.set_vld_statuses({})", vld_statuses)
        # m = timed_run(m.retrieve_game_positions, "m = m.retrieve_game_positions()")
        # m = timed_run(m.retrieve_vld_game_positions, "m = m.retrieve_vld_game_positions()")
        # m = timed_run(m.set_features, "m = m.set_features({})", features)
        # m = timed_run(m.set_patterns, "m = m.set_patterns({})", patterns)
        # m = timed_run(m.compute_feature_values, "m = m.compute_feature_values()")
        # m = timed_run(m.compute_indexes, "m = m.compute_indexes()")
        # m = timed_run(m.combine_gps_features_patterns, "m = m.combine_gps_features_patterns()")
        # m = timed_run(m.compute_vmaps, "m = m.compute_vmaps()")
        # m = timed_run(m.compute_gpxpidf, "m = m.compute_gpxpidf()")
        # m = timed_run(m.compute_x, "m = m.compute_x()")
        # m = timed_run(m.compute_y, "m = m.compute_y()")
        # m = timed_run(m.compute_analytics, "m = m.compute_analytics()")
        # m = timed_run(m.optimize, "m = m.optimize({}, {{...}})", ridge_reg_param, l_bfgs_b_options)
        # if l_bfgs_b_options is not None:
        #     print("   l_bfgs_b_options = {}".format(l_bfgs_b_options))
        # m = timed_run(m.compute_wmean_for_patterns, "m = m.compute_wmean_for_patterns()")
        # m = timed_run(m.compute_evmap, "m = m.compute_evmap()")
        # m = timed_run(m.compute_ievmap, "m = m.compute_ievmap()")
        # m = timed_run(m.compute_vld_gpxpidf, "m = m.compute_vld_gpxpidf()")
        # m = timed_run(m.compute_vld_x, "m = m.compute_vld_x()")
        # m = timed_run(m.compute_vld_y, "m = m.compute_vld_y()")
        # m = timed_run(m.validate, "m = m.validate()")

    def populate_cthelper(self):
        self._CTHelper = _RglmdfGeneralDataCTHelper()
        c = self._CTHelper

        c.file_digest = ct.c_char_p('0000000000000000000000000000000000000000000000000000000000000000'.encode('utf-8'))
        c.file_creation_time = unix_time_now()
        c.format = c_rglmdf_file_data_format_type_is_general
        c.set_batch_ids(self.batches)
        c.empty_count = self.empty_count
        c.set_position_statuses(self.statuses)
        c.set_features(self.features)
        c.set_patterns(self.patterns)
        c.set_position_summary_table(self.position_summary_table)
        c.set_entity_freq_summary_table(self.vmap)
        
        # HERE

        # + ("file_digest", ct.c_char_p),
        # + ("file_creation_time", c_time_t),
        # + ("format", c_rglmdf_file_data_format_type_t),
        # + ("batch_id_cnt", ct.c_size_t),
        # + ("batch_ids", ct.POINTER(ct.c_uint64)),
        # + ("empty_count", ct.c_uint8),
        # + ("position_status_cnt", ct.c_size_t),
        # + ("position_status_buffer", ct.c_char_p),
        # + ("position_statuses", ct.POINTER(ct.c_char_p)),
        # + ("feature_cnt", ct.c_size_t),
        # + ("features", ct.POINTER(c_board_feature_id_t)),
        # + ("pattern_cnt", ct.c_size_t),
        # + ("patterns", ct.POINTER(c_board_pattern_id_t)),
        # + ("position_summary", _RglmdfPositionSummaryTable),
        # ("entity_freq_summary", _RglmdfEntityFreqSummaryTable),
        # ("positions", _RglmdfSolvedAndClassifiedGpTable),
        # ("reverse_map_a_f", ct.POINTER(ct.POINTER(ct.c_int32))),
        # ("reverse_map_a_p", ct.POINTER(ct.POINTER(ct.c_int32))),
        # ("reverse_map_b", ct.POINTER(ct.c_int32)),


        return

    def write_to_binary_file(self, filename : str, time=None):
        self._CTHelper.write_to_binary_file(filename, time)
    
    def set_conn(self, conn: RegabDBConnection) -> Rglm:
        if not isinstance(conn, RegabDBConnection):
            raise TypeError('Argument conn is not an instance of RegabDBConnection')
        self.conn = conn
        return self

    def close_conn(self):
        if self.conn:
            self.conn.close()

    def set_empty_count(self, ec: int) -> Rglm:
        self.empty_count = regab_empty_count(ec)
        return self

    def set_batches(self, bs) -> Rglm:
        self.batches = regab_batches(bs)
        return self

    def set_vld_batches(self, bs) -> Rglm:
        self.vld_batches = regab_batches(bs)
        return self

    def set_statuses(self, sts) -> Rglm:
        self.statuses = regab_statuses(sts)
        return self

    def set_vld_statuses(self, sts) -> Rglm:
        self.vld_statuses = regab_statuses(sts)
        return self

    def set_features(self, fs) -> Rglm:
        self.features = regab_features(fs)
        return self

    def set_patterns(self, ps) -> Rglm:
        self.patterns = regab_patterns(ps)
        return self

    def _retrieve_game_positions(self, batches, statuses, limit=None, where=None, fields=None) -> pd.DataFrame:
        q = regab_gp_as_df(self.conn, batches, statuses, self.empty_count, limit, where, fields)
        summary_table = pd.DataFrame(q[['seq', 'batch_id', 'status']].groupby(['batch_id', 'status']).count().to_records())
        summary_table.rename(columns={'seq': 'count'}, inplace=True)
        cols = [x for x in list(q.columns) if x not in ['batch_id', 'status', 'player', 'empty_count']]
        q['gpid'] = q.index
        gps = pd.DataFrame()
        for c in ['gpid'] + cols:
            gps[c] = q.pop(c)
        return (gps, summary_table)

    def retrieve_game_positions(self, limit=None, where=None, fields=None) -> Rglm:
        """
        Retrieves game positions from the REGAB database.
        """
        gps, summary_table = self._retrieve_game_positions(self.batches, self.statuses, limit, where, fields)
        self.game_positions = gps
        self.position_summary_table = summary_table
        return self

    def extract_rglmdf_entity_freq_summary_table(self) -> pd.DataFrame:
        """
        Extracts the _RglmdfEntityFreqSummaryTable table from the _CTHelper object
        and returns it as a pandas dataframe.
        """
        c = self._CTHelper
        t = c.entity_freq_summary
        a = np.ctypeslib.as_array(t.records, shape=(t.ntuples,))
        d = pd.DataFrame(a, columns = a.dtype.names)
        return d
    
    def load_game_positions(self) -> Rglm:
        """
        Loads game position from the _CTHelper object.
        """
        game_positions = pd.DataFrame()
        c = self._CTHelper
        a = np.ctypeslib.as_array(c.positions.records, shape=(c.positions.ntuples,))
        d = pd.DataFrame(a, columns = a.dtype.names)
        game_positions['gpid'] = d.pop('row_n')
        game_positions['seq'] = d.pop('gp_id')
        for x in ['mover', 'opponent', 'game_value']:
            game_positions[x] = d.pop(x)
        self.game_positions = game_positions
        return self

    def retrieve_vld_game_positions(self, limit=None, where=None, fields=None) -> Rglm:
        """
        Retrieves validation game positions from the REGAB database.
        """
        gps, summary_table = self._retrieve_game_positions(self.vld_batches, self.vld_statuses, limit, where, fields)
        self.vld_game_positions = gps
        self.vld_position_summary_table = summary_table
        return self

    def compute_feature_values(self) -> Rglm:
        """
        Computes the feature values on all the game positions extracted from he REGAB database.
        """
        if not isinstance(self.game_positions, pd.DataFrame):
            raise TypeError('The field game_positions is not an instance of DataFrame')
        if not set([self._mover_field, self._opponent_field]).issubset(self.game_positions.columns):
            raise ValueError('The game_positions data frame is missing mover or opponent columns')
        if self.features is None or self.features == []:
            raise ValueError('The field features is not defined or empty')
        self.feature_values, flabel_dict = compute_feature_values_on_df(self.game_positions, self.features, mover=self._mover_field, opponent=self._opponent_field)
        self.flabel_dict = dict((key.id, value) for (key, value) in flabel_dict.items())
        return self

    def compute_vld_feature_values(self) -> Rglm:
        """
        Computes the feature values on all the validation game positions extracted from he REGAB database.
        """
        if not isinstance(self.vld_game_positions, pd.DataFrame):
            raise TypeError('The field vld_game_positions is not an instance of DataFrame')
        if not set([self._mover_field, self._opponent_field]).issubset(self.game_positions.columns):
            raise ValueError('The game_positions data frame is missing mover or opponent columns')
        if self.features is None or self.features == []:
            raise ValueError('The field features is not defined or empty')
        self.vld_feature_values, unused = compute_feature_values_on_df(self.vld_game_positions, self.features, mover=self._mover_field, opponent=self._opponent_field)
        return self

    def compute_indexes(self) -> Rglm:
        """
        Computes the pattern index values on all the game positions extracted from he REGAB database.
        """
        if not isinstance(self.game_positions, pd.DataFrame):
            raise TypeError('The field game_positions is not an instance of DataFrame')
        if not set([self._mover_field, self._opponent_field]).issubset(self.game_positions.columns):
            raise ValueError('The game_positions data frame is missing mover or opponent columns')
        if self.patterns is None or self.patterns == []:
            raise ValueError('The field patterns is not defined or empty')
        self.indexes, plabel_dict_i0, plabel_dict_i1 = compute_indexes_on_df(self.game_positions, self.patterns, mover=self._mover_field, opponent=self._opponent_field)
        self.plabel_dict_i0 = dict((key.id, value) for (key, value) in plabel_dict_i0.items())
        self.plabel_dict_i1 = dict((key.id, value) for (key, value) in plabel_dict_i1.items())
        return self

    def compute_vld_indexes(self) -> Rglm:
        """
        Computes the pattern index values on all the validation game positions extracted from he REGAB database.
        """
        if not isinstance(self.vld_game_positions, pd.DataFrame):
            raise TypeError('The field vld_game_positions is not an instance of DataFrame')
        if not set([self._mover_field, self._opponent_field]).issubset(self.game_positions.columns):
            raise ValueError('The game_positions data frame is missing mover or opponent columns')
        if self.patterns is None or self.patterns == []:
            raise ValueError('The field patterns is not defined or empty')
        self.vld_indexes, unused_0, unused_1 = compute_indexes_on_df(self.vld_game_positions, self.patterns, mover=self._mover_field, opponent=self._opponent_field)
        return self

    def combine_gps_features_patterns(self) -> Rglm:
        """
        Computes the gpdf data frame, by concatenating game_positions, feature values, and pattern indexes.
        """
        self.gpdf = pd.concat([self.game_positions, self.feature_values, self.indexes], axis=1, copy=False)
        return self

    def combine_vld_gps_features_patterns(self) -> Rglm:
        """
        Computes the validation gpdf data frame, by concatenating vld_game_positions, validation feature values, and validation pattern indexes.
        """
        self.vld_gpdf = pd.concat([self.vld_game_positions, self.vld_feature_values, self.vld_indexes], axis=1, copy=False)
        return self

    def get_feature_analytics(self, labels) -> pd.DataFrame:
        """
        Argument labels must be one column or a list of columns included in the gpdf column labels.
        
        As an example, here label F_001 represent MOBILITY:
        .
        .  >>> m.get_feature_analytics('F_001')
        .         count  min  max       mean        std
        .  F_001                                       
        .  0.05     455  -64    4 -52.153846  14.186395
        .  0.10    1015  -64   28 -45.389163  17.277788
        .  0.15    2297  -64   54 -38.234219  19.879931
        .  0.20    4348  -64   58 -31.655934  20.895818
        .  0.25    7755  -64   58 -25.470277  21.595861
        .  0.30   12077  -64   64 -19.850294  22.070053
        .  0.35   17537  -64   64 -14.563722  22.112477
        .  0.40   22566  -64   64  -9.372153  22.392523
        .  0.45   26262  -60   64  -4.100069  22.464623
        .  0.50   27320  -64   64   1.286969  22.574448
        .  0.55   25447  -58   64   5.917790  22.487615
        .  0.60   20506  -54   64  10.651907  22.195976
        .  0.65   14909  -46   64  15.655242  21.785173
        .  0.70    9312  -40   64  19.483677  21.067947
        .  0.75    4922  -34   64  24.036164  19.704655
        .  0.80    2100  -32   64  26.958095  18.537477
        .  0.85     803  -24   64  30.789539  17.553603
        .  0.90     240  -10   62  33.941667  15.495604
        .  0.95      56  -20   64  33.500000  17.982820
        .  1.00       5   46   54  49.600000   2.966479

        """
        res = self.gpdf.groupby(labels).game_value.agg(['count', 'min', 'max', 'mean', 'std'])
        return res

    def compute_vmaps(self) -> Rglm:
        """
        Computes the vmap and ivmap data frames.
        These two data frames map the RGLM variable id (weight) to and from the three-field-key (etype, eid, idx). 

        Here an example of the vmap data frame:
        .
        .         vid  etype  eid   idx
        .  0        0      0    0     0
        .  1        1      0    1     0
        .  2        2      1    0     0
        .  3        3      1    0     1
        .  4        4      1    0     2
        .  ...    ...    ...  ...   ...
        .  2960  2960      1    0  6371
        .  2961  2961      1    0  6398
        .  2962  2962      1    0  6452
        .  2963  2963      1    0  6479
        .  2964  2964      1    0  6560

        Here an example of the ivmap data frame:
        .
        .                   vid
        .  etype eid idx       
        .  0     0   0        0
        .        1   0        1
        .  1     0   0        2
        .            1        3
        .            2        4
        .  ...              ...
        .            6371  2960
        .            6398  2961
        .            6452  2962
        .            6479  2963
        .            6560  2964

        vid : variable id, it is the index of the weight in the weight array.
        etype : entity type, 0 for features, 1 for patterns. It is always 1 in this data frame.
        eid : entity id, it is the pattern id.
        idx : index, the index value.

        """
        vmap_colnames = ['vid', 'etype', 'eid', 'idx']
        self.vmap = pd.DataFrame(columns = vmap_colnames, dtype = 'int64')
        var_cnt = 0
        
        for f in self.features:
            var_cnt_for_f = f.field_cnt
            var_cnt_updated = var_cnt + var_cnt_for_f
            indexes = list(range(0, var_cnt_for_f))
            rglm_var_id = list(range(var_cnt, var_cnt_updated))
            entity_type = [0] * var_cnt_for_f
            f_id_list = [f.id] * var_cnt_for_f
            vmap_f = pd.DataFrame(list(zip(rglm_var_id, entity_type, f_id_list, indexes)), columns = vmap_colnames)
            self.vmap = pd.concat([self.vmap, vmap_f], axis = 0, ignore_index = True, sort = False, copy = False)
            var_cnt = var_cnt_updated
            
        for p in self.patterns:
            set_of_indexes = set()
            labels = self.plabel_dict_i1[p.id]
            for label in labels:
                col_as_list = self.gpdf[label].values.tolist()
                set_of_indexes.update(col_as_list)
            indexes = sorted(set_of_indexes)
            var_cnt_for_p = len(indexes)
            var_cnt_updated = var_cnt + var_cnt_for_p
            rglm_var_id = list(range(var_cnt, var_cnt_updated))
            entity_type = [1] * var_cnt_for_p
            p_id_list = [p.id] * var_cnt_for_p
            vmap_p = pd.DataFrame(list(zip(rglm_var_id, entity_type, p_id_list, indexes)), columns = vmap_colnames)
            self.vmap = pd.concat([self.vmap, vmap_p], axis = 0, ignore_index = True, sort = False, copy = False)
            var_cnt = var_cnt_updated
            mi = pd.MultiIndex.from_frame(self.vmap[['etype', 'eid', 'idx']])
            self.ivmap = pd.DataFrame(self.vmap['vid'].values, index=mi, columns=['vid'])
            
        return self

    def compute_gpxpidf(self) -> Rglm:
        """
        Computes the gpxpidf data frame.

        This data frame is the precomputation of the X matrix for what belongs to patterns:
          gpid    -> row_id
          vid     -> column_id
          counter -> value

        Here an example of the data frame:
        . 
        .           vid  etype  eid   idx    gpid  counter
        .  0          2      1    0     0       0        1
        .  1          2      1    0     0      23        1
        .  2          2      1    0     0      27        1
        .  3          2      1    0     0      28        1
        .  4          2      1    0     0      32        1
        .  ...      ...    ...  ...   ...     ...      ...
        .  796170  2964      1    0  6560  199786        1
        .  796171  2964      1    0  6560  199795        1
        .  796172  2964      1    0  6560  199871        1
        .  796173  2964      1    0  6560  199882        1
        .  796174  2964      1    0  6560  199915        1

        vid : variable id, it is the index of the weight in the weight array.
        etype : entity type, 0 for features, 1 for patterns. It is always 1 in this data frame.
        eid : entity id, it is the pattern id.
        idx : index, the index value.
        gpid : game position id, the id of the game position record in the gpdf/game_positions data frames.
        counter : count the times the pattern/index is found in the game position (almost always it is 1).

        """
        gpxpidf_colnames = ['vid', 'etype', 'eid', 'idx', 'gpid', 'counter']
        self.gpxpidf = pd.DataFrame(columns = gpxpidf_colnames, dtype = 'int64')
        for p in self.patterns:
            labels = self.plabel_dict_i1[p.id]
            renamed_labels = dict(zip(labels, ['idx']*p.n_instances))
            res = pd.concat(self.gpdf[['gpid', x]].rename(columns=renamed_labels) for x in labels)
            res.insert(loc=1, column='eid', value=[p.id]*len(res))
            res.insert(loc=1, column='etype', value=[1]*len(res))
            res['counter'] = 1
            res_grouped = res.groupby(['gpid', 'etype', 'eid', 'idx'])['counter'].sum().reset_index()
            mi = pd.MultiIndex.from_frame(res_grouped[['etype', 'eid', 'idx']])
            res_grouped = pd.DataFrame(res_grouped.values, index=mi, columns=['gpid', 'etype', 'eid', 'idx', 'counter'], dtype = 'int64')
            res_grouped = res_grouped.merge(self.ivmap, left_index=True, right_index=True, how='left')
            self.gpxpidf = pd.concat([self.gpxpidf, res_grouped], axis=0,  ignore_index = True, sort = False, copy = False)
        return self

    def compute_x(self) -> Rglm:
        """
        Computes the x matrix and its transposed xt one.
        X is a standard name in machine learning terminology to refer to the matrix that
        multiplied by the weights return the linear predictor vector.
        """
        n_row = len(self.game_positions)
        n_col = len(self.vmap)
        row_idx = np.array([], dtype='int64')
        col_idx = np.array([], dtype='int64')
        data_values = np.array([], dtype='float64')
        for find, fcol in enumerate(self.feature_values):
            row_idx = np.append(row_idx, np.array(range(0, n_row)))
            col_idx = np.append(col_idx, np.array([find]*n_row))
            data_values = np.append(data_values, self.feature_values[fcol].to_numpy())
        row_idx = np.append(row_idx, self.gpxpidf['gpid'].to_numpy())
        col_idx = np.append(col_idx, self.gpxpidf['vid'].to_numpy())
        data_values = np.append(data_values, self.gpxpidf['counter'].to_numpy())
        self.x = csr_matrix((data_values, (row_idx, col_idx)), shape = (n_row, n_col), dtype = 'float64')
        self.xt = self.x.transpose()
        return self

    def compute_y(self) -> Rglm:
        """
        Computes the y (epsilon) array applaying the rglm_gv_to_gvt transformation to the game_value
        field as retrieved from the regab database (game_position data frame).
        Reset w (weights) array to 0, and yh (epsilon-hat) array to 0.5.
        """
        gv = self.game_positions['game_value'].to_numpy()
        gvt = rglm_gv_to_gvt(gv)
        self.y = gvt
        self.w = np.full(len(self.vmap), 0.)
        self.yh = np.full(len(gvt), 0.5)
        return self

    def function(self) -> float:
        """
        Computes the objective function evaluated at the current weight values.
        As a side effects it updates the epsilon-hat array.
        """
        self.yh = rglm_sigmoid(self.x @ self.w)
        return 0.5 * sum((self.y - self.yh)**2)
    
    def gradient(self) -> np.ndarray:
        """
        Computes the gradient of the objective function evaluated at the current weight values.
        As a side effects it updates the epsilon-hat array.
        """
        self.yh = rglm_sigmoid(self.x @ self.w)
        return - self.xt @ ((self.yh * (1. - self.yh)) * (self.y - self.yh))

    def compute_analytics(self) -> Rglm:
        """
        Adds to vmap the following fields:
          - count  : Count of game positions categorized by having the pattern/feature configuration
          - min    :
          - max    :
          - mean   :
          - std    :
          - perc10 :
          - perc25 :
          - perc50 :
          - perc75 :
          - perc90 :
          - oprobs : Observed probability
          - eprobs : Expected probability
        """
        #
        # ocount: observed count
        #         It is the count of the occurrences of the pattern configuration in the data set.
        #
        # oprob: observed probability
        #        It is the observed probability to have the pattern configuration corresponding to the vid value
        #        present in the board of the game position record in the model sample.
        #        For features it is always 1.0
        #
        # meangv: mean game value
        #         It is the mean game value of the subset of game positions having the pattern configuration
        #         In case of features, it is the weighted mean value, where the weight is the feature value.
        #
        #        The values are added to the vmap data frame as columns.
        #
        #                  vid  etype  eid    idx   count  min  max       mean        std  perc10  perc25  perc50  perc75  perc90     oprob    eprobs
        #         0          0      0    0      0  399792  -64   64  -1.393860  26.459620   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000
        #         1          1      0    3      0  399792  -64   64  -1.393860  26.459620   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000
        #         2          2      0    3      1  399792  -64   64  -1.393860  26.459620   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000
        #         3          3      0    3      2  399792  -64   64  -1.393860  26.459620   -36.0   -22.0    -2.0    18.0    34.0  1.000000  1.000000
        #         4          4      1    1      0    9086  -64   64  -2.420207  30.424988   -42.0   -26.0    -4.0    20.0    40.0  0.005682  0.005270
        #         ...      ...    ...  ...    ...     ...  ...  ...        ...        ...     ...     ...     ...     ...     ...       ...       ...
        #         71441  71441      1   11  59044     479  -60   64   8.125261  22.410876   -20.0    -6.0    10.0    22.0    36.4  0.000150  0.000105
        #         71442  71442      1   11  59045     238  -62   30 -17.436975  21.176386   -44.6   -33.5   -18.0    -4.0    12.0  0.000074  0.000050
        #         71443  71443      1   11  59046    3224  -64   64  13.232630  22.925177   -18.0    -2.0    14.0    30.0    42.0  0.001008  0.000939
        #         71444  71444      1   11  59047    1271  -62   64   4.483084  23.602854   -26.0   -10.0     4.0    22.0    36.0  0.000397  0.000259
        #         71445  71445      1   11  59048    8114  -64   44 -31.440227  20.963169   -58.0   -48.0   -34.0   -18.0    -2.0  0.002537  0.001444
        #

        # Boundaries for the percentile analysis.
        percentiles = [10, 25, 50, 75, 90]
        
        # Transform the x matrix from CSR to CSC format. CSC is better suited to slice columns.
        x = self.x.tocsc()
        
        # Collects game values as an array.
        y = self.game_positions.game_value.values

        # Count of RGLM variables (weights).
        w_count = len(self.vmap)

        # Count of variables belonging to features and to patterns.
        w_belonging_to_feature_count = len(self.vmap[self.vmap['etype'] == 0])
        w_belonging_to_pattern_count = len(self.vmap[self.vmap['etype'] == 1])
        
        if w_belonging_to_feature_count + w_belonging_to_pattern_count != w_count:
            raise ValueError('w_belonging_to_feature_count + w_belonging_to_pattern_count must be equal to w_count')

        # Prepare the list to accumulate result to.
        vmap_extension_records = []
        
        # Key statistics of the sample are assigned to all features.
        sample_count = len(y)
        sample_min = y.min()
        sample_max = y.max()
        sample_mean = y.mean()
        sample_std = y.std()
        sample_perc = np.percentile(y, percentiles)
        vmap_extension_record = {
            'count': sample_count,
            'min': sample_min,
            'max': sample_max,
            'mean': sample_mean,
            'std': sample_std,
            'perc10': sample_perc[0], 'perc25': sample_perc[1], 'perc50': sample_perc[2], 'perc75': sample_perc[3], 'perc90': sample_perc[4]
        }

        # Loop over variables belonging to features.
        for vid in range(0, w_belonging_to_feature_count):
            vmap_extension_records.append(vmap_extension_record)

        # Loop over variables belonging to patterns.
        for vid in range(w_belonging_to_feature_count, w_count):
            # Slice the column corresponding to the RGLM variable (vid).
            vid_x_col = x[:,vid].astype(int)
            # Duplicate values time the occurrences when a pattern is found more than once in a game position.
            game_values = np.repeat(y[vid_x_col.indices], vid_x_col.data)
            # Compute prcentiles.
            perc = np.percentile(game_values, percentiles)
            # Prepare the record and append it.
            vmap_extension_record = {
                'count': len(game_values),
                'min': min(game_values),
                'max': max(game_values),
                'mean': game_values.mean(),
                'std': game_values.std(),
                'perc10': perc[0], 'perc25': perc[1], 'perc50': perc[2], 'perc75': perc[3], 'perc90': perc[4]
            }
            vmap_extension_records.append(vmap_extension_record)
            
        vmap_extension = pd.DataFrame.from_dict(vmap_extension_records)
        self.vmap = pd.concat([self.vmap, vmap_extension], axis=1, ignore_index=False)
        
        # Compute the Observed Probabilities (oprob).
        def get_n_instances(row):
            if row['etype'] == 0:
                ret = 1.
            elif row['etype'] == 1:
                pid = int(row['eid'])
                p = patterns_as_list[pid]
                ret = float(p.n_instances)
            else:
                raise ValueError('etype must be 0 (Feature) or 1 (Pattern).')
            return ret

        n_instances = self.vmap.apply(lambda row: get_n_instances(row), axis=1)
        
        x_sum_by_weight = np.array(self.x.sum(axis=0)).ravel()
        oprobs = x_sum_by_weight / (n_instances.values * sample_count)
        oprobs[0:w_belonging_to_feature_count] = 1.0
        
        self.vmap['oprobs'] = oprobs

        return self

    def retrieve_expected_probabilities_from_regab_db(self) -> Rglm:
        """
        Retrieve the expected probabilities (eprob) from the REGAB database, then merge (join) vmap with the extraction (df_b).
        eprobs for features are set to 1.
        """
        df_b = regab_patternlist_probs_as_df(rc=self.conn, patterns=self.patterns, ec=self.empty_count, is_principal=True)
        df_b = df_b.rename(columns={'pattern_id': 'eid', 'principal': 'idx', 'probs': 'eprobs'})
        df_b['etype'] = 1
        self.vmap = pd.merge(self.vmap, df_b, how='left', on=['etype', 'eid', 'idx'])
        self.vmap.loc[self.vmap['etype'] == 0, 'eprobs'] = 1.
        return self

    def retrieve_expected_probabilities_from_rglm_data_file(self) -> Rglm:
        df = self.extract_rglmdf_entity_freq_summary_table()
        df = df.rename(columns={'entity_class':            'etype',
                                'entity_id':               'eid',
                                'principal_index_value':   'idx',
                                'theoretical_probability': 'eprobs'})
        df.drop(columns=['glm_variable_id', 'total_cnt', 'relative_frequency', 'weight'], inplace=True)
        self.vmap = pd.merge(self.vmap, df, how='left', on=['etype', 'eid', 'idx'])
        return self
    
    def optimize(self, c=None, options=None) -> Rglm:
        """
        Finds the minimum of the objective function, optimizing the values
        assigned to weights.
        Argument c is the Ridge regularization coefficient defaulted to 0.0.
        Argument options is passed as it is to the scipy minimize call.

        """
        if c is None:
            c = 0.
        if options is None:
            options = {'disp': False,
                       'maxcor': 50,
                       'ftol': 1e-08,
                       'gtol': 1e-05,
                       'eps': 1e-08,
                       'maxfun': 5000,
                       'maxiter': 5000,
                       'iprint': 1,
                       'maxls': 20,
                       'finite_diff_rel_step': None}
        
        def fg(w):
            linear_predictor = self.x @ w
            self.yh = rglm_sigmoid(linear_predictor)
            self.r = self.y - self.yh
            f = 0.5 * (sum(self.r**2) + c * sum(w**2))
            g = - self.xt @ ((self.yh * (1. - self.yh)) * self.r) + c * w
            return f, g
        
        self.opt_res = minimize(fg, self.w, jac=True, method='L-BFGS-B', options=options)

        self.w = self.opt_res.x
        self.yh = rglm_sigmoid(self.x @ self.w)
        self.r = self.y - self.yh

        self.vmap['weight'] = self.w
        
        return self

    def compute_wmean_for_patterns(self):
        self.wmeans = dict.fromkeys([p.id for p in self.patterns])
        df = self.vmap[self.vmap['etype'] == 1][['eid','oprobs','weight']]
        wmeandf = df.assign(wmean=df.oprobs*df.weight).groupby('eid', as_index=False).wmean.sum()
        wmeandf = wmeandf.astype({'eid':'int','wmean':'float'})
        for row in wmeandf.itertuples(index=False):
            eid, wmean = row
            p = patterns_as_list[eid]
            self.wmeans[p.id] = wmean
        return self

    def compute_evmap(self):
        etype = np.empty([0], dtype = int)
        eid = np.empty([0], dtype = int)
        idx = np.empty([0], dtype = int)
        pidx = np.empty([0], dtype = int)
        wmean = np.empty([0], dtype = int)
        for f in self.features:
            f_etype = np.repeat(0, (f.field_cnt))
            f_eid = np.repeat(f.id, (f.field_cnt))
            f_idx = np.arange(0, f.field_cnt)
            f_pidx = f_idx
            f_wmean = np.repeat(0.0, (f.field_cnt))
            etype = np.concatenate((etype, f_etype))
            eid = np.concatenate((eid, f_eid))
            idx = np.concatenate((idx, f_idx))
            pidx = np.concatenate((pidx, f_pidx))
            wmean = np.concatenate((wmean, f_wmean))
        for p in self.patterns:
            p_etype = np.repeat(1, (p.n_configurations))
            p_eid = np.repeat(p.id, (p.n_configurations))
            p_idx = np.arange(0, p.n_configurations)
            p_pidx = p.principal_index_vec(p_idx)
            p_wmean = np.repeat(self.wmeans[p.id], (p.n_configurations))
            etype = np.concatenate((etype, p_etype))
            eid = np.concatenate((eid, p_eid))
            idx = np.concatenate((idx, p_idx))
            pidx = np.concatenate((pidx, p_pidx))
            wmean = np.concatenate((wmean, p_wmean))
        self.evmap = pd.DataFrame()
        self.evmap['etype'] = etype
        self.evmap['eid'] = eid
        self.evmap['idx'] = idx
        self.evmap['pidx'] = pidx
        self.evmap['wmean'] = wmean
        self.evmap = self.evmap.merge(self.vmap[['etype', 'eid', 'idx', 'vid', 'weight', 'count', 'oprobs', 'eprobs']],
                                                  how='left',
                                                  left_on=['etype', 'eid', 'pidx'],
                                                  right_on=['etype', 'eid', 'idx'])
        self.evmap.rename(columns={'idx_x': 'idx'}, inplace=True)
        self.evmap['vid'] = self.evmap['vid'].fillna(-1).astype(int)
        self.evmap['computed'] = ~pd.isna(self.evmap["idx_y"])
        self.evmap.weight.fillna(self.evmap.wmean, inplace=True)
        self.evmap.drop(columns=['idx_y'], inplace=True)
        self.evmap['evid'] = self.evmap.index
        self.evmap['count'] = self.evmap['count'].fillna(0)
        self.evmap['count'] = self.evmap['count'].astype('int')
        self.evmap['oprobs'] = self.evmap['oprobs'].fillna(0)
        self.evmap['eprobs'] = self.evmap['eprobs'].fillna(0)
        self.evmap = self.evmap.loc[:, ['evid', 'etype', 'eid', 'idx', 'pidx', 'vid', 'wmean', 'computed', 'weight', 'count', 'oprobs', 'eprobs']]
        self.evmap.rename(columns={'count': 'total_cnt'}, inplace=True)
        self.ew = self.evmap['weight'].values
        return self

    def compute_ievmap(self):
        mi = pd.MultiIndex.from_frame(self.evmap[['etype', 'eid', 'idx']])
        self.ievmap = pd.DataFrame(self.evmap['evid'].values, index=mi, columns=['evid'])
        return self

    def compute_vld_gpxpidf(self):
        """
        Computes the vld_gpxpidf data frame.

        This data frame is the precomputation of the X matrix for validation for what belongs to patterns:
          gpid    -> row_id
          vid     -> column_id
          counter -> value

        Here an example of the data frame:
        . 


        vid : variable id, it is the index of the weight in the weight array.
        etype : entity type, 0 for features, 1 for patterns. It is always 1 in this data frame.
        eid : entity id, it is the pattern id.
        idx : index, the index value.
        gpid : game position id, the id of the game position record in the gpdf/game_positions data frames.
        counter : count the times the pattern/index is found in the game position (almost always it is 1).

        """
        vld_gpxpidf_colnames = ['evid', 'etype', 'eid', 'idx', 'gpid', 'counter']
        self.vld_gpxpidf = pd.DataFrame(columns = vld_gpxpidf_colnames, dtype = 'int64')
        for p in self.patterns:
            labels = self.plabel_dict_i0[p.id]
            renamed_labels = dict(zip(labels, ['idx']*p.n_instances))
            res = pd.concat(self.vld_gpdf[['gpid', x]].rename(columns=renamed_labels) for x in labels)
            res.insert(loc=1, column='eid', value=[p.id]*len(res))
            res.insert(loc=1, column='etype', value=[1]*len(res))
            res['counter'] = 1
            res_grouped = res.groupby(['gpid', 'etype', 'eid', 'idx'])['counter'].sum().reset_index()
            mi = pd.MultiIndex.from_frame(res_grouped[['etype', 'eid', 'idx']])
            res_grouped = pd.DataFrame(res_grouped.values, index=mi, columns=['gpid', 'etype', 'eid', 'idx', 'counter'], dtype = 'int64')
            res_grouped = res_grouped.merge(self.ievmap, left_index=True, right_index=True, how='left')
            self.vld_gpxpidf = pd.concat([self.vld_gpxpidf, res_grouped], axis=0,  ignore_index = True, sort = False, copy = False)
        return self
    
    def compute_vld_x(self):
        n_row = len(self.vld_game_positions)
        n_col = len(self.evmap)
        row_idx = np.array([], dtype='int64')
        col_idx = np.array([], dtype='int64')
        data_values = np.array([], dtype='float64')
        for find, fcol in enumerate(self.vld_feature_values):
            row_idx = np.append(row_idx, np.array(range(0, n_row)))
            col_idx = np.append(col_idx, np.array([find]*n_row))
            data_values = np.append(data_values, self.vld_feature_values[fcol].to_numpy())
        row_idx = np.append(row_idx, self.vld_gpxpidf['gpid'].to_numpy())
        col_idx = np.append(col_idx, self.vld_gpxpidf['evid'].to_numpy())
        data_values = np.append(data_values, self.vld_gpxpidf['counter'].to_numpy())
        self.vld_x = csr_matrix((data_values, (row_idx, col_idx)), shape = (n_row, n_col), dtype = 'float64')
        self.vld_xt = self.vld_x.transpose()
        return self
    
    def compute_vld_y(self):
        vld_linear_predictor = self.vld_x @ self.ew
        self.vld_yh = rglm_sigmoid(vld_linear_predictor)
        vld_gv = self.vld_game_positions['game_value'].to_numpy()
        vld_gvt = rglm_gv_to_gvt(vld_gv)
        self.vld_y = vld_gvt
        self.vld_r = self.vld_y - self.vld_yh
        return self
    
    def validate(self):
        opt_gp_count = len(self.game_positions)
        vld_gp_count = len(self.vld_game_positions)
        self.vld_summary = {
            'ref_residual_norm': np.linalg.norm(self.y - 0.5),
            'ref_residual_mean': np.mean(self.y - 0.5),
            'ref_residual_std': np.std(self.y - 0.5),
            'opt_residual_norm': np.linalg.norm(self.r),
            'opt_residual_mean': np.mean(self.r),
            'opt_residual_std': np.std(self.r),
            'vld_residual_norm': np.linalg.norm(self.vld_r),
            'vld_residual_mean': np.mean(self.vld_r),
            'vld_residual_std': np.std(self.vld_r),
            'opt_function_value': 0.5 * sum(self.r**2),
            'vld_function_value': 0.5 * sum(self.vld_r**2),
        }
        return self
    
    def get_model_weights(self):
        
        feature_cnt = len(self.features)
        pattern_cnt = len(self.patterns)
        weight_cnt = len(self.ew)

        features = np.array([f.id for f in self.features], dtype=np.int32)
        feature_record_size = features.itemsize
        if ct.sizeof(ct.c_int32) != feature_record_size:
            raise Exception('Sizeof features record is not defined consistently.')

        patterns = np.array([p.id for p in self.patterns], dtype=np.int32)
        pattern_record_size = patterns.itemsize
        if ct.sizeof(ct.c_int32) != pattern_record_size:
            raise Exception('Sizeof patterns record is not defined consistently.')

        # HERE
        
        weights = np.zeros(weight_cnt, dtype = [('entity_class', '<i2'),
                                                ('entity_id', '<i2'),
                                                ('index_value', '<i4'),
                                                ('principal_index_value', '<i4'),
                                                ('glm_variable_id', '<i4'),
                                                ('total_cnt', '<i8'),
                                                ('relative_frequency', '<f8'),
                                                ('theoretical_probability', '<f8'),
                                                ('weight', '<f8')])
        weights['entity_class'] = self.evmap.etype.values
        weights['entity_id'] = self.evmap.eid.values
        weights['index_value'] = self.evmap.idx.values
        weights['principal_index_value'] = self.evmap.pidx.values
        weights['glm_variable_id'] = self.evmap.vid.values
        weights['total_cnt'] = self.evmap.total_cnt.values
        weights['relative_frequency'] = self.evmap.oprobs.values
        weights['theoretical_probability'] = self.evmap.eprobs.values
        weights['weight'] = self.evmap.weight.values
        weight_record_size = weights.itemsize
        if ct.sizeof(_RglmWeightRecordCTHelper) != weight_record_size:
            raise Exception('Sizeof weights record is not defined consistently.')
        
        mw = RglmModelWeights()
        mw.allocate_memory_for_arrays(feature_cnt, pattern_cnt, weight_cnt,
                                      feature_record_size, pattern_record_size, weight_record_size)
        
        c = mw._CTHelper
        c.file_creation_time = 0
        c.general_data_checksum = ct.c_char_p('0000000000000000000000000000000000000000000000000000000000000000'.encode('utf-8'))
        c.empty_count = self.empty_count
        c.gp_sample_size = len(self.game_positions)
 
        ct.memmove(c.features, features.ctypes.data, feature_cnt * feature_record_size)
        ct.memmove(c.patterns, patterns.ctypes.data, pattern_cnt * pattern_record_size)
        ct.memmove(c.weights, weights.ctypes.data, weight_cnt * weight_record_size)

        f = libreversi.rglmdf_model_veights_compute_reverse_map
        f.restype = None
        f.argtypes = [ct.POINTER(_RglmModelWeightsCTHelper)]
        ct_rglmdf_model_weights_p = ct.byref(mw._CTHelper)
        f(ct_rglmdf_model_weights_p)        
        
        return mw

    def save(self, fp):
        if self.conn == None:
            conn_established = False
            conn_dbname = None
            conn_user = None
            conn_host = None
            conn_port = None
            conn_password = None
        else:
            conn_established = True
            conn_dbname = self.conn.dbname
            conn_user = self.conn.user
            conn_host = self.conn.host
            conn_port = self.conn.port
            conn_password = self.conn.password            

        fields = {
            'conn_established': conn_established,
            'conn_dbname': conn_dbname,
            'conn_user': conn_user,
            'conn_host': conn_host,
            'conn_port': conn_port,
            'conn_password': conn_password,
            'empty_count': self.empty_count,
            'batches': self.batches,
            'statuses': self.statuses,
            'game_positions': self.game_positions,
            'features': self.features if self.features == None else [f.id for f in self.features],
            'patterns': self.patterns if self.patterns == None else [p.id for p in self.patterns],
            'flabel_dict': self.flabel_dict,
            'feature_values': self.feature_values,
            'indexes': self.indexes,
            'plabel_dict_i0': self.plabel_dict_i0,
            'plabel_dict_i1': self.plabel_dict_i1,
            'gpdf': self.gpdf,
            'vmap': self.vmap,
            'ivmap': self.ivmap,
            'gpxpidf': self.gpxpidf,
            'x': self.x,
            'xt': self.xt,
            'y': self.y,
            'yh': self.yh,
            'w': self.w,
            'r': self.r,
            'opt_res': self.opt_res,
            'wmeans': self.wmeans,
            'evmap': self.evmap,
            'ievmap': self.ievmap,
            'ew': self.ew,
            'vld_batches': self.vld_batches,
            'vld_statuses': self.vld_statuses,
            'vld_game_positions': self.vld_game_positions,
            'vld_feature_values': self.vld_feature_values,
            'vld_indexes': self.vld_indexes,
            'vld_gpdf': self.vld_gpdf,
            'vld_gpxpidf': self.vld_gpxpidf,
            'vld_x': self.vld_x,
            'vld_xt': self.vld_xt,
            'vld_y': self.vld_y,
            'vld_yh': self.vld_yh,
            'vld_r': self.vld_r,
            'vld_summary': self.vld_summary,
        }
        
        f = open(fp, 'wb')
        pickle.dump(fields, f)
        f.close()

    @classmethod
    def load(cls, fp) -> Rglm:
        f = open(fp, 'rb')
        fields = pickle.load(f)
        f.close()

        mw = Rglm()

        conn_established = fields['conn_established']
        if conn_established:
            conn_dbname = fields['conn_dbname']
            conn_user = fields['conn_user']
            conn_host = fields['conn_host']
            conn_port = fields['conn_port']
            conn_password = fields['conn_password']
            mw.conn = RegabDBConnection(dbname=conn_dbname, user=conn_user, host=conn_host, port=conn_port, password=conn_password)
        else:
            mw.conn = None
        
        mw.empty_count = fields['empty_count']
        mw.batches = fields['batches']
        mw.vld_batches = fields['vld_batches']
        mw.statuses = fields['statuses']
        mw.vld_statuses = fields['vld_statuses']
        mw.game_positions = fields['game_positions']
        mw.vld_game_positions = fields['vld_game_positions']
        features = fields['features']
        mw.features = None if features == None else [features_as_list[i] for i in features]
        mw.flabel_dict = fields['flabel_dict']
        patterns = fields['patterns']
        mw.patterns = None if patterns == None else [patterns_as_list[i] for i in patterns]
        mw.feature_values = fields['feature_values']
        mw.vld_feature_values = fields['vld_feature_values']
        mw.indexes = fields['indexes']
        mw.vld_indexes = fields['vld_indexes']
        mw.plabel_dict_i0 = fields['plabel_dict_i0']
        mw.plabel_dict_i1 = fields['plabel_dict_i1']
        mw.gpdf = fields['gpdf']
        mw.vld_gpdf = fields['vld_gpdf']
        mw.vmap = fields['vmap']
        mw.ivmap = fields['ivmap']
        mw.gpxpidf = fields['gpxpidf']
        mw.x = fields['x']
        mw.xt = fields['xt']
        mw.y = fields['y']
        mw.yh = fields['yh']
        mw.w = fields['w']
        mw.r = fields['r']
        mw.opt_res = fields['opt_res']
        mw.wmeans = fields['wmeans']
        mw.evmap = fields['evmap']
        mw.ievmap = fields['ievmap']
        mw.ew = fields['ew']
        mw.vld_gpxpidf = fields['vld_gpxpidf']
        mw.vld_x = fields['vld_x']
        mw.vld_xt = fields['vld_xt']
        mw.vld_y = fields['vld_y']
        mw.vld_yh = fields['vld_yh']
        mw.vld_r = fields['vld_r']
        mw.vld_summary = fields['vld_summary']
        
        return mw

test_run_0 = {'cfg_fname': 'cfg/regab.cfg',
              'env': 'test',
              'ec': 20,
              'batches': [6],
              'vld_batches': [5],
              'statuses': 'CMR,CMS',
              'vld_statuses': 'CMR,CMS',
              'features': 'INTERCEPT,MOBILITY3',
              'patterns': 'EDGE,DIAG3',
              'ridge_reg_param': 0.1,
              'l_bfgs_b_options': {'disp': False,
                                   'maxcor': 50,
                                   'ftol': 1e-08,
                                   'gtol': 1e-05,
                                   'eps': 1e-08,
                                   'maxfun': 5000,
                                   'maxiter': 5000,
                                   'iprint': 1,
                                   'maxls': 20,
                                   'finite_diff_rel_step': None},
              }

test_run_a2050 = {'cfg_fname': 'cfg/regab.cfg',
                  'env': 'test',
                  'ec': 20,
                  'batches': [3],
                  'vld_batches': [5],
                  'statuses': 'CMR,CMS',
                  'vld_statuses': 'CMR,CMS',
                  'features': 'INTERCEPT,MOBILITY3',
                  'patterns': 'XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR',
                  'ridge_reg_param': 0.01,
                  'l_bfgs_b_options': {'disp': True,
                                       'maxcor': 50,
                                       'ftol': 1e-08,
                                       'gtol': 1e-05,
                                       'eps': 1e-08,
                                       'maxfun': 5000,
                                       'maxiter': 5000,
                                       'iprint': 1,
                                       'maxls': 20,
                                       'finite_diff_rel_step': None},
                  }

test_run_t2030 = {'cfg_fname': 'cfg/regab.cfg',
                  'env': 'test',
                  'ec': 20,
                  'batches': [3],
                  'vld_batches': [6],
                  'statuses': 'CMR,CMS',
                  'vld_statuses': 'CMR,CMS',
                  'features': 'INTERCEPT,MOBILITY3',
                  'patterns': 'EDGE',
                  'ridge_reg_param': 0.01,
                  'l_bfgs_b_options': {'disp': True,
                                       'maxcor': 50,
                                       'ftol': 1e-08,
                                       'gtol': 1e-05,
                                       'eps': 1e-08,
                                       'maxfun': 5000,
                                       'maxiter': 5000,
                                       'iprint': 1,
                                       'maxls': 20,
                                       'finite_diff_rel_step': None},
                  }

test_run_t2037 = {'cfg_fname': 'cfg/regab.cfg',
                  'env': 'test',
                  'ec': 20,
                  'batches': [3],
                  'vld_batches': [6],
                  'statuses': 'CMR,CMS',
                  'vld_statuses': 'CMR,CMS',
                  'features': 'INTERCEPT,MOBILITY3',
                  'patterns': '2X5COR',
                  'ridge_reg_param': 0.01,
                  'l_bfgs_b_options': {'disp': True,
                                       'maxcor': 50,
                                       'ftol': 1e-08,
                                       'gtol': 1e-05,
                                       'eps': 1e-08,
                                       'maxfun': 5000,
                                       'maxiter': 5000,
                                       'iprint': 1,
                                       'maxls': 20,
                                       'finite_diff_rel_step': None},
                  }

test_run_t2099 = {'cfg_fname': 'cfg/regab.cfg',
                  'env': 'test',
                  'ec': 20,
                  'batches': [3],
                  'vld_batches': [6],
                  'statuses': 'CMR,CMS',
                  'vld_statuses': 'CMR,CMS',
                  'features': 'INTERCEPT,MOBILITY3',
                  'patterns': '2X6COR',
                  'ridge_reg_param': 0.01,
                  'l_bfgs_b_options': {'disp': True,
                                       'maxcor': 50,
                                       'ftol': 1e-08,
                                       'gtol': 1e-05,
                                       'eps': 1e-08,
                                       'maxfun': 5000,
                                       'maxiter': 5000,
                                       'iprint': 1,
                                       'maxls': 20,
                                       'finite_diff_rel_step': None},
                  }

def rglm_workflow(kvargs: dict):

    def get_key(key: str):
        if key in kvargs:
            return kvargs[key]
        else:
            raise ValueError('Key {} is missing from the kvargs dictionary.'.format(key))

    def get_key_if_exists(key: str):
        if key in kvargs:
            return kvargs[key]
        else:
            return None

    cfg_fname = get_key('cfg_fname')
    env = get_key('env')
    ec = get_key('ec')
    batches = get_key('batches')
    vld_batches = get_key('vld_batches')
    statuses = get_key('statuses')
    vld_statuses = get_key('vld_statuses')
    features = get_key('features')
    patterns = get_key('patterns')
    ridge_reg_param = get_key_if_exists('ridge_reg_param')
    l_bfgs_b_options = get_key_if_exists('l_bfgs_b_options')
    
    conn = RegabDBConnection.new_from_config(cfg_fname, env)
    
    def timed_run(m, s, *args):
        sw = StopWatch()
        sw.start()
        ret = m(*args)
        sw.stop()
        cs = s.format(*args)
        print("{:<80} {}".format(cs, sw.get_elapsed_time_as_td()))
        return ret

    m = timed_run(Rglm, "m = Rglm()")
    m = timed_run(m.set_conn, "m = m.set_conn({})", conn)
    m = timed_run(m.set_empty_count, "m = m.set_empty_count({})", ec)
    m = timed_run(m.set_batches, "m = m.set_batches({})", batches)
    m = timed_run(m.set_vld_batches, "m = m.set_vld_batches({})", vld_batches)
    m = timed_run(m.set_statuses, "m = m.set_statuses({})", statuses)
    m = timed_run(m.set_vld_statuses, "m = m.set_vld_statuses({})", vld_statuses)
    m = timed_run(m.retrieve_game_positions, "m = m.retrieve_game_positions()")
    m = timed_run(m.retrieve_vld_game_positions, "m = m.retrieve_vld_game_positions()")
    m = timed_run(m.set_features, "m = m.set_features({})", features)
    m = timed_run(m.set_patterns, "m = m.set_patterns({})", patterns)
    m = timed_run(m.compute_feature_values, "m = m.compute_feature_values()")
    m = timed_run(m.compute_vld_feature_values, "m = m.compute_vld_feature_values()")
    m = timed_run(m.compute_indexes, "m = m.compute_indexes()")
    m = timed_run(m.compute_vld_indexes, "m = m.compute_vld_indexes()")
    m = timed_run(m.combine_gps_features_patterns, "m = m.combine_gps_features_patterns()")
    m = timed_run(m.combine_vld_gps_features_patterns, "m = m.combine_vld_gps_features_patterns()")
    m = timed_run(m.compute_vmaps, "m = m.compute_vmaps()")
    m = timed_run(m.compute_gpxpidf, "m = m.compute_gpxpidf()")
    m = timed_run(m.compute_x, "m = m.compute_x()")
    m = timed_run(m.compute_y, "m = m.compute_y()")
    m = timed_run(m.compute_analytics, "m = m.compute_analytics()")
    m = timed_run(m.retrieve_expected_probabilities_from_regab_db, "m = m.retrieve_expected_probabilities_from_regab_db()")
    m = timed_run(m.optimize, "m = m.optimize({}, {{...}})", ridge_reg_param, l_bfgs_b_options)
    if l_bfgs_b_options is not None:
        print("   l_bfgs_b_options = {}".format(l_bfgs_b_options))
    m = timed_run(m.compute_wmean_for_patterns, "m = m.compute_wmean_for_patterns()")
    m = timed_run(m.compute_evmap, "m = m.compute_evmap()")
    m = timed_run(m.compute_ievmap, "m = m.compute_ievmap()")
    m = timed_run(m.compute_vld_gpxpidf, "m = m.compute_vld_gpxpidf()")
    m = timed_run(m.compute_vld_x, "m = m.compute_vld_x()")
    m = timed_run(m.compute_vld_y, "m = m.compute_vld_y()")
    m = timed_run(m.validate, "m = m.validate()")

    return m

def rglm_sigmoid(x: np.ndarray) -> np.ndarray:
    """
    Computes the sigmoid (logistic) function on the given array of float values.
    """
    return 1. / (1. + np.exp(-x))

def rglm_gv_to_gvt(gv: np.ndarray) -> np.ndarray:
    """
    Applies a linear transformation to the game value (GV), that belongs to the range
    [-64..+64] of even integer numbers, or to the same range of float values, to the
    range [0.01..0.99] of float values (GVT - Game Value Transformed).
    The function works on scalar values as well as on ndarays.

    The gap between the two bounds 0 and 1 and the values mapping -64 and +64
    is choosen to be 0.01, leading to -64 mapped to 0.01 and +64 to 0.99.
    The value 0.1 for the gap has been tested as well, giving very similar results.
    0.01 is mapping values over 40 to be quite sensibly out of the linear part of the
    sigmoid function, on the other side 0.1 has values up to 50 still quite in the linear
    region. Anyhow the residual of the optimization, when trasformed in the range [-64..64]
    are very similar.

    gvt  = a *  gv + b
    0.01 = a * -64 + b
    0.99 = a * +64 + b

    Summing the two equations: 1    =   2b -> b = 0.5
    Subctracting them:         0.98 = 128a -> a = 0.98/128 = 0.49/64 = 0.00765625

    gvt = 0.00765625 * gv + 0.5

    """
    return 0.00765625 * gv + 0.5

def rglm_gvt_to_gv(gvt: np.ndarray) -> np.ndarray:
    """
    Reverts back the game value transformed (GVT) to the game value represented in
    its standard form of disc difference.
    The returned value is not rounded and so it is not an even integer.
    The function works on scalar values as well as on ndarays.

    gvt  = a *  gv + b
    a = 0.49/64 = 0.00765625
    b = 0.5

    gv = (gvt - b) / a

    1/a = 64 / 0.49 = 130.612244897959

    gv = 130.612244897959 * (gvt - .5)

    """
    return 130.612244897959 * (gvt - .5)

def rglm_round_gv(gv: np.ndarray) -> np.ndarray:
    """
    Rounds the game float value to the nearest even integer.
    The function works on scalar values as well as on ndarays.
    """
    return (np.round(0.5 * gv) * 2).astype(int)

import time

class StopWatch:
    """
    Collects the time spent by a program execution between the start and stop statements.
    """
    def __init__(self):
        """
        Sets to None all the relevant attributes.
        """
        self._start_time = None
        self._stop_time = None
        self._status = 0
        self._elapsed_time = 0

    def start(self):
        if not self._status == 0:
            raise Exception('Cannot call start on a running stop-watch')
        self._status = 1
        self._stop_time = None
        self._start_time = time.perf_counter_ns()

    def stop(self):
        if not self._status == 1:
            raise Exception('Cannot call stop on a stopped stop-watch')
        self._stop_time = time.perf_counter_ns()
        self._status = 0
        self._elapsed_time += self._stop_time - self._start_time

    def reset(self):
        if not self._status == 0:
            raise Exception('Cannot call reset on a running stop-watch')        
        self._start_time = None
        self._stop_time = None
        self._elapsed_time = 0        
        
    def get_status(self):
        return self._status

    def get_start_time(self):
        return self._start_time

    def get_stop_time(self):
        return self._stop_time
    
    def get_elapsed_time(self):
        return self._elapsed_time

    def get_elapsed_time_as_td(self):
        return pd.Timedelta(self.get_elapsed_time(), unit='ns')

def unix_time_now() -> int:
    presentDate = datetime.datetime.utcnow()
    unix_timestamp = datetime.datetime.timestamp(presentDate)*1000
    unix_timestamp_int = int(unix_timestamp)
    return unix_timestamp_int
