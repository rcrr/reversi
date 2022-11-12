#
# rglm.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022 Roberto Corradini. All rights reserved.
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

#
# To do:
#
# -1- We need a Model class. This class is collecting:
#     .done. the REGAB query
#     .done. returned RESULT SET ( a pandas Data Frame )
#     .done. Feature/Pattern configuration
#     .done. data frame with computed indexes
#     .done. df computed features
#     .done. build a dictionary that maps gpdf columns to features and patterns
#     .done. build the vmap and the inverse vmap (ivmap) tables
#     .done. build the sparse matrix for patterns as a dataframe of integers
#     .done. X sparse matrix ( the X matrix )
#        Build the X and y numpy array.
#        X is a data frame having as row the game position and as columns the GLM variables.
#        X must be sparse. No way otherwise it is huge.
#        All columns must be float64.
#        y is an array having the expected value for the game position.
#        in case of a Logistic Regression y should be 0 or 1 dpending on value set as border between
#        win or lose.
#     .. Compute the effe and grad values for the scipy.optimize.minimize call
#     .. hyperparameters
#     .. computed weights
#
# -2- Setup a kind of workflow with states and checks ....
#
# -3- Read and write to file ...
#
#     In practice we need to build the REGAB/RGML machinery in a different way.
#
#     *** what happens when a pattern configuation is missing from the data ?
#     *** run many solutions of the logit model having a different win/lose boundary
#
# -4- Complete documentation ....
#
# -5- Code tests ....
#

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
      The feature object is the key, the value is a list of strings collecting the
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
    """
    def __init__(self):
        """
        Sets to None all the relevant attributes.
        """
        self.conn = None
        self.empty_count = None
        self.batches = None
        self.statuses = None
        self.features = None
        self.flabel_dict = None
        self.patterns = None
        self.plabel_dict_i0 = None
        self.plabel_dict_i1 = None
        self.game_positions = None
        self.indexes = None
        self.feature_values = None
        self.gpdf = None
        self.vmap = None
        self.ivmap = None
        self.gpxpidf = None
        self.x = None
        self.xt = None
        self.y = None
        self.yh = None
        self.w = None
        
        self._mover_field = 'mover'
        self._opponent_field = 'opponent'


    def set_conn(self, conn) -> 'Rglm':
        if not isinstance(conn, RegabDBConnection):
            raise TypeError('Argument conn is not an instance of RegabDBConnection')
        self.conn = conn
        return self

    def close_conn(self):
        if self.conn:
            self.conn.close()

    def set_empty_count(self, ec: int) -> 'Rglm':
        self.empty_count = regab_empty_count(ec)
        return self

    def set_batches(self, bs) -> 'Rglm':
        self.batches = regab_batches(bs)
        return self

    def set_statuses(self, sts) -> 'Rglm':
        self.statuses = regab_statuses(sts)
        return self

    def set_features(self, fs) -> 'Rglm':
        self.features = regab_features(fs)
        return self

    def set_patterns(self, ps) -> 'Rglm':
        self.patterns = regab_patterns(ps)
        return self

    def retrieve_game_positions(self, limit=None, where=None, fields=None) -> 'Rglm':
        gps = regab_gp_as_df(self.conn, self.batches, self.statuses, self.empty_count, limit, where, fields)
        cols = gps.columns
        gps['gpid'] = gps.index
        for c in ['gpid'] + list(cols):
            gps[c] = gps.pop(c)
        self.game_positions = gps
        return self

    def compute_indexes(self) -> 'Rglm':
        if not isinstance(self.game_positions, pd.DataFrame):
            raise TypeError('The field game_positions is not an instance of DataFrame')
        if not set([self._mover_field, self._opponent_field]).issubset(self.game_positions.columns):
            raise ValueError('The game_positions data frame is missing mover or opponent columns')
        if self.patterns is None or self.patterns == []:
            raise ValueError('The field patterns is not defined or empty')
        self.indexes, self.plabel_dict_i0, self.plabel_dict_i1 = compute_indexes_on_df(self.game_positions, self.patterns, mover=self._mover_field, opponent=self._opponent_field)
        return self

    def compute_feature_values(self) -> 'Rglm':
        if not isinstance(self.game_positions, pd.DataFrame):
            raise TypeError('The field game_positions is not an instance of DataFrame')
        if not set([self._mover_field, self._opponent_field]).issubset(self.game_positions.columns):
            raise ValueError('The game_positions data frame is missing mover or opponent columns')
        if self.features is None or self.features == []:
            raise ValueError('The field features is not defined or empty')
        self.feature_values, self.flabel_dict = compute_feature_values_on_df(self.game_positions, self.features, mover=self._mover_field, opponent=self._opponent_field)
        return self

    def combine_gps_features_patterns(self) -> 'Rglm':
        self.gpdf = pd.concat([self.game_positions, self.feature_values, self.indexes], axis=1, copy=False)
        return self

    def compute_vmaps(self) -> 'Rglm':
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
            labels = self.plabel_dict_i1[p]
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

    def compute_gpxpidf(self) -> 'Rglm':
        gpxpidf_colnames = ['vid', 'etype', 'eid', 'idx', 'gpid', 'counter']
        self.gpxpidf = pd.DataFrame(columns = gpxpidf_colnames, dtype = 'int64')
        for p in self.patterns:
            labels = self.plabel_dict_i1[p]
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

    def compute_x(self):
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

    def compute_y(self):
        gv = self.game_positions['game_value'].to_numpy()
        gvt = rglm_gv_to_gvt(gv)
        self.y = gvt
        self.yh = np.full(len(gvt), 0.5)
        return self


    def function(self):
        return 0.5 * sum((self.y - self.yh)**2)
    
    def gradient(self):
        return - self.xt @ ((self.yh * (1. - self.yh)) * (self.y - self.yh))

    def optimize(self):

        opts = {'disp': None,
                'maxcor': 40,
                'ftol': 1e-08,
                'gtol': 1e-05,
                'eps': 1e-08,
                'maxfun': 5000,
                'maxiter': 5000,
                'iprint': 99,
                'maxls': 20,
                'finite_diff_rel_step': None}

        self.w = np.full(len(self.vmap), 0.)

        def fg(w):
            linear_predictor = self.x @ w
            self.yh = rglm_sigmoid(linear_predictor)
            r = self.y - self.yh
            f = 0.5 * sum(r**2)
            g = - self.xt @ ((self.yh * (1. - self.yh)) * r)            
            return f, g
        
        result = minimize(fg, self.w, jac=True, method='L-BFGS-B', options=opts)
        return result
    
def rglm_test():

    cfg_fname = 'cfg/regab.cfg'
    env = 'test'
    ec = 20
    batches = [3]
    statuses = 'CMR,CMS'
    features = 'INTERCEPT,MOBILITY3'
    patterns = 'XEDGE,CORNER,R2,R3,R4,DIAG4,DIAG5,DIAG6,DIAG7,DIAG8,2X5COR'

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
    m = timed_run(m.set_statuses, "m = m.set_statuses({})", statuses)
    m = timed_run(m.retrieve_game_positions, "m = m.retrieve_game_positions()")
    m = timed_run(m.set_features, "m = m.set_features({})", features)
    m = timed_run(m.set_patterns, "m = m.set_patterns({})", patterns)
    m = timed_run(m.compute_indexes, "m = m.compute_indexes()")
    m = timed_run(m.compute_feature_values, "m = m.compute_feature_values()")
    m = timed_run(m.combine_gps_features_patterns, "m = m.combine_gps_features_patterns()")
    m = timed_run(m.compute_vmaps, "m = m.compute_vmaps()")
    m = timed_run(m.compute_gpxpidf, "m = m.compute_gpxpidf()")
    m = timed_run(m.compute_x, "m = m.compute_x()")
    m = timed_run(m.compute_y, "m = m.compute_y()")

    return m

def rglm_sigmoid(x: np.ndarray) -> np.ndarray:
    return 1. / (1. + np.exp(-x))

def rglm_gv_to_gvt(g: int) -> float:
    """
    Transforms the game value, that belongs to the range [-64..+64] of even numbers,
    to the range [0.01..0.99] of float values.
    """
    # a = 0.49/64
    a = 0.00765625
    b = 0.5
    t = a * g + b
    return t

def rglm_compute_grad(m: Rglm) -> np.ndarray:
    y = m.y
    yh = np.full(len(m.game_positions), 0.5)
    r = y - yh
    dg = yh * (1. - yh)
    tmp = dg * r
    x = m.x
    xt = x.transpose()
    grad = - xt @ tmp
    return grad

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
