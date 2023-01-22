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

#
# To do:
#
# -0- [done] Implements the Ridge regularization.
#
# -1- Add one more Feature ( MOBILITYX ) and one more Pattern ( 2X6COR da 12 !!! ).
#
# -2- Introduce the Lasso regularization.
#     Here we do not have a solution yet, options are:
#     . Use the (Orthant-Wise Limited-memory Quasi-Newton (OWL-QN) method: https://www.chokkan.org/software/liblbfgs/
#     . "double the variables" and use the constraint version L-BFGS-C of the algorithm
#     . Try something more fancy ...
#
# -3- More data analytics.
#     [done] Parameters profiling ... each variable/weight should have min, max, mean, std game values.
#     There should be a second data set for validation.
#        Actions:
#        . Add the vid column to evmap, could we name it evid (expanded variable id) to make it clear it is a different thing. 
#        . Add e new function compute_vld_gpxpidf
#        . Add a new function compute_vld_x
#        . Finally compute logit(vld_x @ ew) as the vld_yh. Compare it with vld_y ...
#     There should be a final report ... with KPI used to compare different models.
#     Prepare an info(verbosity) method that give back Model Info.
#
# -4- [done] Setup a kind of workflow.
#            rglm_test() should be renamed to something like ... execute work-flow
#            input data should be provided by means of a dictionary
#
# -5- Read and write to file ...
#
#     In practice we need to build the REGAB/RGML machinery in a different way.
#
# -6- Complete documentation ....
#
# -7- Code tests ....
#

def rgml_test(m):
    print('rglm_test')

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
        Initit all the relevant attributes.
        """
        self.conn = None
        self.empty_count = None
        self.batches = None
        self.vld_batches = None
        self.statuses = None
        self.vld_statuses = None
        self.game_positions = None
        self.vld_game_positions = None
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
        
        self._mover_field = 'mover'
        self._opponent_field = 'opponent'

    import inspect
    
    def set_conn(self, conn: RegabDBConnection) -> Rglm:
        if not isinstance(conn, RegabDBConnection):
            raise TypeError('Argument conn is not an instance of RegabDBConnection')
        self.conn = conn
        self.status = 'CONN'
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
        gps = regab_gp_as_df(self.conn, batches, statuses, self.empty_count, limit, where, fields)
        cols = gps.columns
        gps['gpid'] = gps.index
        for c in ['gpid'] + list(cols):
            gps[c] = gps.pop(c)
        return gps

    def retrieve_game_positions(self, limit=None, where=None, fields=None) -> Rglm:
        """
        Retrieves game positions from the REGAB database.
        """
        gps = self._retrieve_game_positions(self.batches, self.statuses, limit, where, fields)
        self.game_positions = gps
        return self

    def retrieve_vld_game_positions(self, limit=None, where=None, fields=None) -> Rglm:
        """
        Retrieves validation game positions from the REGAB database.
        """
        gps = self._retrieve_game_positions(self.vld_batches, self.vld_statuses, limit, where, fields)
        self.vld_game_positions = gps
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
        self.feature_values, self.flabel_dict = compute_feature_values_on_df(self.game_positions, self.features, mover=self._mover_field, opponent=self._opponent_field)
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
        self.indexes, self.plabel_dict_i0, self.plabel_dict_i1 = compute_indexes_on_df(self.game_positions, self.patterns, mover=self._mover_field, opponent=self._opponent_field)
        self.vld_indexes, unused_0, unused_1 = compute_indexes_on_df(self.vld_game_positions, self.patterns, mover=self._mover_field, opponent=self._opponent_field)
        return self

    def combine_gps_features_patterns(self) -> Rglm:
        """
        Computes the gpdf data frame, by concatenating game_positions, feature values, and pattern indexes.
        """
        self.gpdf = pd.concat([self.game_positions, self.feature_values, self.indexes], axis=1, copy=False)
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
        
        # Retrieve the expected probabilities (eprob) from the REGAB database, then merge (join) vmap with the extraction (df_b).
        # eprobs for features are set to 1.
        df_b = regab_patternlist_probs_as_df(rc=self.conn, patterns=self.patterns, ec=self.empty_count, is_principal=True)
        df_b = df_b.rename(columns={'pattern_id': 'eid', 'principal': 'idx', 'probs': 'eprobs'})
        df_b['etype'] = 1
        self.vmap = pd.merge(self.vmap, df_b, how='left', on=['etype', 'eid', 'idx'])
        self.vmap.loc[self.vmap['etype'] == 0, 'eprobs'] = 1.

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
        self.wmeans = dict.fromkeys(self.patterns)
        df = self.vmap[self.vmap['etype'] == 1][['eid','oprobs','weight']]
        wmeandf = df.assign(wmean=df.oprobs*df.weight).groupby('eid', as_index=False).wmean.sum()
        wmeandf = wmeandf.astype({'eid':'int','wmean':'float'})
        for row in wmeandf.itertuples(index=False):
            eid, wmean = row
            p = patterns_as_list[eid]
            self.wmeans[p] = wmean
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
            p_wmean = np.repeat(self.wmeans[p], (p.n_configurations))
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
        self.evmap = self.evmap.merge(self.vmap[['etype', 'eid', 'idx', 'weight']],
                                                  how='left',
                                                  left_on=['etype', 'eid', 'pidx'],
                                                  right_on=['etype', 'eid', 'idx'])
        self.evmap.rename(columns={'idx_x': 'idx'}, inplace=True)
        self.evmap['computed'] = ~pd.isna(self.evmap["idx_y"])
        self.evmap.weight.fillna(self.evmap.wmean, inplace=True)
        self.evmap.drop(columns=['idx_y'], inplace=True)
        self.evmap['evid'] = self.evmap.index
        self.evmap = self.evmap.loc[:, ['evid', 'etype', 'eid', 'idx', 'pidx', 'wmean', 'computed', 'weight']]
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
            labels = self.plabel_dict_i0[p]
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
    
    def validate(self):
        vld_linear_predictor = self.vld_x @ self.ew
        self.vld_yh = rglm_sigmoid(vld_linear_predictor)
        vld_gv = self.vld_game_positions['game_value'].to_numpy()
        vld_gvt = rglm_gv_to_gvt(vld_gv)
        self.vld_y = vld_gvt
        return self

test_run_0 = {'cfg_fname': 'cfg/regab.cfg',
              'env': 'test',
              'ec': 20,
              'batches': [6],
              'vld_batches': [5],
              'statuses': 'CMR,CMS',
              'vld_statuses': 'CMR,CMS',
              'features': 'INTERCEPT,MOBILITY',
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
    m = timed_run(m.compute_indexes, "m = m.compute_indexes()")
    m = timed_run(m.combine_gps_features_patterns, "m = m.combine_gps_features_patterns()")
    m = timed_run(m.compute_vmaps, "m = m.compute_vmaps()")
    m = timed_run(m.compute_gpxpidf, "m = m.compute_gpxpidf()")
    m = timed_run(m.compute_x, "m = m.compute_x()")
    m = timed_run(m.compute_y, "m = m.compute_y()")
    m = timed_run(m.compute_analytics, "m = m.compute_analytics()")
    m = timed_run(m.optimize, "m = m.optimize({}, {{...}})", ridge_reg_param, l_bfgs_b_options)
    if l_bfgs_b_options is not None:
        print("   l_bfgs_b_options = {}".format(l_bfgs_b_options))
    m = timed_run(m.compute_wmean_for_patterns, "m = m.compute_wmean_for_patterns()")
    m = timed_run(m.compute_evmap, "m = m.compute_evmap()")
    m = timed_run(m.compute_ievmap, "m = m.compute_ievmap()")
    m = timed_run(m.compute_vld_gpxpidf, "m = m.compute_vld_gpxpidf()")
    m = timed_run(m.compute_vld_x, "m = m.compute_vld_x()")
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
