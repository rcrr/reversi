/**
 * @file
 *
 * @brief Reversi Generalized Linear Model utility functions.
 *
 * @par rglm_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019, 2020 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "rglm_utils.h"

double
rglmut_logistic_function (const double x)
{
  const double e =  2.71828182845904523536;
  return (double) 1.0 / ((double) 1.0 + pow(e, -x));
}

double
rglmut_gv_scale (const int v)
{
  return (double) 0.00765625 * (double) v + (double) 0.5;
}

void
rglmut_gv_init (const rglmdf_general_data_t *data,
                const size_t emme,
                double *const v)
{
  assert(data);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) v  : true);

  const rglmdf_solved_and_classified_gp_record_t *const records = rglmdf_get_positions_records(data);

  for (size_t i = 0; i < emme; i++)
    v[i] = rglmut_gv_scale(records[i].game_value);
}

void
rglmut_evaluation_function_eval (const rglmdf_general_data_t *const data,
                                 const size_t enne,
                                 const double *w,
                                 const size_t emme,
                                 double *const e)
{
  assert(data);
  assert(enne == rglmdf_get_entity_freq_summary_ntuples(data));
  assert(enne > 0 ? (bool) w  : true);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) e  : true);

  const int32_t *const i2array = rglmdf_get_positions_i2array(data);
  const double *const farray = rglmdf_get_positions_farray(data);

  /* ni: number of pattern instances considered by the model. */
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(data);

  /* nf: number of feature values considered by the model. */
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(data);

  for (size_t i = 0; i < emme; i++) {
    double sum = 0.0;
    /* Vector of feature values belonging to a solved and classified game position. */
    const double *const feature_values = &farray[i * nf];
    for (size_t j = 0; j < nf; j++) {
      sum += w[j] * feature_values[j];
    }
    /* Vector of glm variables id belonging to a solved and classified game position. */
    const int32_t *const glm_variable_ids = &i2array[i * ni];
    for (size_t j = 0; j < ni; j++) {
      sum += w[glm_variable_ids[j]];
    }
    e[i] = rglmut_logistic_function(sum);
  }
}

void
rglmut_evaluation_function_derivative_eval (const size_t emme,
                                            const double *const e,
                                            double *const de)
{
  assert(emme > 0 ? (bool) e && (bool) de : true);

  for (size_t i = 0; i < emme; i++)
    de[i] = e[i] * (1 - e[i]);
}

void
rglmut_residual_value_eval (const size_t emme,
                            const double *const e,
                            const double *const v,
                            double *const r)
{
  assert(emme > 0 ? (bool) e && (bool) v && (bool) r : true);

  for (size_t i = 0; i < emme; i++)
    r[i] = v[i] - e[i];
}

void
rglmut_minus_grad_f_eval (const rglmdf_general_data_t *const data,
                          double *const minus_grad_f,
                          const size_t enne,
                          const size_t emme,
                          const double *r,
                          const double *de)
{
  assert(data);
  assert(enne == rglmdf_get_entity_freq_summary_ntuples(data));
  assert(enne > 0 ? (bool) minus_grad_f : true);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) r && (bool) de : true);

  const int32_t *const i2array = rglmdf_get_positions_i2array(data);
  const double *const farray = rglmdf_get_positions_farray(data);

  /* ni: number of pattern instances considered by the model. */
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(data);

  /* nf: number of feature values considered by the model. */
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(data);

  /* Initializes the vector. */
  for (size_t k = 0; k < enne; k++) minus_grad_f[k] = 0.0;

  for (size_t i = 0; i < emme; i++) {
    /* Vector of feature values belonging to a solved and classified game position. */
    const double *const feature_values = &farray[i * nf];
    /* Vector of glm variables id belonging to a solved and classified game position. */
    const int32_t *const glm_variable_ids = &i2array[i * ni];
    const double z = r[i] * de[i];
    for (size_t j = 0; j < nf; j++)
      minus_grad_f[j] += z * feature_values[j];
    for (size_t j = 0; j < ni; j++)
      minus_grad_f[glm_variable_ids[j]] += z;
  }
}

void
rgmlut_big_b_eval (const rglmdf_general_data_t *const data,
                   double **const big_b,
                   const size_t enne,
                   const size_t emme,
                   const double *e,
                   const double *de,
                   const double *v)
{
  assert(data);
  assert(enne == rglmdf_get_entity_freq_summary_ntuples(data));
  assert(enne > 0 ? (bool) big_b : true);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) e && (bool) de && (bool) v : true);

  const int32_t *const i2array = rglmdf_get_positions_i2array(data);
  const double *const farray = rglmdf_get_positions_farray(data);

  /* ni: number of pattern instances considered by the model. */
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(data);

  /* nf: number of feature values considered by the model. */
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(data);

  /* Initializes the upper triangle of the B matrix. */
  for (size_t i = 0; i < enne; i++)
    for (size_t j = i; j < enne; j++)
      big_b[i][j] = 0.0;

  /* Computes the upper triangle of the B matrix. */
  for (size_t k = 0; k < emme; k++) {
    /* Vector of feature values belonging to a solved and classified game position. */
    const double *const feature_values = &farray[k * nf];
    /* Vector of glm variables id belonging to a solved and classified game position. */
    const int32_t *const glm_variable_ids = &i2array[k * ni];
    const double z = de[k] * (v[k] - 2*(1 + v[k])*e[k] + 3*e[k]*e[k]);

    for (size_t i = 0; i < nf; i++) {
      for (size_t j = i; j < nf; j++) {
        big_b[i][j] -= z * feature_values[i] * feature_values[j];
      }
      for (size_t j = 0; j < ni; j++) {
        const size_t idj = glm_variable_ids[j];
        big_b[i][idj] -= z * feature_values[i];
      }
    }
    for (size_t i = 0; i < ni; i++) {
      for (size_t j = 0; j < ni; j++) {
        const size_t idi = glm_variable_ids[i];
        const size_t idj = glm_variable_ids[j];
        if (idj >= idi) big_b[idi][idj] -= z;
      }
    }
  }
}
