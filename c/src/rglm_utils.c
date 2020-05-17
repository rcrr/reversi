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
rglmut_logistic_function (double x)
{
  static const double e =  2.71828182845904523536;
  return (double) 1.0 / ((double) 1.0 + pow(e, -x));
}

double
rglmut_gv_scale (int v)
{
  return (double) 0.00765625 * (double) v + (double) 0.5;
}

void
rglmut_gv_init (rglmdf_general_data_t *data,
                size_t emme,
                double *v)
{
  assert(data);
  assert(emme == data->positions.ntuples);

  for (size_t i = 0; i < emme; i++)
    v[i] = rglmut_gv_scale(data->positions.records[i].game_value);
}

void
rglmut_evaluation_function_eval (rglmdf_general_data_t *data,
                                 size_t enne,
                                 double *w,
                                 size_t emme,
                                 double *e)
{
  assert(data);
  assert(emme == data->positions.ntuples);
  assert(w);
  assert(enne == data->pattern_freq_summary.ntuples);
  assert(e);

  double sum;

  /* Vector of glm variables id belonging to a solved and classified game position. */
  uint32_t *glm_variable_id;

  /* nq: number of pattern instances considered by the model. */
  const size_t nq = data->positions.n_index_values_per_record;

  for (size_t i = 0; i < emme; i++) {
    glm_variable_id = &data->positions.iarray[i * nq];
    sum = 0.0;
    for (size_t j = 0; j < nq; j++) {
      sum += w[glm_variable_id[j]];
    }
    e[i] = rglmut_logistic_function(sum);
  }
}

void
rglmut_evaluation_function_derivative_eval (size_t emme,
                                            double *e,
                                            double *de)
{
  assert(e);
  assert(de);

  for (size_t i = 0; i < emme; i++)
    de[i] = e[i] * (1 - e[i]);
}

void
rglmut_residual_value_eval (size_t emme,
                            double *e,
                            double *v,
                            double *r)
{
  assert(e);
  assert(v);
  assert(r);

  for (size_t i = 0; i < emme; i++)
    r[i] = v[i] - e[i];
}

void
rglmut_minus_grad_f_eval (rglmdf_general_data_t *data,
                          double *minus_grad_f,
                          size_t enne,
                          size_t emme,
                          double *r,
                          double *de)
{
  assert(data);
  assert(emme == data->positions.ntuples);
  assert(enne == data->pattern_freq_summary.ntuples);
  assert(r);
  assert(de);

  double z;

  /* Vector of glm variables id belonging to a solved and classified game position. */
  uint32_t *glm_variable_id;

  /* nq: number of pattern instances considered by the model. */
  const size_t nq = data->positions.n_index_values_per_record;

  for (size_t k = 0; k < enne; k++) minus_grad_f[k] = 0.0;

  for (size_t i = 0; i < emme; i++) {
    glm_variable_id = &data->positions.iarray[i * nq];
    z = r[i] * de[i];
    for (size_t j = 0; j < nq; j++)
      minus_grad_f[glm_variable_id[j]] += z;
  }
}

void
rgmlut_big_b_eval (rglmdf_general_data_t *data,
                   double **big_b,
                   size_t enne,
                   size_t emme,
                   double *e,
                   double *de,
                   double *v)
{
  assert(data);
  assert(big_b);
  assert(emme == data->positions.ntuples);
  assert(enne == data->pattern_freq_summary.ntuples);
  assert(de);

  size_t idi, idj;
  double z;

  /* Vector of glm variables id belonging to a solved and classified game position. */
  uint32_t *glm_variable_id;

  /* nq: number of pattern instances considered by the model. */
  const size_t nq = data->positions.n_index_values_per_record;

  /* Initializes the upper triangle of the B matrix. */
  for (size_t i = 0; i < enne; i++)
    for (size_t j = i; j < enne; j++)
      big_b[i][j] = 0.0;

  /* Computes the upper triangle of the B matrix. */
  for (size_t k = 0; k < emme; k++) {
    glm_variable_id = &data->positions.iarray[k * nq];
    //z = de[k] * (de[k] + r[k] * (1 - 2 * e[k]));
    z = de[k] * (v[k] - 2*(1 + v[k])*e[k] + 3*e[k]*e[k]);
    for (size_t i = 0; i < nq; i++)
      for (size_t j = 0; j < nq; j++) {
        idi = glm_variable_id[i];
        idj = glm_variable_id[j];
        if (idj >= idi) big_b[idi][idj] -= z;
      }
  }
}
