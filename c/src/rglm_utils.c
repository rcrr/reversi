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
 * @copyright 2019 Roberto Corradini. All rights reserved.
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
      //printf("j, glm_variable_id[j], w[glm_variable_id[j]: %zu, %d, %f\n", j, glm_variable_id[j], w[glm_variable_id[j]]);
      sum += w[glm_variable_id[j]];
    }
    e[i] = rglmut_logistic_function(sum);
    //if (1) abort();
  }
}

void
rglmut_evaluation_function_derivative_eval (size_t emme,
                                            double *e,
                                            double *de)
{
  assert(e);
  assert(de);

  for (size_t i = 0; i < emme; i++) de[i] = e[i] * (1 - e[i]);
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

  for (size_t i = 0; i < emme; i++) r[i] = e[i] - v[i];
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
      minus_grad_f[glm_variable_id[j]] -= z;
  }
}

void
rgmlut_big_b_eval (rglmdf_general_data_t *data,
                   double **big_b,
                   size_t enne,
                   size_t emme,
                   double *e,
                   double *de,
                   double *r)
{
  assert(data);
  assert(big_b);
  assert(emme == data->positions.ntuples);
  assert(enne == data->pattern_freq_summary.ntuples);
  assert(de);

  size_t idi, idj;

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
    for (size_t i = 0; i < nq; i++)
      for (size_t j = 0; j < nq; j++) {
        idi = glm_variable_id[i];
        idj = glm_variable_id[j];
        //if (idj >= idi) big_b[idi][idj] += de[k] * de[k]; // GAUSS-NEWTON
        if (idj >= idi) big_b[idi][idj] += (de[k] * de[k]) + (r[k] * e[k] * (1 - e[k]) * (1 - 2 * e[k])); // NEWTON
      }
  }
}

void
rglmut_dedwk_equation (int *result,
                       rglmdf_general_data_t *data,
                       double *f,
                       double *w,
                       double **jacobian)
{
  assert(result);
  assert(data);
  assert(f);

  /*
   * i: index over glm variables
   * j: index over game positions
   * k: index over equations
   * q: column index in the jacobian matrix
   */
  size_t i, j, k, q, idx, idy;

  /* Vector of scaled game values, and the link function. */
  double *r, *g;

  /* Service variables. */
  double sum, expression;

  /* Vector of glm variables id belonging to a solved and classified game position. */
  uint32_t *glm_variable_id;

  /* nk: number of equations. */
  const size_t nk = data->pattern_freq_summary.ntuples;
  //printf("nk=%zu\n", nk);

  /* ni: number of glm variables, same value as nk. */
  //const size_t ni = data->pattern_freq_summary.ntuples;
  //printf("ni=%zu\n", ni);

  /* nj: number of solved and classified game positions. */
  const size_t nj = data->positions.ntuples;
  //printf("nj=%zu\n", nj);

  /* nq: number of pattern instances considered by the model. */
  const size_t nq = data->positions.n_index_values_per_record;
  //printf("nq=%zu\n", nq);

  /* Computes the r vector, having the scaled game values. */
  r = (double *) malloc( nj * sizeof(double));
  if (!r) {
    *result = -1;
    return;
  }
  for (j = 0; j < nj; j++) r[j] = rglmut_gv_scale(data->positions.records[j].game_value);
  //for (j = 0; j < nj; j++) printf("gv=%d, r=%f\n", data->positions.records[j].game_value, r[j]);

  /* Allocates the g vector. */
  g = (double *) malloc( nj * sizeof(double));
  if (!g) {
    free(r);
    *result = -2;
    return;
  }

  /* Computes the g function for each j index. */
  for (j = 0; j < nj; j++) {
    glm_variable_id = &data->positions.iarray[j * nq];
    sum = 0.0;
    for (i = 0; i < nq; i++) {
      idx = glm_variable_id[i];
      sum += w[idx];
      //printf("j=%zu; glm_variable_id[%zu]=%zu, sum=%f\n", j, i, idx, sum);
    }
    g[j] = rglmut_logistic_function(sum);
  }

  /*
  for (j = 0; j < nj; j++) {
    printf("g[%4zu]=%f\n", j, g[j]);
  }
  */

  /* Initializes the f vector. */
  for (k = 0; k < nk; k++) f[k] = 0.0;

  /* Computes the f vector. */
  for (j = 0; j < nj; j++) {
    glm_variable_id = &data->positions.iarray[j * nq];
    for (k = 0; k < nq; k++) {
      idx = glm_variable_id[k];
      f[idx] += (r[j] - g[j]) * g[j] * (1 - g[j]); // This is -f NOT f ...
    }
  }

  /*
  for (k = 0; k < nk; k++) {
    printf("f[%zu]=%f\n", k, f[k]);
  }
  */

  /* Initializes the upper triangle of the Jacobian matrix. */
  for (k = 0; k < nk; k++)
    for (q = k; q < nk; q++)
      jacobian[k][q] = 0.0;

  /* Computes the upper triangle of the Jacobian matrix. */
  for (j = 0; j < nj; j++) {
    glm_variable_id = &data->positions.iarray[j * nq];
    expression = - ((3.0 * g[j] * g[j]) - 2.0 * (r[j] + 1) * g[j] + r[j]) * g[j] * (1.0 - g[j]);
    if (expression < 0.0) {
      printf("expression[%4zu]=%f, g[j]=%f, r[j]=%f\n", j, expression, g[j], r[j]);
      abort();
    }
    for (k = 0; k < nq; k++) {
      for (q = 0; q < nq; q++) {
        idx = glm_variable_id[k];
        idy = glm_variable_id[q];
        if (idy >= idx) jacobian[idx][idy] += expression;
      }
    }
  }

  /*
  for (k = 0; k < nk; k++) {
    for (q = 0; q < nk; q++) {
      printf("jacobian[%zu][%zu]=%f\n", k, q, jacobian[k][q]);
    }
  }
  */

  free(g);
  free(r);
  *result = 0;
}
