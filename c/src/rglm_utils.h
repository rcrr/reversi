/**
 * @file
 *
 * @brief Reversi Generalized Linear Model utilities.
 * @details This module defines utility functions for the RGLM module.
 *
 * @par rglm_utils.h
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

#ifndef RGLM_UTILS_H
#define RGLM_UTILS_H

#include "rglm_data_files.h"

extern double
rglmut_logistic_function (double x);

extern double
rglmut_gv_scale (int v);

extern void
rglmut_gv_init (rglmdf_general_data_t *data,
                size_t emme,
                double *v);

extern void
rglmut_evaluation_function_eval (rglmdf_general_data_t *data,
                                 size_t enne,
                                 double *w,
                                 size_t emme,
                                 double *e);

extern void
rglmut_evaluation_function_derivative_eval (size_t emme,
                                            double *e,
                                            double *de);

extern void
rglmut_residual_value_eval (size_t emme,
                            double *e,
                            double *v,
                            double *r);

extern void
rglmut_minus_grad_f_eval (rglmdf_general_data_t *data,
                          double *minus_grad_f,
                          size_t enne,
                          size_t emme,
                          double *r,
                          double *de);

extern void
rgmlut_big_b_eval (rglmdf_general_data_t *data,
                   double **big_b,
                   size_t enne,
                   size_t emme,
                   double *e,
                   double *de,
                   double *r);

#endif /* RGLM_UTILS_H */
