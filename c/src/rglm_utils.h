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
 * @copyright 2019, 2020, 2022 Roberto Corradini. All rights reserved.
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

/**
 * @brief Computes the game evaluation at given depth for the given board.
 *
 * @details Returns the game value of the board `b` given the
 *          RGLM model weights `mw`.
 *          The depth o the alpha-beta search is given by the empty_count
 *          difference between the board and the model_weights.
 *
 * @invariant Parameter `mw` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * The invariant is guardegd by an assertion.
 *
 * @param  [in] mw the model weights data structure
 * @param  [in] b  the game board
 * @return         the game value of the board
 */
extern double
rglmut_eval_gp_negascout (const board_t *const b,
                          const rglmdf_model_weights_t *const mw);

/**
 * @brief Computes the game evaluation for the given board.
 *
 * @details Returns the game value of the board `b` given the
 *          RGLM model weights `mw`.
 *
 * @invariant Parameter `mw` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * The invariant is guardegd by an assertion.
 *
 * @param  [in] mw the model weights data structure
 * @param  [in] b  the game board
 * @return         the game value of the board
 */
extern double
rglmut_eval_gp_using_model_weights (const rglmdf_model_weights_t *const mw,
                                    const board_t *const b);

/**
 * @brief Computes the value of the logistic function.
 *
 * @details Returns the value of the function `1 / ( 1 + pow(e, -x))`,
 *          where `x` is the independent variable in `[-INF:+INF]`, `e` is
 *          the Euler constant, and `pow()` id the power function.
 *          The function is pure.
 *
 * @param  [in] x the value of the independent variable
 * @return        the value of the logistic function
 */
extern double
rglmut_logistic_function (double x);

/**
 * @brief Computes the game value scaling operation.
 *
 * @details Returns the value of the function `0.49 * v + 0.5`,
 *          where `v` is the independent variable in `[-64:+64]`.
 *          The function projects the range `[-64:+64]` into `[0.01:0.99]`.
 *          The function is pure.
 *
 * @param  [in] v the value of the independent variable
 * @return        the mapped value
 */
extern double
rglmut_gv_scale (int v);

/**
 * @brief Computes the game value scaling back operation.
 *
 * @details Reverts the game value to the scale `[-64.:+64.]`
 *          without rounding to the nearest even integer.
 *          The function is pure.
 *
 * @param  [in] v the value of the independent variable
 * @return        the mapped value
 */
extern double
rglmut_gv_scale_back_f (const double v);

/**
 * @brief Computes the game value scaling back operation.
 *
 * @details Reverts the game value to the scale `[-64:+64]`
 *          rounding to the nearest even integer.
 *          The function is pure.
 *
 * @param  [in] v the value of the independent variable
 * @return        the mapped value
 */
extern int
rglmut_gv_scale_back_i (const double v);

/**
 * @brief Initializes the vector `v`.
 *
 * @details The game values taken from the game positions contained int the
 *          general data structure are copied into the vector `v`.
 *
 * @invariant Parameter `data` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `emme` must be equal to the count of tuples of game positions
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `v` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  data  reference to the general data structure
 * @param [in]  emme  the size of the vector v
 * @param [out] v     the initialized vector
 */
extern void
rglmut_gv_init (const rglmdf_general_data_t *data,
                size_t emme,
                double *v);

/**
 * @brief Computes the evaluation function and stores values into the vector `e`.
 *
 * @details The evaluation function is computed for all the game positions contained into the
 *          general data structure.
 *          The value is obtained as the dot product of the weights vector `w` and the entity values
 *          vector derived from the general data structure.
 *
 * @invariant Parameter `data` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `enne` must be equal to the count of independent variables
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `w` must be not `NULL` when `enne` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `emme` must be equal to the count of tuples of game positions
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `e` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  data  reference to the general data structure
 * @param [in]  enne  the size of the vector w
 * @param [in]  w     the vector of weights
 * @param [in]  emme  the size of the vector e
 * @param [out] e     the computed vector of game values
 */
extern void
rglmut_evaluation_function_eval (const rglmdf_general_data_t *data,
                                 size_t enne,
                                 const double *w,
                                 size_t emme,
                                 double *e);

/**
 * @brief Computes the derivative function and stores values into the vector `de`.
 *
 * @details The vector `e` must contain the game evaluation computed as the logistic function
 *          of the linear combination of the feature/pattern using the weight vector.
 *
 * @invariant Parameter `e` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `de` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  emme  the size of the vectors e and de
 * @param [in]  e     the vector of game evaluations
 * @param [out] de    the computed vector of derivative values
 */
extern void
rglmut_evaluation_function_derivative_eval (size_t emme,
                                            const double *e,
                                            double *de);

/**
 * @brief Computes the residual value.
 *
 * @details The function computes the difference `r = v - e`.
 *
 * @invariant Parameters `e`, `v`, and `r` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  emme the size of the vectors e, v and r
 * @param [in]  e    the vector of game evaluations
 * @param [in]  v    the vector of exact game values
 * @param [out] r    the residual vector
 */
extern void
rglmut_residual_value_eval (size_t emme,
                            const double *e,
                            const double *v,
                            double *r);

/**
 * @brief Computes the right hand side vector of the Newton equation.
 *
 * @details The function computes the gradient of the evaluation function with the sign changed.
 *
 * @invariant Parameter `data` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `enne` must be equal to the count of independent variables
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `minus_grad_f` must be not `NULL` when `enne` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `emme` must be equal to the count of tuples of game positions
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `r` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `de` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  data         reference to the general data structure
 * @param [out] minus_grad_f the computed vector
 * @param [in]  enne         the size of vector minus_grad_f
 * @param [in]  emme         the size of the vector r and de
 * @param [in]  r            the residual vector
 * @param [in]  de           the derivative vector
 */
extern void
rglmut_minus_grad_f_eval (const rglmdf_general_data_t *data,
                          double *minus_grad_f,
                          size_t enne,
                          size_t emme,
                          const double *r,
                          const double *de);

/**
 * @brief Computes the upper right triangle of the Hessian matrix.
 *
 * @details The Hessian matrix, here named Big B is a square matrix of size `(enne x enne)`.
 *          It is computed having the vectors `e`, `de`, and `v` of size `emme`.
 *          The vector `e` is the evaluation of the game position.
 *          The vector `de` is the derivative of the evaluation.
 *          The vector `v` is the true game value.
 *
 * @invariant Parameter `data` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `enne` must be equal to the count of independent variables
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `big_b` must be not `NULL` when `enne` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `emme` must be equal to the count of tuples of game positions
 * held by the general data structure.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `e` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `de` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `v` must be not `NULL` when `emme` is not zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  data  reference to the general data structure
 * @param [out] big_b the computed vector
 * @param [in]  enne  the size of vector minus_grad_f
 * @param [in]  emme  the size of the vector r and de
 * @param [in]  e     the game evaluation vector
 * @param [in]  de    the derivative vector
 * @param [in]  v     the game true value vector
 */
extern void
rgmlut_big_b_eval (const rglmdf_general_data_t *data,
                   double **big_b,
                   size_t enne,
                   size_t emme,
                   const double *e,
                   const double *de,
                   const double *v);

#endif /* RGLM_UTILS_H */
