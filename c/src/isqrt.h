/**
 * @file
 *
 * @brief Inverse Square Root functions definitions.
 * @details This module defines procedure to compute the inverse square
 * root mathematical function.
 *
 * @par isqrt.h
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

#ifndef ISQRT_H
#define ISQRT_H



/**
 * @brief Computes the inverse square root mathematical function.
 *
 * @details Returns the value of `1/sqrt(x)`.
 *
 * @invariant Parameter `x` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `e` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `x0` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  x  independent variable
 * @param [in]  e  epsilon
 * @param [in]  x0 initial estimation
 * @param [out] ee estimated error
 * @return         inverse square root of `x`
 */
extern double
isqrt_double_newton (double x,
                     double e,
                     double x0,
                     double *ee);

/**
 * @brief Computes the inverse square root mathematical function.
 *
 * @details Returns the value of `1/sqrt(x)`.
 *
 * @invariant Parameter `x` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `e` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `x0` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  x  independent variable
 * @param [in]  e  epsilon
 * @param [in]  x0 initial estimation
 * @param [out] ee estimated error
 * @return         inverse square root of `x`
 */
extern double
isqrt_double_halley (double x,
                     double e,
                     double x0,
                     double *ee);

/**
 * @brief Computes the inverse square root mathematical function.
 *
 * @details Returns the value of `1/sqrt(x)`.
 *
 * @invariant Parameter `x` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `e` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `x0` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  x  independent variable
 * @param [in]  e  epsilon
 * @param [in]  x0 initial estimation
 * @param [out] ee estimated error
 * @return         inverse square root of `x`
 */
extern double
isqrt_double_householder3 (double x,
                           double e,
                           double x0,
                           double *ee);

/**
 * @brief Computes the inverse square root mathematical function.
 *
 * @details Returns the value of `1/sqrt(x)`.
 *
 * @invariant Parameter `x` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `e` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `x0` must be not greater than zero.
 * The invariant is guarded by an assertion.
 *
 * @param [in]  x  independent variable
 * @param [in]  e  epsilon
 * @param [in]  x0 initial estimation
 * @param [out] ee estimated error
 * @return         inverse square root of `x`
 */
extern double
isqrt_double_halley2 (double x,
                      double e,
                      double x0,
                      double *ee);

extern double
isqrt_double_quake (const double x);


#endif /* ISQRT_H */
