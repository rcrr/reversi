/**
 * @file
 *
 * @brief Distribution and special function definitions.
 * @details This module defines the Beta distribution and the
 * Gamma function.
 *
 * @par distributions.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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

#ifndef DSTRB_H
#define DSTRB_H


/**
 * @brief The natural logarithm of the Gamma function.
 *
 * @details Returns the value of `ln(Gamma(x))`.
 *          Implements the Lanczos approximation of the Gamma function.
 *
 *          http://www.numericana.com/answer/info/godfrey.htm
 *          Lanczos Implementation of the Gamma Function
 *          Algorithm published by Paul Godfrey on 2001
 *
 *          Relative accuracy is better than 10^-13.
 *          This function has two more digits of accuracy compared
 *          with the Numerical Recipes one.
 *
 * @param [in] x independent variable
 * @return       logarithm of gamma function of `x`
 */
extern double
dstrb_log_gamma_function (double x);

/**
 * @brief The Beta function.
 *
 * @details Returns the value of `Beta(x, y)`.
 *
 * @param [in] x independent variable
 * @param [in] y independent variable
 * @return       the value of `Beta(x, y)`
 */
extern double
dstrb_beta_function (double x,
                     double y);

/**
 * @brief The probability density function of the Beta distribution.
 *
 * @details Returns the value of the PDF of the Beta
 *          distribution computed at the x value.
 *
 * @param [in] a the alpha shape parameter
 * @param [in] b the beta shape parameter
 * @param [in] x independent variable
 * @return       the value of the PDF computed at x
 */
extern double
dstrb_beta_pdf (const double a,
                const double b,
                const double x);

/**
 * @brief The cumulative distribution function of the Beta distribution.
 *
 * @details Returns the value of the CDF of the Beta
 *          distribution computed at the x value.
 *
 * @param [in] a the alpha shape parameter
 * @param [in] b the beta shape parameter
 * @param [in] x independent variable
 * @return       the value of the CDF computed at x
 */
extern double
dstrb_beta_cdf (double a,
                double b,
                double x);


#endif /* DSTRB_H */
