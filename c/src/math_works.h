/**
 * @file
 *
 * @brief Math Works module definitions.
 * @details This module defines ...
 *
 * @par math_works.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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

#ifndef MATH_WORKS_H
#define MATH_WORKS_H

extern int
mathw_dummy (int a);

/**
 * @brief Evaluates the logarithm of the gamma function.
 *
 * @details This routine calculates the LOG(GAMMA) function for a positive real
 *          argument X.  Computation is based on an algorithm outlined in
 *          references 1 and 2.  The program uses rational functions that
 *          theoretically approximate LOG(GAMMA) to at least 18 significant
 *          decimal digits.  The approximation for X > 12 is from reference
 *          3, while approximations for X < 12.0 are similar to those in
 *          reference 1, but are unpublished.
 *
 *          Reference:
 *
 *          William Cody, Kenneth Hillstrom,
 *          Chebyshev Approximations for the Natural Logarithm of the
 *          Gamma Function,
 *          Mathematics of Computation,
 *          Volume 21, Number 98, April 1967, pages 198-203.
 *
 *          Kenneth Hillstrom,
 *          ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
 *          May 1969.
 *
 *          John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
 *          Charles Mesztenyi, John Rice, Henry Thatcher,
 *          Christoph Witzgall,
 *          Computer Approximations,
 *          Wiley, 1968,
 *          LC: QA297.C64.
 *
 * @author Original FORTRAN77 version by William Cody, Laura Stoltz.
 *         C version by John Burkardt.
 *         Incorporation into Reversi code base by Roberto Corradini.
 *
 * @param [in] x the argument of the function
 * @return       the value of the function
 */
extern double
mathw_gamma_log (double x);

#endif //MATH_WORKS_H
