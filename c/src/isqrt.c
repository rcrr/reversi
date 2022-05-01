/**
 * @file
 *
 * @brief Inverse Square Root module implementation.
 *
 * @par isqrt.c
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
#include <inttypes.h>
#include <assert.h>

#include "isqrt.h"

/**
 * @cond
 */

/*
 * Internal constants.
 */

/**
 * @endcond
 */

double
isqrt_double_newton (const double x,
                     const double e,
                     const double x0,
                     double *ee)
{
  assert(x > 0.);
  assert(e > 0.);
  assert(x0 > 0.);

  int i;
  const int max_i = 32;
  double y, y1, dy, z, abs_dy;

  y = x0;
  for (i = 0; i < max_i; i++) {
    z = x * y * y;
    y1 = y * ((z + 1.) / (2 * z));
    dy = y1 - y;
    abs_dy = dy * ((dy > 0) - (dy < 0));
    y = y1;
    if (abs_dy < e) break;
  }

  if (ee) *ee = abs_dy;

  return y;
}

double
isqrt_double_halley (const double x,
                     const double e,
                     const double x0,
                     double *ee)
{
  assert(x > 0.);
  assert(e > 0.);
  assert(x0 > 0.);

  int i;
  const int max_i = 32;
  double y, y1, dy, z, abs_dy;

  y = x0;
  for (i = 0; i < max_i; i++) {
    z = x * y * y;
    y1 = y * ((z + 3.) / (3. * z + 1.));
    dy = y1 - y;
    abs_dy = dy * ((dy > 0) - (dy < 0));
    y = y1;
    if (abs_dy < e) break;
  }

  if (ee) *ee = abs_dy;

  return y;
}

double
isqrt_double_householder3 (const double x,
                           const double e,
                           const double x0,
                           double *ee)
{
  assert(x > 0.);
  assert(e > 0.);
  assert(x0 > 0.);

  int i;
  const int max_i = 32;
  double y, y1, dy, z, abs_dy;

  y = x0;
  for (i = 0; i < max_i; i++) {
    z = x * y * y;
    y1 = y * ((z * z + 6. * z + 1.) / (4. * z * (z + 1.)));
    dy = y1 - y;
    abs_dy = dy * ((dy > 0) - (dy < 0));
    y = y1;
    if (abs_dy < e) break;
  }

  if (ee) *ee = abs_dy;

  return y;
}

double
isqrt_double_halley2 (const double x,
                      const double e,
                      const double x0,
                      double *ee)
{
  assert(x > 0.);
  assert(e > 0.);
  assert(x0 > 0.);

  int i;
  const int max_i = 32;
  double y, y1, dy, z, z2, z3, abs_dy;

  y = x0;
  for (i = 0; i < max_i; i++) {
    z = x * y * y;
    z2 = z * z;
    z3 = z2 * z;
    y1 = y * ((z + 3.) / (3. * z + 1.)) * ((z3 + 33. * z2 + 27. * z + 3.) / (3. * z3 + 27. * z2 + 33. * z + 1.));
    dy = y1 - y;
    abs_dy = dy * ((dy > 0) - (dy < 0));
    y = y1;
    if (abs_dy < e) break;
  }

  if (ee) *ee = abs_dy;

  return y;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
double
isqrt_double_quake (const double x)
{
  uint64_t i, *ip;
  double y, z, *yp;

  /* Quake fast inverse square root algorithm. */
  y = x;
  ip = (uint64_t *) &y;
  i = *ip;
  i = (uint64_t) 0x5fe6eb50c7b537a9 - (i >> 1);
  yp = (double *) &i;
  y = *yp;

  /* First iteration of an Householder third order refinement. */
  z = x * y * y;
  y *= ((z * z + 6. * z + 1.) / (4. * z * (z + 1.)));

  /* Second iteration. */
  z = x * y * y;
  y *= ((z * z + 6. * z + 1.) / (4. * z * (z + 1.)));

  return y;
}
#pragma GCC diagnostic pop
