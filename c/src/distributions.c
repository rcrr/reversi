/**
 * @file
 *
 * @brief Distribution and special function implementations.
 *
 * @par distributions.c
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "distributions.h"

/**
 * @cond
 */

/*
 * Internal functions.
 */

static double
dstrb_betacfd (double a,
               double b,
               double x)
{
  int m, m2;
  double aa, c, d, del, h;

  const int max_iterations = 100;
  const double eps = 3.0e-14;
  const double fp_min = 1.0e-44;

  const double qab = a + b;
  const double qap = a + 1.0;
  const double qam = a - 1.0;

  c = 1.0;
  d = 1.0 - qab * x / qap;
  if (fabs(d) < fp_min) d = fp_min;
  d = 1.0 / d;
  h = d;
  for (m = 1; m <= max_iterations; m++) {
    m2 = 2 * m;
    aa = m * (b - m) * x / ((qam + m2) * (a + m2));
    d = 1.0 + aa * d;
    if (fabs(d) < fp_min) d=fp_min;
    c = 1.0 + aa / c;
    if (fabs(c) < fp_min) c = fp_min;
    d = 1.0 / d;
    h *= d * c;
    aa = - (a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
    d = 1.0 + aa * d;
    if (fabs(d) < fp_min) d = fp_min;
    c = 1.0 + aa / c;
    if (fabs(c) < fp_min) c = fp_min;
    d = 1.0 / d;
    del = d * c;
    h *= del;
    if (fabs(del-1.0) < eps) break;
  }
  if (m > max_iterations) {
    fprintf(stderr, "a or b too big, or max_iterations too small in betacf\n");
    abort();
  }

  return h;
}

/**
 * @endcond
 */


double
dstrb_log_gamma_function (const double x)
{
  const double sqrt_2pi = 2.50662827463100024;

  const double c[] = { 1.000000000000000174663, 5716.400188274341379136, -14815.30426768413909044,
                       14291.49277657478554025, -6348.160217641458813289, 1301.608286058321874105,
                       -108.1767053514369634679, 2.605696505611755827729, -0.7423452510201416151527e-2,
                       0.5384136432509564062961e-7, -0.4023533141268236372067e-8 };

  const double ss = x + 8.5;

  double t = x + 9.0;
  double s = c[0];
  for (int k = 10; k > 0; --k)
    s += c[k] / t--;

  return log(s * sqrt_2pi) + (x - 0.5) * log(ss) - ss;
}

double
dstrb_beta_function (const double x,
                     const double y)
{
  const double gx = dstrb_log_gamma_function(x);
  const double gy = dstrb_log_gamma_function(y);
  const double gz = dstrb_log_gamma_function(x + y);
  const double logbeta = gx + gy - gz;
  return exp(logbeta);
}

double
dstrb_beta_cdf (const double a,
                const double b,
                const double x)
{
  assert(x >= 0.0 && x <= 1.0);
  double bt;

  if (x == 0.0 || x == 1.0)
    bt = 0.0;
  else
    bt = exp(dstrb_log_gamma_function(a + b)
             - dstrb_log_gamma_function(a)
             - dstrb_log_gamma_function(b)
             + a * log(x)
             + b * log(1.0 - x)
             );
  if (x < (a + 1.0) / (a + b + 2.0))
    return bt * dstrb_betacfd(a, b, x) / a;
  else
    return 1.0 - bt * dstrb_betacfd(b, a, 1.0 - x) / b;
}

double
dstrb_beta_pdf (const double a,
                const double b,
                const double x)
{
  assert(a > 0);
  assert(b > 0);
  assert(x >= 0.0 && x <= 1.0);
  const double a1 = a - 1.0;
  const double b1 = b - 1.0;
  const double x1 = 1.0 - x;
  const double y1 = pow(x, a1) * pow(x1, b1);
  const double y0 = dstrb_beta_function(a, b);
  return y1 / y0;
}
