/**
 * @file
 *
 * @brief Distribution and special function module unit test suite.
 * @details Collects tests and helper methods for the distribution
 * and special function module.
 *
 * @par ut_distributions.c
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include "unit_test.h"
#include "distributions.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
dstrb_dummy_test_t (ut_test_t *const t)
{
  double epsilon, result, expected;

  expected = 1.;
  epsilon = 1.e-6;
  result = 1.0000001;
  ut_assert(t, fabs(result - expected) < epsilon);
}

static void
dstrb_log_gamma_function_t (ut_test_t *const t)
{
  struct test_data {
    double x;           /* Input value. */
    double e;           /* Expected result. */
  };

  const struct test_data td[] =
    {
     { .x =  2.0, .e =  0.0                       },
     { .x =  3.0, .e =  0.6931471805599453094172  },
     { .x =  4.0, .e =  1.791759469228055000813   },
     { .x =  5.0, .e =  3.178053830347945619647   },
     { .x =  6.0, .e =  4.787491742782045994248   },
     { .x =  7.0, .e =  6.57925121201010099506    },
     { .x =  8.0, .e =  8.525161361065414300166   },
     { .x =  9.0, .e = 10.60460290274525022842    },
     { .x = 10.0, .e = 12.80182748008146961121    },
     { .x = 11.0, .e = 15.10441257307551529523    },
     { .x = 15.0, .e = 25.19122118273867982907    },

     { .x = 0.1, .e = +2.252712651734205984155    },
     { .x = 0.2, .e = +1.524063822430784531647    },
     { .x = 0.4, .e = +0.7966778177017838080687   },
     { .x = 0.5, .e = +0.5723649429247000870728   }, // Gamma(1/2) = sqrt(pi) = 1.7724538509055160273
     { .x = 1.5, .e = -0.1207822376352452222881   }, // Gamma(3/2) = 0.5 * sqrt(pi) = 0.8862269254527580137

     { .x = 1./3., .e = +0.9854206469277670691851 }, // Gamma(1/3) = 2.6789385347077476336556929409746776441286893779573011009
     { .x = 1./4., .e = +1.288022524698077457371  }, // Gamma(1/4) = 3.6256099082219083119306851558676720029951676828800654674333
     { .x = 1./5., .e = +1.524063822430784524881  }, // Gamma(1/5) = 4.590843711998803053204758275929152
     { .x = 1./6., .e = +1.716733435078239525574  }, // Gamma(1/6) = 5.56631600178023
     { .x = 1./7., .e = +1.879169271595835836456  }, // Gamma(1/7) = 6.5480629402478244377140933494289962626211351873841351
     { .x = 1./8., .e = +2.01941835755379634532   }, // Gamma(1/8) = 7.5339415987976119046992298412151336246104195881490759409831

     { .x = 1.461632144968362, .e = -0.1214862905358496080955 }, // Min value of the Gamma function: y = 0.885603194410888700278815901
    };

  const size_t td_size = sizeof(td) / sizeof(td[0]);

  const bool verbose = ut_run_time_is_verbose(t);

  const double epsilon = 1.e-14;

  if (verbose) {
    printf("\n");
    printf("epsilon = %e\n", epsilon);
  }
  for (size_t i = 0; i < td_size; i++) {
    const double r = dstrb_log_gamma_function(td[i].x);
    const double d = fabs(r - td[i].e);
    if (verbose) printf("i = %2zu, x = %+23.18f, e = %+23.18f, r = %+23.18f, fabs(r-e) = %+23.18f\n", i, td[i].x, td[i].e, r, d);
    ut_assert(t, d < epsilon);
  }
}

static void
dstrb_beta_function_t (ut_test_t *const t)
{
  struct test_data {
    double x;           /* Input value x. */
    double y;           /* Input value y. */
    double e;           /* Expected result. */
  };

  const struct test_data td[] =
    {
     { .x =   1.0, .y =  1.0, .e =  1.0 },
     { .x =   6.0, .y =  3.0, .e =  0.005952380952380952380952 },
     { .x =   3.0, .y =  6.0, .e =  0.005952380952380952380952 },
     { .x =   5.0, .y =  5.0, .e =  0.001587301587301587301587 },
     { .x =  10.0, .y = 10.0, .e =  1.082508822446902942259E-6 },
     { .x =   0.2, .y =  0.2, .e =  9.501501389884365877947 },
     { .x =   0.1, .y = 15.0, .e =  7.278567591795812422652176 },
    };

  const size_t td_size = sizeof(td) / sizeof(td[0]);

  const double epsilon  = 1.e-12;

  const bool verbose = ut_run_time_is_verbose(t);

  if (verbose) {
    printf("\n");
    printf("epsilon = %e\n", epsilon);
  }
  for (size_t i = 0; i < td_size; i++) {
    const double r = dstrb_beta_function(td[i].x, td[i].y);
    const double d = fabs(r - td[i].e);
    if (verbose) printf("i = %2zu, x = %+23.18f, y = %+23.18f, e = %+23.18f, r = %+23.18f, fabs(r-e) = %+23.18f\n",
                        i, td[i].x, td[i].y, td[i].e, r, d);
    ut_assert(t, d < epsilon);
  }
}

static void
dstrb_beta_cdf_t (ut_test_t *const t)
{
  struct test_data {
    double a;           /* Input value a. */
    double b;           /* Input value b. */
    double x;           /* Input value x. */
    double e;           /* Expected result. */
  };

  const struct test_data td[] =
    {
     { .a = 1.0, .b = 1.0, .x = 0.0, .e = 0.0 },
     { .a = 1.0, .b = 1.0, .x = 0.5, .e = 0.5 },
     { .a = 1.0, .b = 1.0, .x = 1.0, .e = 1.0 },

     { .a = 1.0, .b = 3.0, .x = 0.4, .   e = 0.784 },
     { .a = 1.0, .b = 3.0, .x = 0.4001, .e = 0.784107982001 },

     { .a = 3.0, .b = 16.0, .x = 0.01, .   e = 0.0007291598430539 },
     { .a = 3.0, .b = 16.0, .x = 0.1,  .   e = 0.2662040052146710 },
     { .a = 3.0, .b = 16.0, .x = 0.15,  .  e = 0.5203379829150269 },
     { .a = 3.0, .b = 16.0, .x = 0.2,  .   e = 0.7286581224509276 },
     { .a = 3.0, .b = 16.0, .x = 0.3,  .   e = 0.9400477932524397 },
     { .a = 3.0, .b = 16.0, .x = 0.99, .   e = 0.9999999999999999 },

     { .a = 3.1, .b = 16.3, .x = 0.15,  .  e = 0.5096891392008445 },
 };

  const size_t td_size = sizeof(td) / sizeof(td[0]);

  const double epsilon = 1.e-14;

  const bool verbose = ut_run_time_is_verbose(t);

  if (verbose) {
    printf("\n");
    printf("epsilon = %e\n", epsilon);
  }
  for (size_t i = 0; i < td_size; i++) {
    const double r = dstrb_beta_cdf(td[i].a, td[i].b, td[i].x);
    const double d = fabs(r - td[i].e);
    if (verbose) printf("i = %2zu, a = %+23.18f, b = %+23.18f, x = %+23.18f, e = %+23.18f, r = %+23.18f, fabs(r-e) = %+23.18f\n",
                        i, td[i].a, td[i].b, td[i].x, td[i].e, r, d);
    ut_assert(t, d < epsilon);
  }
}

static void
dstrb_beta_pdf_t (ut_test_t *const t)
{
  struct test_data {
    double a;           /* Input value a. */
    double b;           /* Input value b. */
    double x;           /* Input value x. */
    double e;           /* Expected result. */
  };

  const struct test_data td[] =
    {
     { .a = 1.0, .b = 1.0, .x = 0.0, .e = 1.0 },
     { .a = 1.0, .b = 1.0, .x = 0.2, .e = 1.0 },
     { .a = 1.0, .b = 1.0, .x = 0.5, .e = 1.0 },
     { .a = 1.0, .b = 1.0, .x = 0.7, .e = 1.0 },
     { .a = 1.0, .b = 1.0, .x = 1.0, .e = 1.0 },

     { .a = 2.0, .b = 5.0, .x = 0.2, .e = 2.4576 },

     { .a = 0.5, .b = 0.5, .x = 0.001, .e = 10.0708791199470941620 },
     { .a = 0.5, .b = 0.5, .x = 0.2,   .e =  0.7957747154594766788 },
     { .a = 0.5, .b = 0.5, .x = 0.5,   .e =  0.6366197723675813430 },

     { .a = 5.0, .b = 1.0, .x = 0.2, .e = 0.008  },
     { .a = 5.0, .b = 1.0, .x = 0.4, .e = 0.128  },
     { .a = 5.0, .b = 1.0, .x = 0.5, .e = 0.3125 },
     { .a = 5.0, .b = 1.0, .x = 0.8, .e = 2.048  },

     { .a = 1.0, .b = 3.0, .x = 0.0, .e = 3.00  },
     { .a = 1.0, .b = 3.0, .x = 0.1, .e = 2.43  },
     { .a = 1.0, .b = 3.0, .x = 0.2, .e = 1.92  },
     { .a = 1.0, .b = 3.0, .x = 0.3, .e = 1.47  },

     { .a = 2.0, .b = 2.0, .x = 0.0, .e = 0.00  },
     { .a = 2.0, .b = 2.0, .x = 0.1, .e = 0.54  },
     { .a = 2.0, .b = 2.0, .x = 0.5, .e = 1.50  },
     { .a = 2.0, .b = 2.0, .x = 0.1, .e = 0.54  },
     { .a = 2.0, .b = 2.0, .x = 0.0, .e = 0.00  },

    };

  const size_t td_size = sizeof(td) / sizeof(td[0]);

  const double epsilon = 1.e-14;

  const bool verbose = ut_run_time_is_verbose(t);

  if (verbose) {
    printf("\n");
    printf("epsilon = %e\n", epsilon);
  }
  for (size_t i = 0; i < td_size; i++) {
    const double r = dstrb_beta_pdf(td[i].a, td[i].b, td[i].x);
    const double d = fabs(r - td[i].e);
    if (verbose) printf("i = %2zu, a = %+23.18f, b = %+23.18f, x = %+23.18f, e = %+23.18f, r = %+23.18f, fabs(r-e) = %+23.18f\n",
                        i, td[i].a, td[i].b, td[i].x, td[i].e, r, d);
    ut_assert(t, d < epsilon);
  }
}


/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "distributions");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dstrb_dummy_test", dstrb_dummy_test_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dstrb_log_gamma_function", dstrb_log_gamma_function_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dstrb_beta_function", dstrb_beta_function_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dstrb_beta_pdf", dstrb_beta_pdf_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dstrb_beta_cdf", dstrb_beta_cdf_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
