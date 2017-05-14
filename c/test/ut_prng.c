/**
 * @file
 *
 * @brief PRNG (Pseudo Random Number Generator) unit test suite.
 * @details Collects tests and helper methods for the prng module.
 *
 * @par ut_prng.c
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

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "unit_test.h"

#include "prng.h"



/*
 * Auxiliary functions.
 */

/**
 * @brief Returns the chi_square value for the `category_observations` array.
 *
 * @details The value is computed according to the explanation given in TAOCP.
 * See: Donald E. Knuth, The Art of Computer Programming, Volume 2, Seminumarical Algorithms, 3rd ed.
 * Paragraph 3.3.1 - General Test Procedures for Studying Random Data, pp 42-47.
 *
 * The `sample_size` parameter is redundant becouse it must be equal to the sum of the observations.
 * An assertion assure this property.
 *
 * @param category_observations  an array with the observed numbers
 * @param category_probabilities an array with the expected probabilities
 * @param categories_count       the size of the the two array
 * @param sample_size            the size of the sample
 * @return                       the chi_square value
 */
static double
hlp_chi_square (const unsigned long int *category_observations,
                const double *category_probabilities,
                const unsigned long int categories_count,
                const unsigned long int sample_size)
{
  unsigned long int sum_of_category_observations = 0;
  for (int i = 0; i < categories_count; i++) {
    sum_of_category_observations += category_observations[i];
  }
  assert(sum_of_category_observations == sample_size);
  double chi_square = 0.;
  for (int i = 0; i < categories_count; i++) {
    double x = *(category_probabilities + i) * sample_size;
    double z = *(category_observations + i) - x;
    chi_square += (z * z) / x;
  }
  return chi_square;
}



/*
 * Test functions.
 */

static void
prng_stdlib_seed_t (ut_test_t *const t)
{
  int r;
  prng_stdlib_init_seed();
  r = prng_stdlib_get_number_in_range(10, 12);
  ut_assert(t, r >= 10 && r <= 12);

  prng_stdlib_init_seed_with_value(12345);
  r = prng_stdlib_get_number_in_range(10, 12);
  ut_assert(t, r >= 10 && r <= 12);
}

static void
prng_stdlib_get_number_in_range_t (ut_test_t *const t)
{
  const int sample_size = 10000;               /* The number of "throws" per each test. */
  const int s_size = 10;                       /* The set size from wich we select. */
  const int range_lo = 0;                      /* The lower bound of the set. */
  const int range_hi = range_lo + s_size - 1;  /* The upper bound of the set (9). */
  const unsigned long int a_prime_number = 19; /* Used to change the random seed at each iteration. */
  const int number_of_tests = 10;              /* The number of iterations (or tests). */
  const unsigned int seed = 92650;             /* A seed value used to initialize the RNG. */

  /*
   * Values has to be checked with the chi_square distribution when the DOF are nine.
   * Results match quite well.
   */
  double expected_chi_square[number_of_tests];
  expected_chi_square[0] = 10.802;
  expected_chi_square[1] = 11.474;
  expected_chi_square[2] = 11.810;
  expected_chi_square[3] = 10.628;
  expected_chi_square[4] =  3.252;
  expected_chi_square[5] = 11.292;
  expected_chi_square[6] = 11.712;
  expected_chi_square[7] =  6.068;
  expected_chi_square[8] = 12.430;
  expected_chi_square[9] = 15.868;

  const double epsilon = 0.000001;

  double s_probabilities[s_size];
  for (int i = 0; i < s_size; i++) {
    s_probabilities[i] = 1. / s_size;
  }

  for (int j = 0; j < number_of_tests; j++) {
    prng_stdlib_init_seed_with_value(seed + j * a_prime_number);

    unsigned long int s_observations[s_size];
    for (int i = 0; i < s_size; i++) {
      s_observations[i] = 0;
    }

    for (int i = 0; i < sample_size; i++) {
      int r = prng_stdlib_get_number_in_range(range_lo, range_hi);
      ut_assert(t, r >= range_lo);
      ut_assert(t, r <= range_hi);
      s_observations[r - range_lo]++;
    }

    double chi_square = hlp_chi_square(s_observations, s_probabilities, s_size, sample_size);
    ut_assert(t, fabs(chi_square - expected_chi_square[j]) <= epsilon);
  }
}

static void
prng_stdlib_shuffle_array_uint8_2_t (ut_test_t *const t)
{
  const int sample_size = 1000;
  const double epsilon = 0.000001;

  const unsigned int seed = 775533;
  prng_stdlib_init_seed_with_value(seed);

  const double expected_chi_square = 1.444000;

  const int s_size = 2;
  const int s_sum = 1;
  uint8_t s[] = {0, 1};

  unsigned long s_observations[2][2] = {{0, 0}, {0, 0}};
  double s_probabilities[2][2] = {{0.5, 0.5}, {0.5, 0.5}};

  for (int i = 0; i < sample_size; i++) {
    int sum = 0;
    for (int j = 0; j < s_size; j++) {
      s[j] = j;
    }
    prng_stdlib_shuffle_array_uint8(s, s_size);
    for (int j = 0; j < s_size; j++) {
      ut_assert(t, s[j] >= 0 && s[j] <= s_size - 1);
      sum += s[j];
      s_observations[j][s[j]]++;
    }
    ut_assert(t, sum == s_sum);
  }

  for (int i = 0; i < s_size; i++) {
    double chi_square = hlp_chi_square(&s_observations[i][0], &s_probabilities[i][0], s_size, sample_size);
    ut_assert(t, fabs(chi_square - expected_chi_square) <= epsilon);
  }

}

static void
prng_stdlib_shuffle_array_uint8_5_t (ut_test_t *const t)
{
  const int sample_size = 1000;
  const double epsilon = 0.000001;

  const unsigned int seed = 775533;
  prng_stdlib_init_seed_with_value(seed);

  /*
   * Values has to be compared with the chi-square table selecting line v=4 (four degree of freedom).
   * All the value are in a quite good range.
   */
  const double expected_chi_square[]            = {6.73, 0.97, 0.73, 2.25, 6.98};
  const double expected_chi_square_transposed[] = {4.84, 4.03, 0.83, 6.17, 1.79};

  const int s_size = 5;
  const int s_sum = 10;
  uint8_t s[] = {0, 1, 2, 3, 4};

  unsigned long s_observations[5][5] = {{0, 0, 0, 0, 0},
                                        {0, 0, 0, 0, 0},
                                        {0, 0, 0, 0, 0},
                                        {0, 0, 0, 0, 0},
                                        {0, 0, 0, 0, 0}};

  unsigned long s_observations_transposed[5][5];

  double s_probabilities[5][5] = {{.2, .2, .2, .2, .2},
                                  {.2, .2, .2, .2, .2},
                                  {.2, .2, .2, .2, .2},
                                  {.2, .2, .2, .2, .2},
                                  {.2, .2, .2, .2, .2}};

  for (int i = 0; i < sample_size; i++) {
    int sum = 0;
    for (int j = 0; j < s_size; j++) {
      s[j] = j;
    }
    prng_stdlib_shuffle_array_uint8(s, s_size);
    for (int j = 0; j < s_size; j++) {
      ut_assert(t, s[j] >= 0 && s[j] <= s_size - 1);
      sum += s[j];
      s_observations[j][s[j]]++;
    }
    ut_assert(t, sum == s_sum);
  }

  for (int i = 0; i < s_size; i++) {
    for (int j = 0; j < s_size; j++) {
      s_observations_transposed[i][j] = s_observations[j][i];
    }
  }

  for (int i = 0; i < s_size; i++) {
    double chi_square = hlp_chi_square(&s_observations[i][0], &s_probabilities[i][0], s_size, sample_size);
    ut_assert(t, fabs(chi_square - expected_chi_square[i]) <= epsilon);
    double chi_square_t = hlp_chi_square(&s_observations_transposed[i][0], &s_probabilities[i][0], s_size, sample_size);
    ut_assert(t, fabs(chi_square_t - expected_chi_square_transposed[i]) <= epsilon);
  }

}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("prng");

  ut_suite_add_simple_test(s, "prng_stdlib_seed", prng_stdlib_seed_t);
  ut_suite_add_simple_test(s, "prng_stdlib_get_number_in_range", prng_stdlib_get_number_in_range_t);
  ut_suite_add_simple_test(s, "prng_stdlib_shuffle_array_uint8_2", prng_stdlib_shuffle_array_uint8_2_t);
  ut_suite_add_simple_test(s, "prng_stdlib_shuffle_array_uint8_5", prng_stdlib_shuffle_array_uint8_5_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
