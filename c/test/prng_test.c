/**
 * @file
 *
 * @brief Pseudo random number generator unit test suite.
 * @details Collects tests and helper methods for the pseudo random number generator module.
 *
 * @par prng_test.c
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
#include <stdint.h>

#include <glib.h>

#include "prng.h"

#define CHI_SQUARE_CATEGORIES_COUNT 8
#define CHI_SQUARE_CATEGORIES_THRESHOLD_COUNT 7
#define CHI_SQUARE_DEGREE_OF_FREEDOM_RANGE_LO 0
#define CHI_SQUARE_DEGREE_OF_FREEDOM_RANGE_HI 9

/*
 * See: Donald E. Knuth, The Art of Computer Programming, Volume 2, Seminumarical Algorithms.
 * Paragraph 3.3.1 - General Test Procedures for Studying Random Data, Table 1.
 */
static const double chi_square_distribution_table[][CHI_SQUARE_CATEGORIES_THRESHOLD_COUNT]
/*     p=1%,    p=5%,  p=25%,  p=50%,  p=75%,  p=95%,  p=99% */
= {{0.00016, 0.00393, 0.1015, 0.4549,  1.323,  3.841,  6.635}, /* v=1 */
   {0.02010, 0.1026,  0.5754, 1.386,   2.773,  5.991,  9.210}, /* v=2 */
   {0.1148,  0.3518,  1.213,  2.366,   4.108,  7.815, 11.34},  /* v=3 */
   {0.2971,  0.7107,  1.923,  3.357,   5.385,  9.488, 13.28},  /* v=4 */
   {0.5543,  1.1455,  2.675,  4.351,   6.626, 11.07,  15.09},  /* v=5 */
   {0.8721,  1.635,   3.455,  5.348,   7.841, 12.59,  16.81},  /* v=6 */
   {1.239,   2.167,   4.255,  6.346,   9.037, 14.07,  18.48},  /* v=7 */
   {1.646,   2.733,   5.071,  7.344,  10.22,  15.51,  20.09},  /* v=8 */
   {2.088,   3.325,   5.899,  8.343,  11.39,  16.92,  21.67}}; /* v=9 */



/* Test function prototypes. */

static void dummy_test (void);

static void prng_stdlib_seed_test (void); //ok
static void prng_stdlib_get_number_in_range_test (void);
static void prng_stdlib_shuffle_array_uint8_2_test (void);
static void prng_stdlib_shuffle_array_uint8_5_test (void);

static void prng_uint64_from_clock_random_seed_test (void);
static void prng_mt19937_basic_test (void);
static void prng_mt19937_random_choice_from_finite_set_test (void);



/* Helper function prototypes. */

static double
hlp_chi_square (const unsigned long int *category_observations,
                const double *category_probabilities,
                const unsigned long int categories_count,
                const unsigned long int sample_size);

static int
hlp_chi_square_assign_to_category (const double chi_square,
                                   const int dof);

static void
hlp_rng_random_choice_from_finite_set_test (const gboolean just_log,
                                            const unsigned int s_size,
                                            const int number_of_tests,
                                            const unsigned int seed,
                                            const unsigned long int a_prime_number,
                                            const int sample_size,
                                            const int number_of_chi_square_comparisons,
                                            const double epsilon,
                                            const double expected_chi_square[],
                                            const unsigned long int chi_square_category_expected_observations[]);

static void
hlp_rng_shuffle_array_uint8_test (const gboolean just_log,
                                  const int s_size,
                                  const int sample_size,
                                  const double epsilon,
                                  const unsigned long int seed,
                                  const double expected_chi_square[],
                                  const double expected_chi_square_transposed[]);



/* Main function. */

int
main (int   argc,
      char *argv[])
{
  g_test_init(&argc, &argv, NULL);

  g_test_add_func("/prng/dummy", dummy_test);

  g_test_add_func("/random/prng_stdlib_seed_test", prng_stdlib_seed_test);
  g_test_add_func("/random/prng_stdlib_get_number_in_range_test", prng_stdlib_get_number_in_range_test);
  g_test_add_func("/random/prng_stdlib_shuffle_array_uint8_2_test", prng_stdlib_shuffle_array_uint8_2_test);
  g_test_add_func("/random/prng_stdlib_shuffle_array_uint8_5_test", prng_stdlib_shuffle_array_uint8_5_test);

  g_test_add_func("/prng/prng_uint64_from_clock_random_seed_test", prng_uint64_from_clock_random_seed_test);
  g_test_add_func("/prng/prng_mt19937_basic_test", prng_mt19937_basic_test);
  g_test_add_func("/prng/prng_mt19937_random_choice_from_finite_set", prng_mt19937_random_choice_from_finite_set_test);

  return g_test_run();
}



/*
 * Test functions.
 */

static void
dummy_test (void)
{
  g_assert(TRUE);
}



static void
prng_stdlib_seed_test (void)
{
  int r;
  prng_stdlib_init_seed();
  r = prng_stdlib_get_number_in_range(10, 12);
  g_assert(r >= 10 && r <= 12);

  prng_stdlib_init_seed_with_value(12345);
  r = prng_stdlib_get_number_in_range(10, 12);
  g_assert(r >= 10 && r <= 12);
}

static void
prng_stdlib_get_number_in_range_test (void)
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
      g_assert_cmpint(r, >=, range_lo);
      g_assert_cmpint(r, <=, range_hi);
      s_observations[r - range_lo]++;
    }

    double chi_square = hlp_chi_square(s_observations, s_probabilities, s_size, sample_size);
    g_assert_cmpfloat(fabs(chi_square - expected_chi_square[j]), <=, epsilon);
  }
}

static void
prng_stdlib_shuffle_array_uint8_2_test (void)
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
      g_assert(s[j] >= 0 && s[j] <= s_size - 1);
      sum += s[j];
      s_observations[j][s[j]]++;
    }
    g_assert(sum == s_sum);
  }

  for (int i = 0; i < s_size; i++) {
    double chi_square = hlp_chi_square(&s_observations[i][0], &s_probabilities[i][0], s_size, sample_size);
    g_assert_cmpfloat(fabs(chi_square - expected_chi_square), <=, epsilon);
  }

}

static void
prng_stdlib_shuffle_array_uint8_5_test (void)
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
      g_assert(s[j] >= 0 && s[j] <= s_size - 1);
      sum += s[j];
      s_observations[j][s[j]]++;
    }
    g_assert(sum == s_sum);
  }

  for (int i = 0; i < s_size; i++) {
    for (int j = 0; j < s_size; j++) {
      s_observations_transposed[i][j] = s_observations[j][i];
    }
  }

  for (int i = 0; i < s_size; i++) {
    double chi_square = hlp_chi_square(&s_observations[i][0], &s_probabilities[i][0], s_size, sample_size);
    g_assert_cmpfloat(fabs(chi_square - expected_chi_square[i]), <=, epsilon);
    double chi_square_t = hlp_chi_square(&s_observations_transposed[i][0], &s_probabilities[i][0], s_size, sample_size);
    g_assert_cmpfloat(fabs(chi_square_t - expected_chi_square_transposed[i]), <=, epsilon);
  }

}



static void
prng_uint64_from_clock_random_seed_test (void)
{
  unsigned long int u = 0;
  for (int i = 0; i < 10; i++) {
    u = prng_uint64_from_clock_random_seed();
    if (u) break;
  }
  /* It is really unlikely that u is taken ten times always zero !!! */
  g_assert(u);
}

static void
prng_mt19937_basic_test (void)
{
  const uint64_t init_key[] = { 0x12345ULL, 0x23456ULL, 0x34567ULL, 0x45678ULL };

  const size_t prand_array_length = 1000;

  const uint64_t expected_uint64_head[] = { 0x64D79B552A559D7F,
                                            0x44A572665A6EE240,
                                            0xEB2BF6DC3D72135C,
                                            0xE3836981F9F82EA0,
                                            0x43A38212350EE392,
                                            0xCE77502BFFCACF8B,
                                            0x5D8A82D90126F0E7,
                                            0xC0510C6F402C1E3C,
                                            0x48D895BF8B69F77B,
                                            0x8D9FBB371F1DE07F };

  const uint64_t expected_uint64_tail[] = { 0x77C8DA91B5313675,
                                            0x4CDB66AD515E0717,
                                            0x2EC4712B0BFDFCD6,
                                            0x6C6F5767FFF27330,
                                            0x071083B972D80C0C,
                                            0x8D8325E82C4FDCDC,
                                            0xB47A658DAD8E13A4,
                                            0x88710BF005FDA027,
                                            0x69BD3EDAF7111200,
                                            0x0DCCDD0C65C810FF };

  const double epsilon = 0.00000001;

  const double expected_double_head[] = { 0.35252031,
                                          0.51052342,
                                          0.79771733,
                                          0.39300273,
                                          0.27216673,
                                          0.72151068,
                                          0.43144703,
                                          0.38522290,
                                          0.20270676,
                                          0.58227313 };

  const double expected_double_tail[] = { 0.47863741,
                                          0.68796498,
                                          0.31526949,
                                          0.41180883,
                                          0.23022147,
                                          0.82342139,
                                          0.83003381,
                                          0.53571829,
                                          0.41081533,
                                          0.48600142 };

  const size_t key_length = sizeof(init_key) / sizeof(init_key[0]);
  const size_t e0_length = sizeof(expected_uint64_head) / sizeof(expected_uint64_head[0]);
  const size_t e1_length = sizeof(expected_uint64_tail) / sizeof(expected_uint64_tail[0]);
  const size_t e2_length = sizeof(expected_double_head) / sizeof(expected_double_head[0]);
  const size_t e3_length = sizeof(expected_double_tail) / sizeof(expected_double_tail[0]);

  prng_mt19937_t *prng = prng_mt19937_new();

  prng_mt19937_init_by_array(prng, init_key, key_length);

  uint64_t *prand_uint64_array = (uint64_t *) malloc(prand_array_length * sizeof(uint64_t));
  for (size_t i = 0; i < prand_array_length; i++) {
    prand_uint64_array[i] = prng_mt19937_get_uint64(prng);
  }

  for (size_t i = 0; i < e0_length; i++) {
    g_assert(prand_uint64_array[i] == expected_uint64_head[i]);
  }

  for (size_t i = 0; i < e1_length; i++) {
    g_assert(prand_uint64_array[(prand_array_length - e1_length) + i] == expected_uint64_tail[i]);
  }

  double *prand_double_array = (double *) malloc(prand_array_length * sizeof(double));
  for (size_t i = 0; i < prand_array_length; i++) {
    prand_double_array[i] = prng_mt19937_get_double_in_c0_o1(prng);
  }

  for (size_t i = 0; i < e2_length; i++) {
    g_assert(fabs(prand_double_array[i] - expected_double_head[i]) < epsilon);
  }

  for (size_t i = 0; i < e3_length; i++) {
    g_assert(fabs(prand_double_array[(prand_array_length - e3_length) + i] - expected_double_tail[i]) < epsilon);
  }

  prng_mt19937_free(prng);
  free(prand_uint64_array);
  free(prand_double_array);
}

static void
prng_mt19937_random_choice_from_finite_set_test (void)
{
  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, 17056359524ULL);

  const unsigned int k = 7;
  const unsigned int selections = 1000;
  uint64_t results[] = { 0, 0, 0, 0, 0, 0, 0 };
  uint64_t expected[] = { 139, 155, 126, 145, 137, 160, 138 };

  for (int i = 0; i < selections; i++) {
    size_t choice = prng_mt19937_random_choice_from_finite_set(prng, k);
    results[choice]++;
  }
  uint64_t cumulated = 0;
  for (int i = 0; i < k; i++) {
    g_assert(results[i] == expected[i]);
    cumulated += results[i];
  }
  g_assert(cumulated == selections);

  prng_mt19937_free(prng);
}


/*
 * Help functions.
 */

/**
 * @brief Returns the chi_square value for the `category_observations` array.
 *
 * @details The value is computed according to the explanation given in TAOCP.
 * See: Donald E. Knuth, The Art of Compuer Programming, Volume 2, Seminumarical Algorithms, 3rd ed.
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
  g_assert_cmpint(sum_of_category_observations, ==, sample_size);
  double chi_square = 0.;
  for (int i = 0; i < categories_count; i++) {
    double x = *(category_probabilities + i) * sample_size;
    double z = *(category_observations + i) - x;
    chi_square += (z * z) / x;
  }
  return chi_square;
}

/**
 * @brief Assigns the given chi_square value to a category.
 *
 * @details The value is computed according to the explanation given in TAOCP.
 * See: Donald E. Knuth, The Art of Compuer Programming, Volume 2, Seminumarical Algorithms, 3rd ed.
 * Paragraph 3.3.1 - General Test Procedures for Studying Random Data, pp 42-47.
 *
 * The `dof` parameter must be positive and has to be in the range minus one of the
 * row count of the `chi_square_distribution_table` 2d array.
 *
 * @param chi_square the chi square value to bi classified
 * @param dof        degrees of freedom of the model
 * @return           the index of the category to assign to
 */
static int
hlp_chi_square_assign_to_category (const double chi_square,
                                   const int dof)
{
  g_assert_cmpint(dof, >=, CHI_SQUARE_DEGREE_OF_FREEDOM_RANGE_LO);
  g_assert_cmpint(dof, <=, CHI_SQUARE_DEGREE_OF_FREEDOM_RANGE_HI);
  for (int k = 0; k < CHI_SQUARE_CATEGORIES_COUNT - 1; k++) {
    if (chi_square < chi_square_distribution_table[dof - 1][k]) return k;
  }
  return CHI_SQUARE_CATEGORIES_COUNT - 1;
}

/**
 * @brief Runs statistical test on selecting from a finite set at random.
 *
 * @details The test is dedicated to verify the statistical properties of the random
 * generator function when used to simulate the random selection of a value among
 * a given set of equi-probable ones.
 *
 * The helper function  uses a new `RandomNumberGenerator` for each test, running a `number_of_test` times.
 * Each test has a `sample_size` number of selections, taken from the RNG sequence.
 * The RNG is initialized using the `seed` parameter plus an integer computed using the iteration
 * counter and the `a_prime_number` parameter. It means that the random generator function is
 * called a number of times equal to the product of `sample_size` an `number_of_test` parameters.
 *
 * For each test the chi_square value of the sample is computed.
 * Tests, starting from the first (position `0`), and ending with the position
 * identified by `number_of_chi_square_comparisons - 1` are compared with the expected results
 * held into the `expected_chi_square` array.
 * If the absolute difference is larger than `epsilon` the test fails.
 *
 * Each computed chi_square value is then classified into eight categories, being [0..1)%,
 * [1..5)%, [5..25)%, [25..50)%, [50..75)%, [75..95)%, [95..99)%, and [99..100], representing
 * the probability of extracting the associated set from the random equi-distributed selection.
 * The resulting observed class frequencies are then compared wit the expected, collected in the
 * `chi_square_category_expected_observations` parameter.
 * If the expected values are different from the computed the test fails.
 *
 * @param s_size                                    the size of the set from which we sample
 * @param number_of_tests                           the number of test to run
 * @param seed                                      used to initialize the random generator
 * @param a_prime_number                            used by the formula that updated the seed each test
 * @param sample_size                               the number of samples for each test
 * @param number_of_chi_square_comparisons          the number of tests for which we verify that the computed
 *                                                  chi_square is equal to the expected one
 * @param epsilon                                   the delta used to compare expected and computed chi_square
 * @param expected_chi_square                       an array holding the expected chi_square values
 * @param chi_square_category_expected_observations an array holding the expected occurrences
 *                                                  of competed chi_square in the probability ranges
 */
static void
hlp_rng_random_choice_from_finite_set_test (const gboolean just_log,
                                            const unsigned int s_size,
                                            const int number_of_tests,
                                            const unsigned int seed,
                                            const unsigned long int a_prime_number,
                                            const int sample_size,
                                            const int number_of_chi_square_comparisons,
                                            const double epsilon,
                                            const double expected_chi_square[],
                                            const unsigned long int chi_square_category_expected_observations[])
{
  g_assert(s_size > 1);
  g_assert(number_of_tests > 0);
  g_assert(sample_size > 1);
  g_assert(number_of_chi_square_comparisons > 0);
  g_assert(epsilon > 0.);

  double s_probabilities[s_size];
  for (int i = 0; i < s_size; i++) {
    s_probabilities[i] = 1.0 / s_size;
  }

  unsigned long int chi_square_category_observations[CHI_SQUARE_CATEGORIES_COUNT];
  for (int k = 0; k < CHI_SQUARE_CATEGORIES_COUNT; k++) {
    chi_square_category_observations[k] = 0;
  }

  for (int j = 0; j < number_of_tests; j++) {
    //RandomNumberGenerator *rng = rng_new(seed + a_prime_number * j);
    prng_mt19937_t *prng = prng_mt19937_new();
    prng_mt19937_init_by_seed(prng, seed + a_prime_number * j);

    unsigned long int s_observations[s_size];
    for (int i = 0; i < s_size; i++) {
      s_observations[i] = 0;
    }

    for (int i = 0; i < sample_size; i++) {
      //unsigned long int rn = rng_random_choice_from_finite_set(rng, s_size);
      uint64_t rn = prng_mt19937_random_choice_from_finite_set(prng, s_size);
      g_assert_cmpuint(rn, <, s_size);
      s_observations[rn]++;
    }

    const double chi_square = hlp_chi_square(s_observations, s_probabilities, s_size, sample_size);
    if (j < number_of_chi_square_comparisons) {
      if (just_log) {
        printf("chi_square=%f\n", chi_square);
      } else {
        g_assert_cmpfloat(fabs(expected_chi_square[j] - chi_square), <=, epsilon);
      }
    }
    chi_square_category_observations[hlp_chi_square_assign_to_category(chi_square, s_size - 1)]++;

    //rng_free(rng);
    prng_mt19937_free(prng);
  }

  for (int k = 0; k < CHI_SQUARE_CATEGORIES_COUNT; k++) {
    if (just_log) {
      printf("chi_square_category_observations[%d]=%lu\n", k, chi_square_category_observations[k]);
    } else {
      g_assert(chi_square_category_expected_observations[k] == chi_square_category_observations[k]);
    }
  }
}

/**
 * @brief Helper function used to generalize tests on the #rng_shuffle_array_uint8 function.
 *
 * @details Runs `sample_size` times the shuffle function on a set of size `s_size`.
 * Observations are accumulated into a square matrix of size `s_size`.
 * Two vector of size `s_size` are filled with the chi_square values computed on rows and
 * columns of the matrix.
 * Chi_square values are compared with the two given `expected_chi_square` and
 * `expected_chi_square_transposed` array parameters.
 * If chi_square computed and expected values differs for less then `epsilon` the test is passed,
 * otherwise it fails.
 * Parameter `seed` initializes the random number generator according to #rng_vew documentation.
 *
 * @param just_log                           disable chi_square assertions when true
 * @param s_size                             the size of the set to shuffle
 * @param sample_size                        the number of samples for each test
 * @param epsilon                            the delta used to compare expected and computed chi_square
 * @param seed                               used to initialize the random generator
 * @param expected_chi_square                array of expected chi_square for rows of the observations matrix
 * @param expected_chi_square_transposed     array of expected chi_square for columns of the observations matrix
 */
static void
hlp_rng_shuffle_array_uint8_test (const gboolean just_log,
                                  const int s_size,
                                  const int sample_size,
                                  const double epsilon,
                                  const unsigned long int seed,
                                  const double expected_chi_square[],
                                  const double expected_chi_square_transposed[])
{
  static const size_t size_of_ulong = sizeof(unsigned long int);
  static const size_t size_of_double = sizeof(double);

  uint8_t s[s_size];
  int s_sum = 0;
  for (int j = 0; j < s_size; j++) {
    s[j] = j;
    s_sum += j;
  }

  //RandomNumberGenerator *rng = rng_new(seed);
  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, seed);

  unsigned long int (*s_observations)[s_size] = malloc(size_of_ulong * s_size * s_size);
  for (int i = 0; i < s_size; i++) {
    for (int j = 0; j < s_size; j++) {
      s_observations[i][j] = 0;
    }
  }

  double (*s_probabilities)[s_size] = malloc(size_of_double * s_size * s_size);
  for (int i = 0; i < s_size; i++) {
    for (int j = 0; j < s_size; j++) {
      s_probabilities[i][j] = (double) (1. / s_size);
    }
  }

  unsigned long int (*s_observations_transposed)[s_size] = malloc(size_of_ulong * s_size * s_size);

  for (int i = 0; i < sample_size; i++) {
    int sum = 0;
    for (int j = 0; j < s_size; j++) {
      s[j] = j;
    }
    //rng_shuffle_array_uint8(rng, s, s_size);
    prng_mt19937_shuffle_array_uint8(prng, s, s_size);
    for (int j = 0; j < s_size; j++) {
      g_assert(s[j] >= 0 && s[j] <= s_size - 1);
      sum += s[j];
      s_observations[j][s[j]]++;
    }
    g_assert(sum == s_sum);
  }

  for (int i = 0; i < s_size; i++) {
    for (int j = 0; j < s_size; j++) {
      s_observations_transposed[i][j] = s_observations[j][i];
    }
  }

  for (int i = 0; i < s_size; i++) {
    double chi_square = hlp_chi_square(&s_observations[i][0], &s_probabilities[i][0], s_size, sample_size);
    double chi_square_t = hlp_chi_square(&s_observations_transposed[i][0], &s_probabilities[i][0], s_size, sample_size);
    if (just_log) {
      printf("[%d], chi_square=%f, chi_square_t=%f\n", i, chi_square, chi_square_t);
    } else {
      g_assert_cmpfloat(fabs(chi_square - expected_chi_square[i]), <=, epsilon);
      g_assert_cmpfloat(fabs(chi_square_t - expected_chi_square_transposed[i]), <=, epsilon);
    }
  }

  //rng_free(rng);
  prng_mt19937_free(prng);
  free(s_observations);
  free(s_observations_transposed);
  free(s_probabilities);
}
