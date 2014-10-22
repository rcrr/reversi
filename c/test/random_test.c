/**
 * @file
 *
 * @brief Random unit test suite.
 * @details Collects tests and helper methods for the random module.
 *
 * @par random_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "random.h"

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

static void random_seed_test (void);
static void random_get_number_in_range_test (void);
static void random_shuffle_array_uint8_2_test (void);
static void random_shuffle_array_uint8_5_test (void);

static void rng_random_seed_test (void);
static void rng_random_choice_from_finite_set_2_test (void);
static void rng_random_choice_from_finite_set_5_test (void);
static void rng_random_choice_from_finite_set_9_test (void);
static void rng_shuffle_array_uint8_2_test (void);
static void rng_shuffle_array_uint8_5_test (void);
static void rng_shuffle_array_uint8_9_test (void);
static void rng_shuffle_array_p_test (void);


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
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/random/dummy", dummy_test);

  g_test_add_func("/random/random_seed_test", random_seed_test);
  g_test_add_func("/random/random_get_number_in_range_test", random_get_number_in_range_test);
  g_test_add_func("/random/random_shuffle_array_uint8_2_test", random_shuffle_array_uint8_2_test);
  g_test_add_func("/random/random_shuffle_array_uint8_5_test", random_shuffle_array_uint8_5_test);

  g_test_add_func("/random/rng_random_seed_test", rng_random_seed_test);
  g_test_add_func("/random/rng_random_choice_from_finite_set_2_test", rng_random_choice_from_finite_set_2_test);
  g_test_add_func("/random/rng_random_choice_from_finite_set_5_test", rng_random_choice_from_finite_set_5_test);
  g_test_add_func("/random/rng_random_choice_from_finite_set_9_test", rng_random_choice_from_finite_set_9_test);
  g_test_add_func("/random/rng_shuffle_array_uint8_2_test", rng_shuffle_array_uint8_2_test);
  g_test_add_func("/random/rng_shuffle_array_uint8_5_test", rng_shuffle_array_uint8_5_test);
  g_test_add_func("/random/rng_shuffle_array_uint8_9_test", rng_shuffle_array_uint8_9_test);
  g_test_add_func("/random/rng_shuffle_array_p_test", rng_shuffle_array_p_test);

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
random_seed_test (void)
{
  int r;
  random_init_seed();
  r = random_get_number_in_range (10, 12);
  g_assert(r >= 10 && r <= 12);

  random_init_seed_with_value(12345);
  r = random_get_number_in_range (10, 12);
  g_assert(r >= 10 && r <= 12);
}

static void
random_get_number_in_range_test (void)
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
    random_init_seed_with_value(seed + j * a_prime_number);

    unsigned long int s_observations[s_size];
    for (int i = 0; i < s_size; i++) {
      s_observations[i] = 0;
    }

    for (int i = 0; i < sample_size; i++) {
      int r = random_get_number_in_range(range_lo, range_hi);
      g_assert_cmpint(r, >=, range_lo);
      g_assert_cmpint(r, <=, range_hi);
      s_observations[r - range_lo]++;
    }

    double chi_square = hlp_chi_square(s_observations, s_probabilities, s_size, sample_size);
    g_assert_cmpfloat(fabs(chi_square - expected_chi_square[j]), <=, epsilon);
  }
}

static void
random_shuffle_array_uint8_2_test (void)
{
  const int sample_size = 1000;
  const double epsilon = 0.000001;

  const unsigned int seed = 775533;
  random_init_seed_with_value(seed);

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
    random_shuffle_array_uint8(s, s_size);
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
random_shuffle_array_uint8_5_test (void)
{
  const int sample_size = 1000;
  const double epsilon = 0.000001;

  const unsigned int seed = 775533;
  random_init_seed_with_value(seed);

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
    random_shuffle_array_uint8(s, s_size);
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
rng_random_seed_test (void)
{
  const int sample_size = 10;
  unsigned long int seed[sample_size];
  for (int i = 0; i < sample_size; i++) {
    seed[i] = rng_random_seed();
    for (int j = 0; j < i; j++) {
      g_assert(seed[j] != seed[i]);
    }
  }
}

static void
rng_random_choice_from_finite_set_2_test (void)
{
  /*
   * The first ten chi_square values in the sequence are checked one by one.
   */
  double expected_chi_square[] = {0.4096, 0.8836, 0.1764, 0.5476, 0.9216, 2.1904, 0.0576, 3.7636, 0.2116, 0.1936};

  /*
   * The distribution depends on the RNG, the seed, the increment of the seed, the sample size, and
   * it depends on the size of the set on which we are sampling.
   * Anyhow the distribution appears really credible!
   */
  unsigned long int chi_square_category_expected_observations[] =  {11, 58, 180, 264, 254, 192, 32, 9};

  hlp_rng_random_choice_from_finite_set_test(FALSE,    /* just_log */
                                             2,        /* s_size */
                                             1000,     /* number_of_tests */
                                             123,      /* seed */
                                             17,       /* a_prime_number */
                                             10000,    /* sample_size */
                                             10,       /* number_of_chi_square_comparisons */
                                             0.000001, /* epsilon */
                                             expected_chi_square,
                                             chi_square_category_expected_observations);
}

static void
rng_random_choice_from_finite_set_5_test (void)
{
  double expected_chi_square[] = {0.973571, 3.839286, 4.087857, 3.529286, 4.645000, 3.097857, 4.837143, 8.080714, 0.844286, 1.419286};

  unsigned long int chi_square_category_expected_observations[] = {12, 38, 207, 259, 251, 188, 34, 11};

  hlp_rng_random_choice_from_finite_set_test(FALSE,    /* just_log */
                                             5,        /* s_size */
                                             1000,     /* number_of_tests */
                                             991,      /* seed */
                                             7,        /* a_prime_number */
                                             14000,    /* sample_size */
                                             10,       /* number_of_chi_square_comparisons */
                                             0.000001, /* epsilon */
                                             expected_chi_square,
                                             chi_square_category_expected_observations);
}

static void
rng_random_choice_from_finite_set_9_test (void)
{
  double expected_chi_square[] = {5.264, 7.262, 4.328, 6.398, 8.882, 9.584, 13.616, 3.860, 6.380, 8.036};

  unsigned long int chi_square_category_expected_observations[] = {13, 37, 185, 249, 275, 187, 45, 9};

  hlp_rng_random_choice_from_finite_set_test(FALSE,    /* just_log */
                                             9,        /* s_size */
                                             1000,     /* number_of_tests */
                                             23,       /* seed */
                                             11,       /* a_prime_number */
                                             1000,     /* sample_size */
                                             10,       /* number_of_chi_square_comparisons */
                                             0.000001, /* epsilon */
                                             expected_chi_square,
                                             chi_square_category_expected_observations);
}

static void
rng_shuffle_array_uint8_2_test (void)
{
  /* Values has to be compared with the chi-square table selecting line v=1 (one degree of freedom). */
  const double expected_chi_square[]            = {0.09604, 0.09604};
  const double expected_chi_square_transposed[] = {0.09604, 0.09604};

  hlp_rng_shuffle_array_uint8_test(FALSE,     /* just_log */
                                   2,         /* s_size */
                                   100000,    /* sample_size */
                                   0.000001,  /* epsilon */
                                   852901,    /* seed */
                                   expected_chi_square,
                                   expected_chi_square_transposed);
}

static void
rng_shuffle_array_uint8_5_test (void)
{
  /* Values has to be compared with the chi-square table selecting line v=4 (four degree of freedom). */
  const double expected_chi_square[]            = {4.33, 6.83, 2.47, 6.37, 3.34};
  const double expected_chi_square_transposed[] = {5.12, 5.00, 2.85, 7.47, 2.90};

  hlp_rng_shuffle_array_uint8_test(FALSE,     /* just_log */
                                   5,         /* s_size */
                                   1000,      /* sample_size */
                                   0.000001,  /* epsilon */
                                   775533,    /* seed */
                                   expected_chi_square,
                                   expected_chi_square_transposed);
}

static void
rng_shuffle_array_uint8_9_test (void)
{
  /* Values has to be compared with the chi-square table selecting line v=8 (eight degree of freedom). */
  const double expected_chi_square[]           = {12.08330,  4.08248, 5.54660, 12.59774, 4.45094, 10.92986, 10.45340, 18.99854, 13.25780};
  const double expected_chi_square_transposed[] = {9.45512, 10.43774, 7.61876,  9.28664, 9.06164, 10.43342, 15.19406, 10.19114, 10.72214};

  hlp_rng_shuffle_array_uint8_test(FALSE,     /* just_log */
                                   9,         /* s_size */
                                   100000,    /* sample_size */
                                   0.000001,  /* epsilon */
                                   10481,     /* seed */
                                   expected_chi_square,
                                   expected_chi_square_transposed);
}


static void
rng_shuffle_array_p_test (void)
{
  const int a_length = 10;
  void *a[a_length];
  for (int i = 0; i < a_length; i++) {
    a[i] = &a[i];
  }
  RandomNumberGenerator *rng = rng_new(37);
  rng_shuffle_array_p(rng, a, a_length);

  /*
  printf("\n\nindex;address;value\n");
  for (int i = 0; i < a_length; i++) {
    printf("%2d;%p;%p\n", i, (void *) &a[i], a[i]);
  }
  0;0x7fff9b050f20;0x7fff9b050f40
  1;0x7fff9b050f28;0x7fff9b050f48
  2;0x7fff9b050f30;0x7fff9b050f20
  3;0x7fff9b050f38;0x7fff9b050f30
  4;0x7fff9b050f40;0x7fff9b050f50
  5;0x7fff9b050f48;0x7fff9b050f60
  6;0x7fff9b050f50;0x7fff9b050f58
  7;0x7fff9b050f58;0x7fff9b050f38
  8;0x7fff9b050f60;0x7fff9b050f28
  9;0x7fff9b050f68;0x7fff9b050f68
  */
  g_assert((void **) a[0] - (void **) a == 4);
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
    RandomNumberGenerator *rng = rng_new(seed + a_prime_number * j);

    unsigned long int s_observations[s_size];
    for (int i = 0; i < s_size; i++) {
      s_observations[i] = 0;
    }

    for (int i = 0; i < sample_size; i++) {
      unsigned long int rn = rng_random_choice_from_finite_set(rng, s_size);
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

    rng_free(rng);
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

  RandomNumberGenerator *rng = rng_new(seed);

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
    rng_shuffle_array_uint8(rng, s, s_size);
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

  rng_free(rng);
  free(s_observations);
  free(s_observations_transposed);
  free(s_probabilities);
}
