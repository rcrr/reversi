/**
 * @file
 *
 * @brief Sort utils unit test suite.
 *
 * @details Collects tests and helper methods for the sort utils module.
 *
 * @par sort_utils_test.c
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
#include <string.h>

#include <glib.h>

#include "sort_utils.h"
#include "random.h"


typedef void (*sort_double_fun) (double *const a, const int count);

/**
 * @enum SortingVersus
 * @brief The sorting versus.
 *
 * Ascending means that a lesser element precede a greater one, descending
 * is the opposite.
 */
typedef enum {
  DSC,   /**< Ascending. */
  ASC    /**< Descending. */
} SortingVersus;

/**
 * @brief A test case collects a set of elements and the expected sorted sequence.
 */
typedef struct {
  gchar         *test_label;                     /**< @brief Test label. */
  SortingVersus  versus;                         /**< @brief 0 for descending, and 1 for ascending. */
  size_t         element_size;                   /**< @brief Number of bytes needed by one element. */
  int            elements_count;                 /**< @brief The size of the array to be sorted. */
  void          *elements;                       /**< @brief The data to be sorted. */
  void          *expected_sorted_sequence;       /**< @brief The expected sequence. */
} TestCase;

/**
 * @brief Fixtures are prepared by the #base_fixture_setup function by linking or
 *        deep-copying the #TestCase structure.
 */
typedef struct {
  int   tests_count;                      /**< @brief Number of tests in the test case. */
  void *tests;                            /**< @brief An array of test cases. */
} Fixture;

/**
 * @brief Sorting test cases for simple arrays of double: base cases.
 */
const TestCase tc_double_base[] =
  {
    { "A simple array of ten elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. },
      (double []) { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. } },

    { "A simple array of ten elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. },
      (double []) { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. } },

    { "An ascending sorted array of ten elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. },
      (double []) { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. } },

    { "An ascending sorted array of ten elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. },
      (double []) { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. } },

    { "A descending sorted array of ten elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. },
      (double []) { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. } },

    { "A descending sorted array of ten elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. },
      (double []) { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. } },

    { "An array of ten equal elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) { 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. },
      (double []) { 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. } },

    { "An array of ten equal elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. },
      (double []) { 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. } },

    { "An ascending sorted array of ten negative elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) { -9., -8., -7., -6., -5., -4., -3., -2., -1.,  0. },
      (double []) {  0., -1., -2., -3., -4., -5., -6., -7., -8., -9. } },

    { "A dscending sorted array of ten negative elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) {  0., -1., -2., -3., -4., -5., -6., -7., -8., -9. },
      (double []) {  0., -1., -2., -3., -4., -5., -6., -7., -8., -9. } },

    { "An ascending sorted array of ten negative elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { -9., -8., -7., -6., -5., -4., -3., -2., -1.,  0. },
      (double []) { -9., -8., -7., -6., -5., -4., -3., -2., -1.,  0. } },

    { "An ascending sorted array of ten negative elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { -9.70, 8.34, 123.02, 0.00, -72.03, 9.71, -3.23, -9.70, 9.70, -0.1 },
      (double []) { -72.03, -9.70, -9.70, -3.23, -0.1, 0.00, 8.34, 9.70, 9.71, 123.02 } },

    { "An array of ten elements, nine zeros and a one, must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { 0., 0., 0., 0., 0., 0., 1., 0., 0., 0. },
      (double []) { 0., 0., 0., 0., 0., 0., 0., 0., 0., 1. } },

    { "An array of ten elements, seven zeros and three ones, must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { 1., 0., 0., 0., 0., 0., 1., 0., 0., 1. },
      (double []) { 0., 0., 0., 0., 0., 0., 0., 1., 1., 1. } },

    { "An array of two elements, must be sorted in ascending order.",
      ASC, sizeof(double), 2,
      (double []) { 1., 0. },
      (double []) { 0., 1. } },

    { "An array of two elements, must be sorted in ascending order.",
      ASC, sizeof(double), 2,
      (double []) { 0., 1. },
      (double []) { 0., 1. } },

    { "An array of two elements, must be sorted in ascending order.",
      ASC, sizeof(double), 2,
      (double []) { 0., 0. },
      (double []) { 0., 0. } },

    { "An array of one element, must be sorted in ascending order.",
      ASC, sizeof(double), 1,
      (double []) { 0. },
      (double []) { 0. } },

    { "An empty array, must be sorted in ascending order.",
      ASC, sizeof(double), 0,
      (double []) { 0. },
      (double []) { 0. } },

    { "An array of seven elements, five zeros and two ones, must be sorted in ascending order.",
      ASC, sizeof(double), 7,
      (double []) { 1., 0., 0., 0., 0., 0., 1. },
      (double []) { 0., 0., 0., 0., 0., 1., 1. } },

    {NULL, ASC, sizeof(double), 1, (double []) {0}, (double []) {0} }
  };



/*
 * Test function prototypes.
 */

static void sort_utils_double_compare_test (void);

static void
sort_utils_qsort_tc_double_base_test (Fixture *fixture,
                                      gconstpointer test_data);

static void sort_utils_qsort_asc_d_1_rand_test (void);
static void sort_utils_qsort_asc_d_n_rand_test (void);
static void sort_utils_qsort_dsc_d_1_rand_test (void);
static void sort_utils_qsort_dsc_d_n_rand_test (void);
static void sort_utils_qsort_asc_d_rand_perf_test (void);

static void
sort_utils_insertionsort_tc_double_base_test (Fixture *fixture,
                                              gconstpointer test_data);

static void sort_utils_insertionsort_asc_d_1_rand_test (void);
static void sort_utils_insertionsort_asc_d_n_rand_test (void);
static void sort_utils_insertionsort_dsc_d_1_rand_test (void);
static void sort_utils_insertionsort_dsc_d_n_rand_test (void);
static void sort_utils_insertionsort_asc_d_rand_perf_test (void);

static void
sort_utils_heapsort_tc_double_base_test (Fixture *fixture,
                                         gconstpointer test_data);

static void sort_utils_heapsort_asc_d_1_rand_test (void);
static void sort_utils_heapsort_asc_d_n_rand_test (void);
static void sort_utils_heapsort_dsc_d_1_rand_test (void);
static void sort_utils_heapsort_dsc_d_n_rand_test (void);
static void sort_utils_heapsort_asc_d_rand_perf_test (void);

static void
sort_utils_smoothsort_tc_double_base_test (Fixture *fixture,
                                           gconstpointer test_data);

static void sort_utils_smoothsort_asc_d_1_rand_test (void);
static void sort_utils_smoothsort_asc_d_n_rand_test (void);
static void sort_utils_smoothsort_dsc_d_1_rand_test (void);
static void sort_utils_smoothsort_dsc_d_n_rand_test (void);
static void sort_utils_smoothsort_asc_d_rand_perf_test (void);

static void
sort_utils_quicksort_tc_double_base_test (Fixture *fixture,
                                          gconstpointer test_data);

static void sort_utils_quicksort_asc_d_1_rand_test (void);
static void sort_utils_quicksort_asc_d_n_rand_test (void);
static void sort_utils_quicksort_dsc_d_1_rand_test (void);
static void sort_utils_quicksort_dsc_d_n_rand_test (void);
static void sort_utils_quicksort_asc_d_rand_perf_test (void);

static void
sort_utils_shellsort_tc_double_base_test (Fixture *fixture,
                                          gconstpointer test_data);

static void sort_utils_shellsort_asc_d_1_rand_test (void);
static void sort_utils_shellsort_asc_d_n_rand_test (void);
static void sort_utils_shellsort_dsc_d_1_rand_test (void);
static void sort_utils_shellsort_dsc_d_n_rand_test (void);
static void sort_utils_shellsort_asc_d_rand_perf_test (void);




/*
 * Helper function prototypes.
 */

static void
base_fixture_setup (Fixture *fixture,
                    gconstpointer test_data);

static void
base_fixture_teardown (Fixture *fixture,
                       gconstpointer test_data);

static void
hlp_run_sort_d_random_test (const sort_double_fun f,
                            const int array_length,
                            const int repetitions,
                            const int factor,
                            const int seed,
                            const SortingVersus v);

static void
sort_utils_qsort_asc_d (double *const a,
                        const int count);

static void
sort_utils_qsort_dsc_d (double *const a,
                        const int count);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/sort_utils/sort_utils_double_compare_test", sort_utils_double_compare_test);


  g_test_add("/sort_utils/sort_utils_qsort_tc_double_base_test",
             Fixture,
             (gconstpointer) tc_double_base,
             base_fixture_setup,
             sort_utils_qsort_tc_double_base_test,
             base_fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_qsort_asc_d_1_rand_test", sort_utils_qsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_qsort_dsc_d_1_rand_test", sort_utils_qsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_qsort_asc_d_n_rand_test", sort_utils_qsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_qsort_dsc_d_n_rand_test", sort_utils_qsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/sort_utils_insertionsort_tc_double_base_test",
             Fixture,
             (gconstpointer) tc_double_base,
             base_fixture_setup,
             sort_utils_insertionsort_tc_double_base_test,
             base_fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_1_rand_test", sort_utils_insertionsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_dsc_d_1_rand_test", sort_utils_insertionsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_n_rand_test", sort_utils_insertionsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_dsc_d_n_rand_test", sort_utils_insertionsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/sort_utils_heapsort_tc_double_base_test",
             Fixture,
             (gconstpointer) tc_double_base,
             base_fixture_setup,
             sort_utils_heapsort_tc_double_base_test,
             base_fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_1_rand_test", sort_utils_heapsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_dsc_d_1_rand_test", sort_utils_heapsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_n_rand_test", sort_utils_heapsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_dsc_d_n_rand_test", sort_utils_heapsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/sort_utils_smoothsort_tc_double_base_test",
             Fixture,
             (gconstpointer) tc_double_base,
             base_fixture_setup,
             sort_utils_smoothsort_tc_double_base_test,
             base_fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_smoothsort_asc_d_1_rand_test", sort_utils_smoothsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_dsc_d_1_rand_test", sort_utils_smoothsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_asc_d_n_rand_test", sort_utils_smoothsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_dsc_d_n_rand_test", sort_utils_smoothsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/sort_utils_quicksort_tc_double_base_test",
             Fixture,
             (gconstpointer) tc_double_base,
             base_fixture_setup,
             sort_utils_quicksort_tc_double_base_test,
             base_fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_quicksort_asc_d_1_rand_test", sort_utils_quicksort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_quicksort_dsc_d_1_rand_test", sort_utils_quicksort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_quicksort_asc_d_n_rand_test", sort_utils_quicksort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_quicksort_dsc_d_n_rand_test", sort_utils_quicksort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/sort_utils_shellsort_tc_double_base_test",
             Fixture,
             (gconstpointer) tc_double_base,
             base_fixture_setup,
             sort_utils_shellsort_tc_double_base_test,
             base_fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_shellsort_asc_d_1_rand_test", sort_utils_shellsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_shellsort_dsc_d_1_rand_test", sort_utils_shellsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_shellsort_asc_d_n_rand_test", sort_utils_shellsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_shellsort_dsc_d_n_rand_test", sort_utils_shellsort_dsc_d_n_rand_test);

  if (g_test_perf()) {
    g_test_add_func("/sort_utils/sort_utils_qsort_asc_d_rand_perf_test", sort_utils_qsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_rand_perf_test", sort_utils_insertionsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_rand_perf_test", sort_utils_heapsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_smoothsort_asc_d_rand_perf_test", sort_utils_smoothsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_quicksort_asc_d_rand_perf_test", sort_utils_quicksort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_shellsort_asc_d_rand_perf_test", sort_utils_shellsort_asc_d_rand_perf_test);
  }

  return g_test_run();
}



/*
 * Test functions.
 */

/*************************************/
/* Unit tests for compare functions. */
/*************************************/

static void
sort_utils_double_compare_test (void)
{
  double a;
  double b;

  a = 3.;
  b = 3.;
  g_assert_true(sort_utils_double_eq(&a, &b));

  a = 3.;
  b = 2.;
  g_assert_false(sort_utils_double_eq(&a, &b));

  a = 3.;
  b = 2.;
  g_assert_true(sort_utils_double_gt(&a, &b));

  a = 2.;
  b = 3.;
  g_assert_false(sort_utils_double_gt(&a, &b));

  a = 3.;
  b = 3.;
  g_assert_false(sort_utils_double_gt(&a, &b));

  a = 3.;
  b = 2.;
  g_assert_true(sort_utils_double_ge(&a, &b));

  a = 2.;
  b = 3.;
  g_assert_false(sort_utils_double_ge(&a, &b));

  a = 3.;
  b = 3.;
  g_assert_true(sort_utils_double_ge(&a, &b));

  a = 3.;
  b = 2.;
  g_assert_false(sort_utils_double_lt(&a, &b));

  a = 2.;
  b = 3.;
  g_assert_true(sort_utils_double_lt(&a, &b));

  a = 3.;
  b = 3.;
  g_assert_false(sort_utils_double_lt(&a, &b));

  a = 3.;
  b = 2.;
  g_assert_false(sort_utils_double_le(&a, &b));

  a = 2.;
  b = 3.;
  g_assert_true(sort_utils_double_le(&a, &b));

  a = 3.;
  b = 3.;
  g_assert_true(sort_utils_double_le(&a, &b));

  a = 2.;
  b = 3.;
  g_assert_cmpint(sort_utils_double_cmp(&a, &b), ==, -1);

  a = 3.;
  b = 3.;
  g_assert_cmpint(sort_utils_double_cmp(&a, &b), ==,  0);

  a = 3.;
  b = 2.;
  g_assert_cmpint(sort_utils_double_cmp(&a, &b), ==, +1);
}



/********************************************/
/* Unit tests for stdlib.h qsort algorithm. */
/********************************************/

static void
sort_utils_qsort_tc_double_base_test (Fixture *fixture,
                                      gconstpointer test_data)
{
  TestCase *tests = fixture->tests;
  g_assert(tests);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *t = &tests[i];
    sort_utils_compare_function f;
    switch (t->versus) {
    case ASC:
      f = sort_utils_double_cmp;
      break;
    case DSC:
      f = sort_utils_double_icmp;
      break;
    default:
      g_test_fail();
      return;
    }
    qsort(t->elements,
          t->elements_count,
          sizeof(double),
          f);
    for (int j = 0; j < t->elements_count; j++) {
      const double *computed = (double *) t->elements + j;
      const double *expected = (double *) t->expected_sorted_sequence + j;
      g_assert_cmpfloat(*expected, ==, *computed);
    }
  }
}

static void
sort_utils_qsort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_qsort_asc_d, 1024, 1, 0, 654, ASC);
}

static void
sort_utils_qsort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_qsort_dsc_d, 1024, 1, 0, 870, DSC);
}

static void
sort_utils_qsort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_qsort_asc_d, 1024, 3, 2, 103, ASC);
  hlp_run_sort_d_random_test(sort_utils_qsort_asc_d, 1023, 3, 2,  55, ASC);
  hlp_run_sort_d_random_test(sort_utils_qsort_asc_d, 1025, 3, 2, 870, ASC);
}

static void
sort_utils_qsort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_qsort_dsc_d, 1024, 3, 2, 253, DSC);
  hlp_run_sort_d_random_test(sort_utils_qsort_dsc_d, 1023, 3, 2, 763, DSC);
  hlp_run_sort_d_random_test(sort_utils_qsort_dsc_d, 1025, 3, 2, 894, DSC);
}

static void
sort_utils_qsort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_qsort_asc_d, 1024, 15, 2, 175, ASC);
}



/********************************************/
/* Unit tests for insertion-sort algorithm. */
/********************************************/

static void
sort_utils_insertionsort_tc_double_base_test (Fixture *fixture,
                                              gconstpointer test_data)
{
  TestCase *tests = fixture->tests;
  g_assert(tests);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *t = &tests[i];
    sort_utils_compare_function f;
    switch (t->versus) {
    case ASC:
      f = sort_utils_double_lt;
      break;
    case DSC:
      f = sort_utils_double_gt;
      break;
    default:
      g_test_fail();
      return;
    }
    sort_utils_insertionsort(t->elements,
                             t->elements_count,
                             sizeof(double),
                             f);
    for (int i = 0; i < t->elements_count; i++) {
      const double *computed = (double *) t->elements + i;
      const double *expected = (double *) t->expected_sorted_sequence + i;
      g_assert_cmpfloat(*expected, ==, *computed);
    }
  }
}

static void
sort_utils_insertionsort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_insertionsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_insertionsort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_insertionsort_dsc_d, 1024, 1, 0, 175, DSC);
}

static void
sort_utils_insertionsort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_insertionsort_asc_d, 1024, 3, 2, 322, ASC);
  hlp_run_sort_d_random_test(sort_utils_insertionsort_asc_d, 1023, 3, 2, 655, ASC);
  hlp_run_sort_d_random_test(sort_utils_insertionsort_asc_d, 1025, 3, 2, 983, ASC);
}

static void
sort_utils_insertionsort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_insertionsort_dsc_d, 1024, 3, 2, 114, DSC);
  hlp_run_sort_d_random_test(sort_utils_insertionsort_dsc_d, 1023, 3, 2, 563, DSC);
  hlp_run_sort_d_random_test(sort_utils_insertionsort_dsc_d, 1025, 3, 2, 940, DSC);
}

static void
sort_utils_insertionsort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_insertionsort_asc_d, 1024, 8, 2, 175, ASC);
}



/***************************************/
/* Unit tests for heap-sort algorithm. */
/***************************************/

static void
sort_utils_heapsort_tc_double_base_test (Fixture *fixture,
                                         gconstpointer test_data)
{
  TestCase *tests = fixture->tests;
  g_assert(tests);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *t = &tests[i];
    sort_utils_compare_function f;
    switch (t->versus) {
    case ASC:
      f = sort_utils_double_lt;
      break;
    case DSC:
      f = sort_utils_double_gt;
      break;
    default:
      g_test_fail();
      return;
    }
    sort_utils_heapsort(t->elements,
                        t->elements_count,
                        sizeof(double),
                        f);
    for (int i = 0; i < t->elements_count; i++) {
      const double *computed = (double *) t->elements + i;
      const double *expected = (double *) t->expected_sorted_sequence + i;
      g_assert_cmpfloat(*expected, ==, *computed);
    }
  }
}

static void
sort_utils_heapsort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_heapsort_asc_d, 1024, 1, 0, 176, ASC);
}

static void
sort_utils_heapsort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_heapsort_dsc_d, 1024, 1, 0, 174, DSC);
}

static void
sort_utils_heapsort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_heapsort_asc_d, 1024, 3, 2, 326, ASC);
  hlp_run_sort_d_random_test(sort_utils_heapsort_asc_d, 1023, 3, 2, 643, ASC);
  hlp_run_sort_d_random_test(sort_utils_heapsort_asc_d, 1025, 3, 2, 987, ASC);
}

static void
sort_utils_heapsort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_heapsort_dsc_d, 1024, 3, 2, 133, DSC);
  hlp_run_sort_d_random_test(sort_utils_heapsort_dsc_d, 1023, 3, 2, 545, DSC);
  hlp_run_sort_d_random_test(sort_utils_heapsort_dsc_d, 1025, 3, 2, 540, DSC);
}

static void
sort_utils_heapsort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_heapsort_asc_d, 1024, 15, 2, 175, ASC);
}



/*****************************************/
/* Unit tests for smooth-sort algorithm. */
/*****************************************/

static void
sort_utils_smoothsort_tc_double_base_test (Fixture *fixture,
                                           gconstpointer test_data)
{
  TestCase *tests = fixture->tests;
  g_assert(tests);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *t = &tests[i];
    sort_utils_compare_function f;
    switch (t->versus) {
    case ASC:
      f = sort_utils_double_le;
      break;
    case DSC:
      f = sort_utils_double_ge;
      break;
    default:
      g_test_fail();
      return;
    }
    sort_utils_smoothsort(t->elements,
                          t->elements_count,
                          sizeof(double),
                          f);
    for (int i = 0; i < t->elements_count; i++) {
      const double *computed = (double *) t->elements + i;
      const double *expected = (double *) t->expected_sorted_sequence + i;
      g_assert_cmpfloat(*expected, ==, *computed);
    }
  }
}

static void
sort_utils_smoothsort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_smoothsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_smoothsort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_smoothsort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_smoothsort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_smoothsort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(sort_utils_smoothsort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(sort_utils_smoothsort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_smoothsort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_smoothsort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(sort_utils_smoothsort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(sort_utils_smoothsort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_smoothsort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_smoothsort_asc_d, 1024, 15, 2, 175, ASC);
}



/****************************************/
/* Unit tests for quick-sort algorithm. */
/****************************************/

static void
sort_utils_quicksort_tc_double_base_test (Fixture *fixture,
                                          gconstpointer test_data)
{
  TestCase *tests = fixture->tests;
  g_assert(tests);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *t = &tests[i];
    sort_utils_compare_function f;
    switch (t->versus) {
    case ASC:
      f = sort_utils_double_cmp;
      break;
    case DSC:
      f = sort_utils_double_icmp;
      break;
    default:
      g_test_fail();
      return;
    }
    sort_utils_quicksort(t->elements,
                         t->elements_count,
                         sizeof(double),
                         f);
    for (int i = 0; i < t->elements_count; i++) {
      const double *computed = (double *) t->elements + i;
      const double *expected = (double *) t->expected_sorted_sequence + i;
      g_assert_cmpfloat(*expected, ==, *computed);
    }
  }
}

static void
sort_utils_quicksort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_quicksort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_quicksort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_quicksort_dsc_d, 1024, 1, 0, 121, DSC);
}

static void
sort_utils_quicksort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_quicksort_asc_d, 1024, 3, 2, 346, ASC);
  hlp_run_sort_d_random_test(sort_utils_quicksort_asc_d, 1023, 3, 2, 673, ASC);
  hlp_run_sort_d_random_test(sort_utils_quicksort_asc_d, 1025, 3, 2, 227, ASC);
}

static void
sort_utils_quicksort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_quicksort_dsc_d, 1024, 3, 2, 433, DSC);
  hlp_run_sort_d_random_test(sort_utils_quicksort_dsc_d, 1023, 3, 2, 375, DSC);
  hlp_run_sort_d_random_test(sort_utils_quicksort_dsc_d, 1025, 3, 2, 560, DSC);
}

static void
sort_utils_quicksort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_quicksort_asc_d, 1024, 15, 2, 175, ASC);
}



/***************************************/
/* Unit tests for shellsort algorithm. */
/***************************************/

static void
sort_utils_shellsort_tc_double_base_test (Fixture *fixture,
                                          gconstpointer test_data)
{
  TestCase *tests = fixture->tests;
  g_assert(tests);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *t = &tests[i];
    sort_utils_compare_function f;
    switch (t->versus) {
    case ASC:
      f = sort_utils_double_cmp;
      break;
    case DSC:
      f = sort_utils_double_icmp;
      break;
    default:
      g_test_fail();
      return;
    }
    sort_utils_shellsort(t->elements,
                         t->elements_count,
                         sizeof(double),
                         f);
    for (int j = 0; j < t->elements_count; j++) {
      const double *computed = (double *) t->elements + j;
      const double *expected = (double *) t->expected_sorted_sequence + j;
      g_assert_cmpfloat(*expected, ==, *computed);
    }
  }
}

static void
sort_utils_shellsort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_shellsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_shellsort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_shellsort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_shellsort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_shellsort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(sort_utils_shellsort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(sort_utils_shellsort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_shellsort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_shellsort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(sort_utils_shellsort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(sort_utils_shellsort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_shellsort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_shellsort_asc_d, 1024, 15, 2, 175, ASC);
}



/*
 * Internal functions.
 */

static void
base_fixture_setup (Fixture *fixture,
                    gconstpointer test_data)
{
  const size_t size_of_test_case = sizeof(TestCase);

  const TestCase *const test_defs = (TestCase *) test_data;

  fixture->tests_count = 0;
  for (int i = 0;; i++) {
    const TestCase *td = &test_defs[i];
    if (td->test_label == NULL) {
      fixture->tests_count = i;
      break;
    }
  }

  TestCase *tests = (TestCase *) malloc(fixture->tests_count * size_of_test_case);
  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *td = &test_defs[i];
    TestCase *t = &tests[i];
    t->test_label = td->test_label;
    t->versus = td->versus;
    t->element_size = td->element_size;
    t->elements_count = td->elements_count;
    const size_t size_of_elements_array = td->elements_count * td->element_size;
    t->elements = malloc(size_of_elements_array);
    memcpy(t->elements, td->elements, size_of_elements_array);
    t->expected_sorted_sequence = td->expected_sorted_sequence;
  }
  fixture->tests = tests;
}

static void
base_fixture_teardown (Fixture *fixture,
                       gconstpointer test_data)
{
  TestCase *tests = (TestCase *) fixture->tests;
  for (int i = 0; i < fixture->tests_count; i++) {
    free(tests[i].elements);
  }
  free(fixture->tests);
}


static void
hlp_run_sort_d_random_test (const sort_double_fun sort_fun,
                            const int array_length,
                            const int repetitions,
                            const int factor,
                            const int seed,
                            const SortingVersus v)
{
  g_assert(array_length > 0);
  double ttime;
  static const size_t size_of_double = sizeof(double);
  int len = array_length;

  for (int i = 0; i < repetitions; i++) {
    double *a = (double *) malloc(len * size_of_double);
    g_assert(a);

    for (int i = 0; i < len; i++) {
      a[i] = i;
    }

    RandomNumberGenerator *rng = rng_new(seed);
    rng_shuffle_array_double(rng, a, len);
    rng_free(rng);

    g_test_timer_start();
    sort_fun(a, len);
    ttime = g_test_timer_elapsed();
    if (g_test_perf())
      g_test_minimized_result(ttime, "Sorting %10u items: %-12.8gsec", len, ttime);

    for (int i = 0; i < len; i++) {
      switch (v) {
      case ASC:
        g_assert(a[i] == i);
        break;
      case DSC:
        g_assert(a[i] == len - (1 + i));
        break;
      default:
        g_test_fail();
      return;
      }
    }

    free(a);

    len = len * factor;
  }
}

static void
sort_utils_qsort_asc_d (double *const a,
                        const int count)
{
  qsort(a, count, sizeof(double), sort_utils_double_cmp);
}

static void
sort_utils_qsort_dsc_d (double *const a,
                        const int count)
{
  qsort(a, count, sizeof(double), sort_utils_double_icmp);
}
