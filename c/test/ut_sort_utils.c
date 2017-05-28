/**
 * @file
 *
 * @brief Sort utils unit test suite.
 * @details Collects tests and helper methods for the sort utils module.
 *
 * @par ut_sort_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014, 2015, 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>

#include "unit_test.h"

#include "sort_utils.h"
#include "prng.h"



#define MIN_UINT64   0
#define MAX_UINT64   0xffffffffffffffff
#define LARGE_UINT64 0xfffffffffffffffe

#define MIN_INT64   (-9223372036854775807LL-1LL)
#define SMALL_INT64 -9223372036854775807LL
#define MAX_INT64   +9223372036854775807LL
#define LARGE_INT64 +9223372036854775806LL



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
} sorting_versus_t;

/**
 * @brief A test case collects a set of elements and the expected sorted sequence.
 */
typedef struct {
  char             *test_label;                    /**< @brief Test label. */
  sorting_versus_t  versus;                        /**< @brief 0 for descending, and 1 for ascending. */
  size_t            element_size;                  /**< @brief Number of bytes needed by one element. */
  int               elements_count;                /**< @brief The size of the array to be sorted. */
  void             *elements;                      /**< @brief The data to be sorted. */
  void             *expected_sorted_sequence;      /**< @brief The expected sequence. */
} test_case_t;

/**
 * @brief Tests with sorting function structure merges a list of test cases with a sorting implementation.
 */
typedef struct {
  void                        *tests;            /**< @brief An array of test cases. */
  sort_utils_compare_function  cmp_asc;          /**< @brief Compare function for ascending cases. */
  sort_utils_compare_function  cmp_dsc;          /**< @brief Compare function for descending cases. */
  sort_utils_sort_function     sort;             /**< @brief Sorting function. */
  size_t                       element_size;     /**< @brief Number of bytes needed by one element. */
} tests_with_sorting_function_t;

/**
 * @brief Fixtures are prepared by the #fixture_setup function by linking or
 *        deep-copying the #TestsWithSortingFunction structure.
 */
typedef struct {
  int   tests_count;                            /**< @brief Number of tests in the test case. */
  void *tests;                                  /**< @brief An array of test cases. */
} fixture_t;



/**
 * @brief Sorting test cases for simple arrays of double: base cases.
 */
const test_case_t tc_double_base[] =
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

    { "A descending sorted array of ten negative elements must be sorted in descending order.",
      DSC, sizeof(double), 10,
      (double []) {  0., -1., -2., -3., -4., -5., -6., -7., -8., -9. },
      (double []) {  0., -1., -2., -3., -4., -5., -6., -7., -8., -9. } },

    { "An ascending sorted array of ten negative elements must be sorted in ascending order.",
      ASC, sizeof(double), 10,
      (double []) { -9., -8., -7., -6., -5., -4., -3., -2., -1.,  0. },
      (double []) { -9., -8., -7., -6., -5., -4., -3., -2., -1.,  0. } },

    { "A generic aarray must be sorted in ascending order.",
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

    { NULL, ASC, sizeof(double), 1, (double []) {0}, (double []) {0} }
  };



/*
 * Tests with sorting function definitions.
 */

/* Qsort */

/**
 * @brief Qsort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_qsort =
  {
    (void *) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    qsort,
    sizeof(double),
  };



/*
 * Auxiliary functions.
 */

static void
fixture_setup (void *const fixture,
               const void *const provided_data)
{
  ;
}

static void
fixture_teardown (void *const fixture,
                  const void *const provided_data)
{
  ;
}

static void
hlp_run_tests_with_sorting_function (ut_test_t *const t,
                                     void *const fixture,
                                     const void *const provided_data)
{
  assert(fixture);
  assert(provided_data);

  ;
}



/*
 * Test functions.
 */

/*************************************/
/* Unit tests for compare functions. */
/************************************/

static void
sort_utils_double_compare_t (ut_test_t *const t)
{
  double a;
  double b;

  a = 2.;
  b = 3.;
  ut_assert(t, sort_utils_double_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_double_icmp(&a, &b) == +1);

  a = 3.;
  b = 3.;
  ut_assert(t, sort_utils_double_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_double_icmp(&a, &b) ==  0);

  a = 3.;
  b = 2.;
  ut_assert(t, sort_utils_double_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_double_icmp(&a, &b) == -1);
}

static void
sort_utils_int_compare_t (ut_test_t *const t)
{
  int a;
  int b;

  a = 2;
  b = 3;
  ut_assert(t, sort_utils_int_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_int_icmp(&a, &b) == +1);

  a = 3;
  b = 3;
  ut_assert(t, sort_utils_int_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_int_icmp(&a, &b) ==  0);

  a = 3;
  b = 2;
  ut_assert(t, sort_utils_int_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_int_icmp(&a, &b) == -1);
}

static void
sort_utils_uint64_t_compare_t (ut_test_t *const t)
{
  uint64_t a;
  uint64_t b;

  a = 2;
  b = 3;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_uint64_t_icmp(&a, &b) == +1);

  a = 3;
  b = 3;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_uint64_t_icmp(&a, &b) ==  0);

  a = 3;
  b = 2;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_uint64_t_icmp(&a, &b) == -1);

  a = MAX_UINT64;
  b = LARGE_UINT64;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) > 0);
}

static void
sort_utils_int64_t_compare_t (ut_test_t *const t)
{
  int64_t a;
  int64_t b;

  a = 2;
  b = 3;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_int64_t_icmp(&a, &b) == +1);

  a = 3;
  b = 3;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_int64_t_icmp(&a, &b) ==  0);

  a = 3;
  b = 2;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_int64_t_icmp(&a, &b) == -1);

  a = MAX_INT64;
  b = LARGE_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) > 0);

  a = 0;
  b = LARGE_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) < 0);

  a = 0;
  b = SMALL_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) > 0);

  a = MIN_INT64;
  b = SMALL_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) < 0);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("sort_utils");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_double_compare", sort_utils_double_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_int_compare", sort_utils_int_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_uint64_t_compare", sort_utils_uint64_t_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_int64_t_compare", sort_utils_int64_t_compare_t);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_qsort",
                            &twsf_double_base_qsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);


  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
