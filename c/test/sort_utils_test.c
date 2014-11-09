/**
 * @file
 *
 * @brief Sort Utils unit test suite.
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

#include <glib.h>

#include "sort_utils.h"
#include "random.h"


typedef void (*sort_utils_sort_d)(double *const a, const int count);

/* Test function prototypes. */

static void dummy_test (void);

static void sort_utils_double_compare_test (void);

static void sort_utils_insertionsort_asc_d_0_test (void);
static void sort_utils_insertionsort_dsc_d_0_test (void);
static void sort_utils_insertionsort_asc_d_1_test (void);
static void sort_utils_insertionsort_asc_d_perf_test (void);

static void sort_utils_heapsort_asc_d_0_test (void);
static void sort_utils_heapsort_dsc_d_0_test (void);
static void sort_utils_heapsort_asc_d_1_test (void);
static void sort_utils_heapsort_asc_d_perf_test (void);

static void sort_utils_smoothsort_d_0_test (void);
static void sort_utils_smoothsort_d_1_test (void);
static void sort_utils_smoothsort_d_perf_test (void);

/* Helper function prototypes. */

static void
hlp_run_sort_d_test (const sort_utils_sort_d f,
                     const int array_length,
                     const int repetitions,
                     const int factor);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/sort_utils/dummy", dummy_test);

  g_test_add_func("/sort_utils/sort_utils_double_compare_test", sort_utils_double_compare_test);

  g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_0_test", sort_utils_insertionsort_asc_d_0_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_dsc_d_0_test", sort_utils_insertionsort_dsc_d_0_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_1_test", sort_utils_insertionsort_asc_d_1_test);

  g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_0_test", sort_utils_heapsort_asc_d_0_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_dsc_d_0_test", sort_utils_heapsort_dsc_d_0_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_1_test", sort_utils_heapsort_asc_d_1_test);

  g_test_add_func("/sort_utils/sort_utils_smoothsort_d_0_test", sort_utils_smoothsort_d_0_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_d_1_test", sort_utils_smoothsort_d_1_test);

  if (g_test_perf()) {
    g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_perf_test", sort_utils_insertionsort_asc_d_perf_test);
    g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_perf_test", sort_utils_heapsort_asc_d_perf_test);
    g_test_add_func("/sort_utils/sort_utils_smoothsort_d_perf_test", sort_utils_smoothsort_d_perf_test);
  }

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
/* Unit tests for insertion-sort algorithm. */
/********************************************/

static void
sort_utils_insertionsort_asc_d_0_test (void)
{
  double a[]        = { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. };
  double expected[] = { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. };

  const int a_length = sizeof(a) / sizeof(a[0]);

  sort_utils_insertionsort(a, a_length, sizeof(double), sort_utils_double_lt);

  for (int i = 0; i < a_length; i++) {
    g_assert_cmpfloat(expected[i], ==, a[i]);
  }
}

static void
sort_utils_insertionsort_dsc_d_0_test (void)
{
  double a[]        = { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. };
  double expected[] = { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. };

  const int a_length = sizeof(a) / sizeof(a[0]);

  sort_utils_insertionsort(a, a_length, sizeof(double), sort_utils_double_gt);

  for (int i = 0; i < a_length; i++) {
    g_assert_cmpfloat(expected[i], ==, a[i]);
  }
}

static void
sort_utils_insertionsort_asc_d_1_test (void)
{
  hlp_run_sort_d_test(sort_utils_insertionsort_asc_d, 1024, 1, 0);
}

static void
sort_utils_insertionsort_asc_d_perf_test (void)
{
  hlp_run_sort_d_test(sort_utils_insertionsort_asc_d, 1024, 8, 2);
}



/***************************************/
/* Unit tests for heap-sort algorithm. */
/***************************************/

static void
sort_utils_heapsort_asc_d_0_test (void)
{
  double a[]        = { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. };
  double expected[] = { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. };

  const int a_length = sizeof(a) / sizeof(a[0]);

  sort_utils_heapsort(a, a_length, sizeof(double), sort_utils_double_lt);

  for (int i = 0; i < a_length; i++) {
    g_assert_cmpfloat(expected[i], ==, a[i]);
  }
}

static void
sort_utils_heapsort_dsc_d_0_test (void)
{
  double a[]        = { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. };
  double expected[] = { 9., 8., 7., 6., 5., 4., 3., 2., 1., 0. };

  const int a_length = sizeof(a) / sizeof(a[0]);

  sort_utils_heapsort(a, a_length, sizeof(double), sort_utils_double_gt);

  for (int i = 0; i < a_length; i++) {
    g_assert_cmpfloat(expected[i], ==, a[i]);
  }
}

static void
sort_utils_heapsort_asc_d_1_test (void)
{
  hlp_run_sort_d_test(sort_utils_heapsort_asc_d, 1024, 1, 0);
}

static void
sort_utils_heapsort_asc_d_perf_test (void)
{
  hlp_run_sort_d_test(sort_utils_heapsort_asc_d, 1024, 15, 2);
}



/*****************************************/
/* Unit tests for smooth-sort algorithm. */
/*****************************************/

static void
sort_utils_smoothsort_d_0_test (void)
{
  double a[]        = { 7., 3., 9., 0., 1., 5., 2., 8., 4., 6. };
  double expected[] = { 0., 1., 2., 3., 4., 5., 6., 7., 8., 9. };

  const int a_length = sizeof(a) / sizeof(a[0]);

  sort_utils_smoothsort_d(a, a_length);

  for (int i = 0; i < a_length; i++) {
    g_assert_cmpfloat(expected[i], ==, a[i]);
  }
}

static void
sort_utils_smoothsort_d_1_test (void)
{
  hlp_run_sort_d_test(sort_utils_smoothsort_d, 1024, 1, 0);
}

static void
sort_utils_smoothsort_d_perf_test (void)
{
  hlp_run_sort_d_test(sort_utils_smoothsort_d, 1024, 15, 2);
}



/*
 * Internal functions.
 */

static void
hlp_run_sort_d_test (const sort_utils_sort_d sort_fun,
                     const int array_length,
                     const int repetitions,
                     const int factor)
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

    RandomNumberGenerator *rng = rng_new(175);
    rng_shuffle_array_double(rng, a, len);
    rng_free(rng);

    g_test_timer_start();
    sort_fun(a, len);
    ttime = g_test_timer_elapsed();
    if (g_test_perf())
      g_test_minimized_result(ttime, "Sorting %10u items: %-12.8gsec", len, ttime);

    for (int i = 0; i < len; i++) {
      g_assert(a[i] == i);
    }

    free(a);

    len = len * factor;
  }
}
