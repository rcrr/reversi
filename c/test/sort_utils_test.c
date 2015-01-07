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
 * @copyright 2014, 2015 Roberto Corradini. All rights reserved.
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



#define MIN_UINT64   0
#define MAX_UINT64   0xffffffffffffffff
#define LARGE_UINT64 0xfffffffffffffffe

#define MIN_INT64   (-9223372036854775807LL-1LL)
#define SMALL_INT64 -9223372036854775807LL
#define MAX_INT64   +9223372036854775807LL
#define LARGE_INT64 +9223372036854775806LL



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
  gchar         *test_label;                    /**< @brief Test label. */
  SortingVersus  versus;                        /**< @brief 0 for descending, and 1 for ascending. */
  size_t         element_size;                  /**< @brief Number of bytes needed by one element. */
  int            elements_count;                /**< @brief The size of the array to be sorted. */
  void          *elements;                      /**< @brief The data to be sorted. */
  void          *expected_sorted_sequence;      /**< @brief The expected sequence. */
} TestCase;

/**
 * @brief Tests with sorting function structure merges a list of test cases with a sorting implementation.
 */
typedef struct {
  gconstpointer               tests;            /**< @brief An array of test cases. */
  sort_utils_compare_function cmp_asc;          /**< @brief Compare function for ascending cases. */
  sort_utils_compare_function cmp_dsc;          /**< @brief Compare function for descending cases. */
  sort_utils_sort_function    sort;             /**< @brief Sorting function. */
  size_t                      element_size;     /**< @brief Number of bytes needed by one element. */
  sort_utils_compare_function cmp_result;       /**< @brief Compare expected and computed results and returns zero when they are equal. */
} TestsWithSortingFunction;

/**
 * @brief Fixtures are prepared by the #fixture_setup function by linking or
 *        deep-copying the #TestsWithSortingFunction structure.
 */
typedef struct {
  int   tests_count;                            /**< @brief Number of tests in the test case. */
  void *tests;                                  /**< @brief An array of test cases. */
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

/**
 * @brief Sorting test cases for simple arrays of int: base cases.
 */
const TestCase tc_int_base[] =
  {
    { "A simple array of ten elements must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { 7, 3, 9, 0, 1, 5, 2, 8, 4, 6 },
      (int []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } },

    { "A simple array of ten elements must be sorted in descending order.",
      DSC, sizeof(int), 10,
      (int []) { 7, 3, 9, 0, 1, 5, 2, 8, 4, 6 },
      (int []) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 } },

    { "An ascending sorted array of ten elements must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
      (int []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } },

    { "An ascending sorted array of ten elements must be sorted in descending order.",
      DSC, sizeof(int), 10,
      (int []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 },
      (int []) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 } },

    { "A descending sorted array of ten elements must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 },
      (int []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } },

    { "A descending sorted array of ten elements must be sorted in descending order.",
      DSC, sizeof(int), 10,
      (int []) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 },
      (int []) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 } },

    { "An array of ten equal elements must be sorted in descending order.",
      DSC, sizeof(int), 10,
      (int []) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
      (int []) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } },

    { "An array of ten equal elements must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
      (int []) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 } },

    { "An ascending sorted array of ten negative elements must be sorted in descending order.",
      DSC, sizeof(int), 10,
      (int []) { -9, -8, -7, -6, -5, -4, -3, -2, -1,  0 },
      (int []) {  0, -1, -2, -3, -4, -5, -6, -7, -8, -9 } },

    { "A descending sorted array of ten negative elements must be sorted in descending order.",
      DSC, sizeof(int), 10,
      (int []) {  0, -1, -2, -3, -4, -5, -6, -7, -8, -9 },
      (int []) {  0, -1, -2, -3, -4, -5, -6, -7, -8, -9 } },

    { "An ascending sorted array of ten negative elements must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { -9, -8, -7, -6, -5, -4, -3, -2, -1,  0 },
      (int []) { -9, -8, -7, -6, -5, -4, -3, -2, -1,  0 } },

    { "An array of ten elements, nine zeros and a one, must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 },
      (int []) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 } },

    { "An array of ten elements, seven zeros and three ones, must be sorted in ascending order.",
      ASC, sizeof(int), 10,
      (int []) { 1, 0, 0, 0, 0, 0, 1, 0, 0, 1 },
      (int []) { 0, 0, 0, 0, 0, 0, 0, 1, 1, 1 } },

    { "An array of two elements, must be sorted in ascending order.",
      ASC, sizeof(int), 2,
      (int []) { 1, 0 },
      (int []) { 0, 1 } },

    { "An array of two elements, must be sorted in ascending order.",
      ASC, sizeof(int), 2,
      (int []) { 0, 1 },
      (int []) { 0, 1 } },

    { "An array of two elements, must be sorted in ascending order.",
      ASC, sizeof(int), 2,
      (int []) { 0, 0 },
      (int []) { 0, 0 } },

    { "An array of one element, must be sorted in ascending order.",
      ASC, sizeof(int), 1,
      (int []) { 0 },
      (int []) { 0 } },

    { "An empty array, must be sorted in ascending order.",
      ASC, sizeof(int), 0,
      (int []) { 0 },
      (int []) { 0 } },

    { "An array of seven elements, five zeros and two ones, must be sorted in ascending order.",
      ASC, sizeof(int), 7,
      (int []) { 1, 0, 0, 0, 0, 0, 1 },
      (int []) { 0, 0, 0, 0, 0, 1, 1 } },

    { NULL, ASC, sizeof(int), 1, (int []) {0}, (int []) {0} }
  };

/**
 * @brief Sorting test cases for simple arrays of uint64_t: base cases.
 */
const TestCase tc_uint64_t_base[] =
  {
    { "A simple array of ten elements must be sorted in ascending order.",
      ASC, sizeof(uint64_t), 10,
      (uint64_t []) { 7, 3, 9, 0, 1, 5, 2, 8, 4, 6 },
      (uint64_t []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } },

    { "A simple array of ten elements must be sorted in descending order.",
      DSC, sizeof(uint64_t), 10,
      (uint64_t []) { 7, 3, 9, 0, 1, 5, 2, 8, 4, 6 },
      (uint64_t []) { 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 } },

    { "An array of three elements, min_uint64, 0, max_uint64, must be sorted in ascending order.",
      ASC, sizeof(uint64_t), 4,
      (uint64_t []) { MIN_UINT64, 1, LARGE_UINT64, MAX_UINT64 },
      (uint64_t []) { MIN_UINT64, 1, LARGE_UINT64, MAX_UINT64 } },

    { "An array of three elements, min_uint64, 0, max_uint64, must be sorted in descending order.",
      DSC, sizeof(uint64_t), 4,
      (uint64_t []) { MIN_UINT64, 1, LARGE_UINT64, MAX_UINT64 },
      (uint64_t []) { MAX_UINT64, LARGE_UINT64, 1, MIN_UINT64 } },

    { NULL, ASC, sizeof(uint64_t), 1, (uint64_t []) {0}, (uint64_t []) {0} }
  };



/*
 * Tests with sorting function definitions.
 */

/* Qsort */

/**
 * @brief Qsort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_qsort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    qsort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Qsort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_qsort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    qsort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Qsort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_qsort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    qsort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Insertion-sort */

/**
 * @brief Insertion-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_insertionsort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_insertionsort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Insertion-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_insertionsort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_insertionsort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Insertion-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_insertionsort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_insertionsort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Binary-sort */

/**
 * @brief Binary-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_binarysort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_binarysort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Binary-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_binarysort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_binarysort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Binary-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_binarysort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_binarysort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Heap-sort */

/**
 * @brief Heap-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_heapsort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_heapsort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Heap-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_heapsort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_heapsort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Heap-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_heapsort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_heapsort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Smooth-sort */

/**
 * @brief Smooth-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_smoothsort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_smoothsort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Smooth-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_smoothsort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_smoothsort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Smooth-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_smoothsort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_smoothsort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Quick-sort */

/**
 * @brief Quick-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_quicksort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_quicksort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Quick-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_quicksort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_quicksort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Quick-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_quicksort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_quicksort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Shell-sort */

/**
 * @brief Shell-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_shellsort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_shellsort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Shell-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_shellsort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_shellsort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Shell-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_shellsort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_shellsort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Merge-sort */

/**
 * @brief Merge-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_mergesort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_mergesort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Merge-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_mergesort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_mergesort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Merge-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_mergesort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_mergesort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/* Tim-sort */

/**
 * @brief Tim-sort is applied to the double base test case.
 */
const TestsWithSortingFunction twsf_double_base_timsort =
  {
    (gconstpointer) tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_timsort,
    sizeof(double),
    sort_utils_double_cmp
  };

/**
 * @brief Tim-sort is applied to the int base test case.
 */
const TestsWithSortingFunction twsf_int_base_timsort =
  {
    (gconstpointer) tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_timsort,
    sizeof(int),
    sort_utils_int_cmp
  };

/**
 * @brief Tim-sort is applied to the uint64_t base test case.
 */
const TestsWithSortingFunction twsf_uint64_t_base_timsort =
  {
    (gconstpointer) tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_timsort,
    sizeof(uint64_t),
    sort_utils_uint64_t_cmp
  };



/*
 * Test function prototypes.
 */

static void sort_utils_double_compare_test (void);

static void sort_utils_int_compare_test (void);

static void sort_utils_uint64_t_compare_test (void);

static void sort_utils_int64_t_compare_test (void);

static void sort_utils_qsort_asc_d_1_rand_test (void);
static void sort_utils_qsort_asc_d_n_rand_test (void);
static void sort_utils_qsort_dsc_d_1_rand_test (void);
static void sort_utils_qsort_dsc_d_n_rand_test (void);
static void sort_utils_qsort_asc_d_rand_perf_test (void);

static void sort_utils_insertionsort_asc_d_1_rand_test (void);
static void sort_utils_insertionsort_asc_d_n_rand_test (void);
static void sort_utils_insertionsort_dsc_d_1_rand_test (void);
static void sort_utils_insertionsort_dsc_d_n_rand_test (void);
static void sort_utils_insertionsort_asc_d_rand_perf_test (void);

static void sort_utils_binarysort_asc_d_1_rand_test (void);
static void sort_utils_binarysort_asc_d_n_rand_test (void);
static void sort_utils_binarysort_dsc_d_1_rand_test (void);
static void sort_utils_binarysort_dsc_d_n_rand_test (void);
static void sort_utils_binarysort_asc_d_rand_perf_test (void);

static void sort_utils_heapsort_asc_d_1_rand_test (void);
static void sort_utils_heapsort_asc_d_n_rand_test (void);
static void sort_utils_heapsort_dsc_d_1_rand_test (void);
static void sort_utils_heapsort_dsc_d_n_rand_test (void);
static void sort_utils_heapsort_asc_d_rand_perf_test (void);

static void sort_utils_smoothsort_asc_d_1_rand_test (void);
static void sort_utils_smoothsort_asc_d_n_rand_test (void);
static void sort_utils_smoothsort_dsc_d_1_rand_test (void);
static void sort_utils_smoothsort_dsc_d_n_rand_test (void);
static void sort_utils_smoothsort_asc_d_rand_perf_test (void);

static void sort_utils_quicksort_asc_d_1_rand_test (void);
static void sort_utils_quicksort_asc_d_n_rand_test (void);
static void sort_utils_quicksort_dsc_d_1_rand_test (void);
static void sort_utils_quicksort_dsc_d_n_rand_test (void);
static void sort_utils_quicksort_asc_d_rand_perf_test (void);

static void sort_utils_shellsort_asc_d_1_rand_test (void);
static void sort_utils_shellsort_asc_d_n_rand_test (void);
static void sort_utils_shellsort_dsc_d_1_rand_test (void);
static void sort_utils_shellsort_dsc_d_n_rand_test (void);
static void sort_utils_shellsort_asc_d_rand_perf_test (void);

static void sort_utils_mergesort_asc_d_1_rand_test (void);
static void sort_utils_mergesort_asc_d_n_rand_test (void);
static void sort_utils_mergesort_dsc_d_1_rand_test (void);
static void sort_utils_mergesort_dsc_d_n_rand_test (void);
static void sort_utils_mergesort_asc_d_rand_perf_test (void);

static void sort_utils_timsort_asc_d_1_rand_test (void);
static void sort_utils_timsort_asc_d_n_rand_test (void);
static void sort_utils_timsort_dsc_d_1_rand_test (void);
static void sort_utils_timsort_dsc_d_n_rand_test (void);
static void sort_utils_timsort_asc_d_rand_perf_test (void);



/*
 * Helper function prototypes.
 */

static void
fixture_setup (Fixture *fixture,
               gconstpointer test_data);

static void
fixture_teardown (Fixture *fixture,
                  gconstpointer test_data);

static void
hlp_run_tests_with_sorting_function (Fixture *fixture,
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

  g_test_add_func("/sort_utils/sort_utils_int_compare_test", sort_utils_int_compare_test);

  g_test_add_func("/sort_utils/sort_utils_uint64_t_compare_test", sort_utils_uint64_t_compare_test);

  g_test_add_func("/sort_utils/sort_utils_int64_t_compare_test", sort_utils_int64_t_compare_test);


  g_test_add("/sort_utils/double_base_qsort",
             Fixture,
             (gconstpointer) &twsf_double_base_qsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_qsort",
             Fixture,
             (gconstpointer) &twsf_int_base_qsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_qsort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_qsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_qsort_asc_d_1_rand_test", sort_utils_qsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_qsort_dsc_d_1_rand_test", sort_utils_qsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_qsort_asc_d_n_rand_test", sort_utils_qsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_qsort_dsc_d_n_rand_test", sort_utils_qsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_insertionsort",
             Fixture,
             (gconstpointer) &twsf_double_base_insertionsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_insertionsort",
             Fixture,
             (gconstpointer) &twsf_int_base_insertionsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_insertionsort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_insertionsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_1_rand_test", sort_utils_insertionsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_dsc_d_1_rand_test", sort_utils_insertionsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_n_rand_test", sort_utils_insertionsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_insertionsort_dsc_d_n_rand_test", sort_utils_insertionsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_binarysort",
             Fixture,
             (gconstpointer) &twsf_double_base_binarysort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_binarysort",
             Fixture,
             (gconstpointer) &twsf_int_base_binarysort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_binarysort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_binarysort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_binarysort_asc_d_1_rand_test", sort_utils_binarysort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_binarysort_dsc_d_1_rand_test", sort_utils_binarysort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_binarysort_asc_d_n_rand_test", sort_utils_binarysort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_binarysort_dsc_d_n_rand_test", sort_utils_binarysort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_heapsort",
             Fixture,
             (gconstpointer) &twsf_double_base_heapsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_heapsort",
             Fixture,
             (gconstpointer) &twsf_int_base_heapsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_heapsort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_heapsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_1_rand_test", sort_utils_heapsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_dsc_d_1_rand_test", sort_utils_heapsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_n_rand_test", sort_utils_heapsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_heapsort_dsc_d_n_rand_test", sort_utils_heapsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_smoothsort",
             Fixture,
             (gconstpointer) &twsf_double_base_smoothsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_smoothsort",
             Fixture,
             (gconstpointer) &twsf_int_base_smoothsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_smoothsort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_smoothsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_smoothsort_asc_d_1_rand_test", sort_utils_smoothsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_dsc_d_1_rand_test", sort_utils_smoothsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_asc_d_n_rand_test", sort_utils_smoothsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_smoothsort_dsc_d_n_rand_test", sort_utils_smoothsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_quicksort",
             Fixture,
             (gconstpointer) &twsf_double_base_quicksort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_quicksort",
             Fixture,
             (gconstpointer) &twsf_int_base_quicksort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_quicksort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_quicksort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_quicksort_asc_d_1_rand_test", sort_utils_quicksort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_quicksort_dsc_d_1_rand_test", sort_utils_quicksort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_quicksort_asc_d_n_rand_test", sort_utils_quicksort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_quicksort_dsc_d_n_rand_test", sort_utils_quicksort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_shellsort",
             Fixture,
             (gconstpointer) &twsf_double_base_shellsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_shellsort",
             Fixture,
             (gconstpointer) &twsf_int_base_shellsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_shellsort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_shellsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_shellsort_asc_d_1_rand_test", sort_utils_shellsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_shellsort_dsc_d_1_rand_test", sort_utils_shellsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_shellsort_asc_d_n_rand_test", sort_utils_shellsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_shellsort_dsc_d_n_rand_test", sort_utils_shellsort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_mergesort",
             Fixture,
             (gconstpointer) &twsf_double_base_mergesort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_mergesort",
             Fixture,
             (gconstpointer) &twsf_int_base_mergesort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_mergesort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_mergesort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_mergesort_asc_d_1_rand_test", sort_utils_mergesort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_mergesort_dsc_d_1_rand_test", sort_utils_mergesort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_mergesort_asc_d_n_rand_test", sort_utils_mergesort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_mergesort_dsc_d_n_rand_test", sort_utils_mergesort_dsc_d_n_rand_test);


  g_test_add("/sort_utils/double_base_timsort",
             Fixture,
             (gconstpointer) &twsf_double_base_timsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/int_base_timsort",
             Fixture,
             (gconstpointer) &twsf_int_base_timsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add("/sort_utils/uint64_t_base_timsort",
             Fixture,
             (gconstpointer) &twsf_uint64_t_base_timsort,
             fixture_setup,
             hlp_run_tests_with_sorting_function,
             fixture_teardown);

  g_test_add_func("/sort_utils/sort_utils_timsort_asc_d_1_rand_test", sort_utils_timsort_asc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_timsort_dsc_d_1_rand_test", sort_utils_timsort_dsc_d_1_rand_test);
  g_test_add_func("/sort_utils/sort_utils_timsort_asc_d_n_rand_test", sort_utils_timsort_asc_d_n_rand_test);
  g_test_add_func("/sort_utils/sort_utils_timsort_dsc_d_n_rand_test", sort_utils_timsort_dsc_d_n_rand_test);


  if (g_test_perf()) {
    g_test_add_func("/sort_utils/sort_utils_qsort_asc_d_rand_perf_test", sort_utils_qsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_insertionsort_asc_d_rand_perf_test", sort_utils_insertionsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_binarysort_asc_d_rand_perf_test", sort_utils_binarysort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_heapsort_asc_d_rand_perf_test", sort_utils_heapsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_smoothsort_asc_d_rand_perf_test", sort_utils_smoothsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_quicksort_asc_d_rand_perf_test", sort_utils_quicksort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_shellsort_asc_d_rand_perf_test", sort_utils_shellsort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_mergesort_asc_d_rand_perf_test", sort_utils_mergesort_asc_d_rand_perf_test);
    g_test_add_func("/sort_utils/sort_utils_timsort_asc_d_rand_perf_test", sort_utils_timsort_asc_d_rand_perf_test);
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

  a = 2.;
  b = 3.;
  g_assert_cmpint(sort_utils_double_cmp(&a, &b), ==, -1);
  g_assert_cmpint(sort_utils_double_icmp(&a, &b), ==, +1);

  a = 3.;
  b = 3.;
  g_assert_cmpint(sort_utils_double_cmp(&a, &b), ==,  0);
  g_assert_cmpint(sort_utils_double_icmp(&a, &b), ==,  0);

  a = 3.;
  b = 2.;
  g_assert_cmpint(sort_utils_double_cmp(&a, &b), ==, +1);
  g_assert_cmpint(sort_utils_double_icmp(&a, &b), ==, -1);
}

static void
sort_utils_int_compare_test (void)
{
  int a;
  int b;

  a = 2;
  b = 3;
  g_assert_cmpint(sort_utils_int_cmp(&a, &b), ==, -1);
  g_assert_cmpint(sort_utils_int_icmp(&a, &b), ==, +1);

  a = 3;
  b = 3;
  g_assert_cmpint(sort_utils_int_cmp(&a, &b), ==,  0);
  g_assert_cmpint(sort_utils_int_icmp(&a, &b), ==,  0);

  a = 3;
  b = 2;
  g_assert_cmpint(sort_utils_int_cmp(&a, &b), ==, +1);
  g_assert_cmpint(sort_utils_int_icmp(&a, &b), ==, -1);
}

static void
sort_utils_uint64_t_compare_test (void)
{
  uint64_t a;
  uint64_t b;

  a = 2;
  b = 3;
  g_assert_cmpint(sort_utils_uint64_t_cmp(&a, &b), ==, -1);
  g_assert_cmpint(sort_utils_uint64_t_icmp(&a, &b), ==, +1);

  a = 3;
  b = 3;
  g_assert_cmpint(sort_utils_uint64_t_cmp(&a, &b), ==,  0);
  g_assert_cmpint(sort_utils_uint64_t_icmp(&a, &b), ==,  0);

  a = 3;
  b = 2;
  g_assert_cmpint(sort_utils_uint64_t_cmp(&a, &b), ==, +1);
  g_assert_cmpint(sort_utils_uint64_t_icmp(&a, &b), ==, -1);

  a = MAX_UINT64;
  b = LARGE_UINT64;
  g_assert_true(sort_utils_uint64_t_cmp(&a, &b) > 0);
}


static void
sort_utils_int64_t_compare_test (void)
{
  int64_t a;
  int64_t b;

  a = 2;
  b = 3;
  g_assert_cmpint(sort_utils_int64_t_cmp(&a, &b), ==, -1);
  g_assert_cmpint(sort_utils_int64_t_icmp(&a, &b), ==, +1);

  a = 3;
  b = 3;
  g_assert_cmpint(sort_utils_int64_t_cmp(&a, &b), ==,  0);
  g_assert_cmpint(sort_utils_int64_t_icmp(&a, &b), ==,  0);

  a = 3;
  b = 2;
  g_assert_cmpint(sort_utils_int64_t_cmp(&a, &b), ==, +1);
  g_assert_cmpint(sort_utils_int64_t_icmp(&a, &b), ==, -1);

  a = MAX_INT64;
  b = LARGE_INT64;
  g_assert_true(sort_utils_int64_t_cmp(&a, &b) > 0);

  a = 0;
  b = LARGE_INT64;
  g_assert_true(sort_utils_int64_t_cmp(&a, &b) < 0);

  a = 0;
  b = SMALL_INT64;
  g_assert_true(sort_utils_int64_t_cmp(&a, &b) > 0);

  a = MIN_INT64;
  b = SMALL_INT64;
  g_assert_true(sort_utils_int64_t_cmp(&a, &b) < 0);
}


/********************************************/
/* Unit tests for stdlib.h qsort algorithm. */
/********************************************/

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



/*****************************************/
/* Unit tests for binary-sort algorithm. */
/*****************************************/

static void
sort_utils_binarysort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_binarysort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_binarysort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_binarysort_dsc_d, 1024, 1, 0, 175, DSC);
}

static void
sort_utils_binarysort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_binarysort_asc_d, 1024, 3, 2, 322, ASC);
  hlp_run_sort_d_random_test(sort_utils_binarysort_asc_d, 1023, 3, 2, 655, ASC);
  hlp_run_sort_d_random_test(sort_utils_binarysort_asc_d, 1025, 3, 2, 983, ASC);
}

static void
sort_utils_binarysort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_binarysort_dsc_d, 1024, 3, 2, 114, DSC);
  hlp_run_sort_d_random_test(sort_utils_binarysort_dsc_d, 1023, 3, 2, 563, DSC);
  hlp_run_sort_d_random_test(sort_utils_binarysort_dsc_d, 1025, 3, 2, 940, DSC);
}

static void
sort_utils_binarysort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_binarysort_asc_d, 1024, 8, 2, 175, ASC);
}



/***************************************/
/* Unit tests for heap-sort algorithm. */
/***************************************/

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



/****************************************/
/* Unit tests for shell-sort algorithm. */
/****************************************/

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



/****************************************/
/* Unit tests for merge-sort algorithm. */
/****************************************/

static void
sort_utils_mergesort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_mergesort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_mergesort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_mergesort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_mergesort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_mergesort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(sort_utils_mergesort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(sort_utils_mergesort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_mergesort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_mergesort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(sort_utils_mergesort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(sort_utils_mergesort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_mergesort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_mergesort_asc_d, 1024, 15, 2, 175, ASC);
}



/**************************************/
/* Unit tests for tim-sort algorithm. */
/**************************************/

static void
sort_utils_timsort_asc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_timsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_timsort_dsc_d_1_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_timsort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_timsort_asc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_timsort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(sort_utils_timsort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(sort_utils_timsort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_timsort_dsc_d_n_rand_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_timsort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(sort_utils_timsort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(sort_utils_timsort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_timsort_asc_d_rand_perf_test (void)
{
  hlp_run_sort_d_random_test(sort_utils_timsort_asc_d, 1024, 15, 2, 175, ASC);
}



/*
 * Internal functions.
 */

static void
fixture_setup (Fixture *fixture,
               gconstpointer tests_with_sorting_function)
{
  const size_t size_of_test_case = sizeof(TestCase);

  const TestsWithSortingFunction *const twsf = (TestsWithSortingFunction *) tests_with_sorting_function;
  const TestCase *const test_defs = (TestCase *) twsf->tests;

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
fixture_teardown (Fixture *fixture,
                  gconstpointer tests_with_sorting_function)
{
  TestCase *tests = (TestCase *) fixture->tests;
  for (int i = 0; i < fixture->tests_count; i++) {
    free(tests[i].elements);
  }
  free(fixture->tests);
}

static void
hlp_run_tests_with_sorting_function (Fixture *fixture,
                                     gconstpointer test_data)
{
  g_assert(fixture);
  g_assert(test_data);

  TestCase *tests = fixture->tests;
  g_assert(tests);

  const TestsWithSortingFunction *twsf = (const TestsWithSortingFunction *) test_data;
  const sort_utils_compare_function cmp_asc = twsf->cmp_asc;
  g_assert(cmp_asc);
  const sort_utils_compare_function cmp_dsc = twsf->cmp_dsc;
  g_assert(cmp_dsc);
  const sort_utils_sort_function sort = twsf->sort;
  g_assert(sort);
  const size_t es = twsf->element_size;
  const sort_utils_compare_function cmp_result = twsf->cmp_result;
  g_assert(cmp_result);

  for (int i = 0; i < fixture->tests_count; i++) {
    const TestCase *tc = &tests[i];
    sort_utils_compare_function cmp;
    switch (tc->versus) {
    case ASC:
      cmp = cmp_asc;
      break;
    case DSC:
      cmp = cmp_dsc;
      break;
    default:
      g_test_fail();
      return;
    }

    sort(tc->elements,
         tc->elements_count,
         es,
         cmp);

    for (int j = 0; j < tc->elements_count; j++) {
      const void *computed = (char *) tc->elements + j * es;
      const void *expected = (char *) tc->expected_sorted_sequence + j * es;
      g_assert(cmp_result(expected, computed) == 0);

    }

  }

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
