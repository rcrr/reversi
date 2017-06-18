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
#include "time_utils.h"
#include "prng.h"



#define MIN_UINT64   0
#define MAX_UINT64   0xffffffffffffffff
#define LARGE_UINT64 0xfffffffffffffffe

#define MIN_INT64   (-9223372036854775807LL-1LL)
#define SMALL_INT64 -9223372036854775807LL
#define MAX_INT64   +9223372036854775807LL
#define LARGE_INT64 +9223372036854775806LL



/**/
typedef void (*sort_double_fun) (double *const a, const int count);

/*
 * The sorting versus.
 *
 * Ascending means that a lesser element precede a greater one, descending
 * is the opposite.
 */
typedef enum {
  DSC,                                             /* Ascending. */
  ASC                                              /* Descending. */
} sorting_versus_t;

/*
 * A test case collects a set of elements and the expected sorted sequence.
 */
typedef struct {
  char             *test_label;                    /* Test label. */
  sorting_versus_t  versus;                        /* 0 for descending, and 1 for ascending. */
  size_t            element_size;                  /* Number of bytes needed by one element. */
  int               elements_count;                /* The size of the array to be sorted. */
  void             *elements;                      /* The data to be sorted. */
  void             *expected_sorted_sequence;      /* The expected sequence. */
} test_case_t;

/*
 * Tests with sorting function structure merges a list of test cases with a sorting implementation.
 */
typedef struct {
  const test_case_t           *tests;            /* An array of test cases. */
  sort_utils_compare_function  cmp_asc;          /* Compare function for ascending cases. */
  sort_utils_compare_function  cmp_dsc;          /* Compare function for descending cases. */
  sort_utils_sort_function     sort;             /* Sorting function. */
  size_t                       element_size;     /* Number of bytes needed by one element. */
} tests_with_sorting_function_t;

/*
 * Fixtures are prepared by the fixture_setup() function by linking or
 * deep-copying the tests_with_sorting_function_t structure.
 */
typedef struct {
  int   tests_count;                            /* Number of tests in the test case. */
  void *tests;                                  /* An array of test cases. */
} fixture_t;



/*
 * Sorting functions.
 */
static const sort_utils_sort_function sort_functions[] =
  {
    sort_utils_insertionsort,
    sort_utils_binarysort,
    sort_utils_heapsort,
    sort_utils_smoothsort,
    sort_utils_quicksort,
    sort_utils_shellsort,
    sort_utils_mergesort,
    sort_utils_timsort
  };

/*
 * Names of sorting functions.
 */
static const char *sort_function_names[] =
  {
    "insertion-sort",
    "binary-sort",
    "heap-sort",
    "smooth-sort",
    "quick-sort",
    "shell-sort",
    "merge-sort",
    "tim-sort"
  };

/*
 * Count for sorting functions.
 */
static const int sort_function_count = sizeof(sort_functions) / sizeof(sort_functions[0]);

/*
 * Sorting test cases for simple arrays of double: base cases.
 */
static const test_case_t tc_double_base[] =
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
 * Sorting test cases for simple arrays of int: base cases.
 */
static const test_case_t tc_int_base[] =
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

    { "An array of sixtyfour elements, must be sorted in ascending order.",
      ASC, sizeof(int), 64,
      (int []) { 0 , 52,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 61, 14, 15,
                 16, 17,  1, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 13, 30, 31,
                 32, 33, 34, 18, 36, 37, 38, 39, 40, 41, 42, 43, 44, 29, 46, 47,
                 48, 49, 50, 51, 35, 53, 54, 55, 56, 57, 58, 59, 60, 45, 62, 63 },
      (int []) { 0 ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
                 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 } },

    { "An array of nintysix elements, must be sorted in ascending order.",
      ASC, sizeof(int), 96,
      (int []) { 93, 52,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 61, 14, 15,
                 16, 17,  1, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 13, 30, 31,
                 32, 33, 34, 18, 36, 37, 38, 39, 40, 41, 42, 43, 44, 29, 46, 47,
                 48, 49, 50, 51, 35, 53, 54, 55, 56, 57, 58, 59, 60, 45, 62, 63,
                 81, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                 80, 64, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,  0, 94, 95 },
      (int []) { 0 ,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
                 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
                 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95 } },

    { NULL, ASC, sizeof(int), 1, (int []) {0}, (int []) {0} }
  };

/*
 * Sorting test cases for simple arrays of uint64_t: base cases.
 */
static const test_case_t tc_uint64_t_base[] =
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
 * Sorting test cases for simple arrays of int64_t: base cases.
 */
static const test_case_t tc_int64_t_base[] =
  {
    { "A simple array of ten elements must be sorted in ascending order.",
      ASC, sizeof(int64_t), 10,
      (int64_t []) { 7, 3, 9, 0, 1, 5, 2, 8, 4, 6 },
      (int64_t []) { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } },

    { "A simple array of ten elements must be sorted in descending order.",
      DSC, sizeof(int64_t), 10,
      (int64_t []) { -7,  3,  9,  0,  1,  5, -2,  8,  4,  6 },
      (int64_t []) {  9,  8,  6,  5,  4,  3,  1,  0, -2, -7 } },

    { "An array of three elements, min_uint64, 0, max_uint64, must be sorted in ascending order.",
      ASC, sizeof(int64_t), 5,
      (int64_t []) { MIN_INT64, SMALL_INT64, 1, LARGE_INT64, MAX_INT64 },
      (int64_t []) { MIN_INT64, SMALL_INT64, 1, LARGE_INT64, MAX_INT64 } },

    { "An array of three elements, min_uint64, 0, max_uint64, must be sorted in descending order.",
      DSC, sizeof(int64_t), 5,
      (int64_t []) { MIN_INT64, SMALL_INT64, 1, LARGE_INT64, MAX_INT64 },
      (int64_t []) { MAX_INT64, LARGE_INT64, 1, SMALL_INT64, MIN_INT64 } },

    { NULL, ASC, sizeof(uint64_t), 1, (int64_t []) {0}, (int64_t []) {0} }
  };



/*
 * Tests with sorting function definitions.
 */

/* Qsort */

/*
 * Qsort is applied to the double base test case.
 */
static const tests_with_sorting_function_t twsf_double_base_qsort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    qsort,
    sizeof(double),
  };

/*
 * Qsort is applied to the int base test case.
 */
static const tests_with_sorting_function_t twsf_int_base_qsort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    qsort,
    sizeof(int),
  };

/*
 * Qsort is applied to the uint64_t base test case.
 */
static const tests_with_sorting_function_t twsf_uint64_t_base_qsort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    qsort,
    sizeof(uint64_t),
  };

/*
 * Qsort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_qsort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    qsort,
    sizeof(int64_t),
  };



/* Insertion-sort */

/*
 * Insertion-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_insertionsort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_insertionsort,
    sizeof(double),
  };

/*
 * Insertion-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_insertionsort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_insertionsort,
    sizeof(int),
  };

/*
 * Insertion-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_insertionsort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_insertionsort,
    sizeof(uint64_t),
  };

/*
 * Insertion-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_insertionsort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_insertionsort,
    sizeof(int64_t),
  };



/* Binary-sort */

/*
 * Binary-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_binarysort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_binarysort,
    sizeof(double),
  };

/*
 * Binary-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_binarysort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_binarysort,
    sizeof(int),
  };

/*
 * Binary-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_binarysort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_binarysort,
    sizeof(uint64_t),
  };

/*
 * Binary-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_binarysort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_binarysort,
    sizeof(int64_t),
  };



/* Heap-sort */

/*
 * Heap-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_heapsort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_heapsort,
    sizeof(double),
  };

/*
 * Heap-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_heapsort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_heapsort,
    sizeof(int),
  };

/*
 * Heap-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_heapsort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_heapsort,
    sizeof(uint64_t),
  };

/*
 * Heap-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_heapsort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_heapsort,
    sizeof(int64_t),
  };



/* Smooth-sort */

/*
 * Smooth-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_smoothsort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_smoothsort,
    sizeof(double),
  };

/*
 * Smooth-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_smoothsort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_smoothsort,
    sizeof(int),
  };

/*
 * Smooth-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_smoothsort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_smoothsort,
    sizeof(uint64_t),
  };

/*
 * Smooth-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_smoothsort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_smoothsort,
    sizeof(int64_t),
  };



/* Quick-sort */

/*
 * Quick-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_quicksort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_quicksort,
    sizeof(double),
  };

/*
 * Quick-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_quicksort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_quicksort,
    sizeof(int),
  };

/*
 * Quick-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_quicksort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_quicksort,
    sizeof(uint64_t),
  };

/*
 * Quick-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_quicksort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_quicksort,
    sizeof(int64_t),
  };



/* Shell-sort */

/*
 * Shell-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_shellsort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_shellsort,
    sizeof(double),
  };

/*
 * Shell-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_shellsort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_shellsort,
    sizeof(int),
  };

/*
 * Shell-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_shellsort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_shellsort,
    sizeof(uint64_t),
  };

/*
 * Shell-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_shellsort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_shellsort,
    sizeof(int64_t),
  };



/* Merge-sort */

/*
 * Merge-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_mergesort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_mergesort,
    sizeof(double),
  };

/*
 * Merge-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_mergesort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_mergesort,
    sizeof(int),
  };

/*
 * Merge-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_mergesort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_mergesort,
    sizeof(uint64_t),
  };

/*
 * Merge-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_mergesort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_mergesort,
    sizeof(int64_t),
  };



/* Tim-sort */

/*
 * Tim-sort is applied to the double base test case.
 */
const tests_with_sorting_function_t twsf_double_base_timsort =
  {
    tc_double_base,
    sort_utils_double_cmp,
    sort_utils_double_icmp,
    sort_utils_timsort,
    sizeof(double),
  };

/*
 * Tim-sort is applied to the int base test case.
 */
const tests_with_sorting_function_t twsf_int_base_timsort =
  {
    tc_int_base,
    sort_utils_int_cmp,
    sort_utils_int_icmp,
    sort_utils_timsort,
    sizeof(int),
  };

/*
 * Tim-sort is applied to the uint64_t base test case.
 */
const tests_with_sorting_function_t twsf_uint64_t_base_timsort =
  {
    tc_uint64_t_base,
    sort_utils_uint64_t_cmp,
    sort_utils_uint64_t_icmp,
    sort_utils_timsort,
    sizeof(uint64_t),
  };

/*
 * Tim-sort is applied to the int64_t base test case.
 */
static const tests_with_sorting_function_t twsf_int64_t_base_timsort =
  {
    tc_int64_t_base,
    sort_utils_int64_t_cmp,
    sort_utils_int64_t_icmp,
    sort_utils_timsort,
    sizeof(int64_t),
  };



/*
 * Auxiliary functions.
 */

static void
fixture_setup (ut_test_t *const t)
{
  const tests_with_sorting_function_t *const twsf = (tests_with_sorting_function_t *) t->provided_data;
  const test_case_t *const test_defs = (test_case_t *) twsf->tests;

  fixture_t *fixture = (fixture_t *) malloc(sizeof(fixture_t));
  fixture->tests_count = 0;
  for (int i = 0;; i++) {
    const test_case_t *td = &test_defs[i];
    if (td->test_label == NULL) {
      fixture->tests_count = i;
      break;
    }
  }
  t->fixture = fixture;

  test_case_t *tests = (test_case_t *) malloc(fixture->tests_count * sizeof(test_case_t));
  for (int i = 0; i < fixture->tests_count; i++) {
    const test_case_t *td = &test_defs[i];
    test_case_t *tc = &tests[i];
    tc->test_label = td->test_label;
    tc->versus = td->versus;
    tc->element_size = td->element_size;
    tc->elements_count = td->elements_count;
    const size_t size_of_elements_array = td->elements_count * td->element_size;
    tc->elements = malloc(size_of_elements_array);
    memcpy(tc->elements, td->elements, size_of_elements_array);
    tc->expected_sorted_sequence = td->expected_sorted_sequence;
  }
  fixture->tests = tests;
}

static void
fixture_teardown (ut_test_t *const t)
{
  fixture_t *fixture = (fixture_t *) t->fixture;

  test_case_t *tests = (test_case_t *) fixture->tests;
  for (int i = 0; i < fixture->tests_count; i++) {
    free(tests[i].elements);
  }
  free(fixture->tests);
  free(fixture);
}

static void
hlp_run_tests_with_sorting_function (ut_test_t *const t)
{
  fixture_t *fixture = (fixture_t *) t->fixture;
  const tests_with_sorting_function_t *const twsf = (tests_with_sorting_function_t *) t->provided_data;

  assert(fixture);
  assert(twsf);

  test_case_t *tests = fixture->tests;
  assert(tests);

  const sort_utils_compare_function cmp_asc = twsf->cmp_asc;
  assert(cmp_asc);
  const sort_utils_compare_function cmp_dsc = twsf->cmp_dsc;
  assert(cmp_dsc);
  const sort_utils_sort_function sort = twsf->sort;
  assert(sort);
  const size_t es = twsf->element_size;

  for (int i = 0; i < fixture->tests_count; i++) {
    const test_case_t *tc = &tests[i];
    sort_utils_compare_function cmp;
    switch (tc->versus) {
    case ASC:
      cmp = cmp_asc;
      break;
    case DSC:
      cmp = cmp_dsc;
      break;
    default:
      ut_test_fail(t);
      return;
    }

    sort(tc->elements, tc->elements_count, es, cmp);

    for (int j = 0; j < tc->elements_count; j++) {
      const void *computed = (char *) tc->elements + j * es;
      const void *expected = (char *) tc->expected_sorted_sequence + j * es;
      ut_assert(t, cmp_asc(expected, computed) == 0);

    }
  }
}

static void
hlp_run_sort_d_random_test (ut_test_t *const t,
                            const sort_double_fun sort_fun,
                            const int array_length,
                            const int repetitions,
                            const int factor,
                            const int seed,
                            const sorting_versus_t v)
{
  assert(array_length > 0);

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  static const size_t size_of_double = sizeof(double);
  int len = array_length;

  for (int i = 0; i < repetitions; i++) {
    double *a = (double *) malloc(len * size_of_double);
    assert(a);

    for (int i = 0; i < len; i++) {
      a[i] = i;
    }

    prng_mt19937_t *prng = prng_mt19937_new();
    prng_mt19937_init_by_seed(prng, seed);
    prng_mt19937_shuffle_array_double(prng, a, len);
    prng_mt19937_free(prng);

    /* Sets the test start time. */
    clock_gettime(CLOCK_REALTIME, &start_time);
    (void) start_time;

    /* Starts the stop-watch. */
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

    sort_fun(a, len);

    /* Stops the stop-watch. */
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

    /* Sets the test end time. */
    clock_gettime(CLOCK_REALTIME, &end_time);
    (void) end_time;

    /* Computes the time taken, and updates the test cpu_time. */
    ret = timespec_diff(&cpu_time, &time_0, &time_1);
    (void) ret; assert(ret == 0);

    if (ut_run_time_is_verbose(t)) {
      fprintf(stdout, "  Sorting %10u items: [%6lld.%9ld]\n", len, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
    }

    for (int i = 0; i < len; i++) {
      switch (v) {
      case ASC:
        ut_assert(t, a[i] == i);
        break;
      case DSC:
        ut_assert(t, a[i] == len - (1 + i));
        break;
      default:
        ut_test_fail(t);
      return;
      }
    }

    free(a);

    len = len * factor;
  }
}

static test_case_t *
hlp_organpipe_int64_new (const size_t n,
                         const double jitters,
                         const int seed,
                         const sorting_versus_t versus)
{
  assert(0. <= jitters && jitters <= 1.);

  const size_t swaps = n * jitters;

  test_case_t *tc = (test_case_t *) malloc(sizeof(test_case_t));
  assert(tc);

  char *desc = malloc(128 * sizeof(char));
  assert(desc);
  sprintf(desc, "Organ pipe int64_t test case: n=%zu, jitters=%3.3f, seed=%d, versus=%s",
          n, jitters, seed, versus == ASC ? "ASC" : "DSC");

  int64_t *el = (int64_t *) malloc(2 * n * sizeof(int64_t));
  assert(el);

  int64_t *ex = (int64_t *) malloc(2 * n * sizeof(int64_t));
  assert(ex);

  for (int64_t k = n; k > 0; k--) {
    el[n - k]     = n - k;
    el[n + k - 1] = n - k;
  }

  if (versus == ASC) {
    for (int64_t k = 0; k < n; k++) {
      ex[2 * k]     = k;
      ex[2 * k + 1] = k;
    }
  } else {
    for (int64_t k = 0; k < n; k++) {
      ex[2 * k]     = n - k - 1;
      ex[2 * k + 1] = n - k - 1;
    }
  }

  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, seed);
  for (int k = 0; k < swaps; k++) {
    unsigned long int i = prng_mt19937_random_choice_from_finite_set(prng, 2 * n);
    unsigned long int j = prng_mt19937_random_choice_from_finite_set(prng, 2 * n);
    if (i != j) {
      const size_t tmp = *(el + i);
      *(el + i) = *(el + j);
      *(el + j) = tmp;
    }
  }
  prng_mt19937_free(prng);

  tc->test_label = desc;
  tc->versus = versus;
  tc->element_size = sizeof(int64_t);
  tc->elements_count = 2 * n;
  tc->elements = el;
  tc->expected_sorted_sequence = ex;

  return tc;
}

static void
test_case_free (test_case_t *tc)
{
  if (tc->test_label)
    free(tc->test_label);
  if (tc->elements)
    free(tc->elements);
  if (tc->expected_sorted_sequence)
    free(tc->expected_sorted_sequence);
  free(tc);
}

static void
test_case_verify_result (ut_test_t *const t,
                         const test_case_t *const tc,
                         const sort_utils_compare_function cmp)
{
  for (int i = 0; i < tc->elements_count; i++) {
    ut_assert(t, cmp(tc->expected_sorted_sequence, tc->elements) == 0);
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



/********************************************/
/* Unit tests for stdlib.h qsort algorithm. */
/********************************************/

static void
sort_utils_qsort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_qsort_asc_d, 1024, 1, 0, 654, ASC);
}

static void
sort_utils_qsort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_qsort_dsc_d, 1024, 1, 0, 870, DSC);
}

static void
sort_utils_qsort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_qsort_asc_d, 1024, 3, 2, 103, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_qsort_asc_d, 1023, 3, 2,  55, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_qsort_asc_d, 1025, 3, 2, 870, ASC);
}

static void
sort_utils_qsort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_qsort_dsc_d, 1024, 3, 2, 253, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_qsort_dsc_d, 1023, 3, 2, 763, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_qsort_dsc_d, 1025, 3, 2, 894, DSC);
}

static void
sort_utils_qsort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_qsort_asc_d, 1024, 15, 2, 175, ASC);
}



/********************************************/
/* Unit tests for insertion-sort algorithm. */
/********************************************/

static void
sort_utils_insertionsort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_insertionsort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_dsc_d, 1024, 1, 0, 175, DSC);
}

static void
sort_utils_insertionsort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_asc_d, 1024, 3, 2, 322, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_asc_d, 1023, 3, 2, 655, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_asc_d, 1025, 3, 2, 983, ASC);
}

static void
sort_utils_insertionsort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_dsc_d, 1024, 3, 2, 114, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_dsc_d, 1023, 3, 2, 563, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_dsc_d, 1025, 3, 2, 940, DSC);
}

static void
sort_utils_insertionsort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_insertionsort_asc_d, 1024, 8, 2, 175, ASC);
}



/*****************************************/
/* Unit tests for binary-sort algorithm. */
/*****************************************/

static void
sort_utils_binarysort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_binarysort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_dsc_d, 1024, 1, 0, 175, DSC);
}

static void
sort_utils_binarysort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_asc_d, 1024, 3, 2, 322, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_asc_d, 1023, 3, 2, 655, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_asc_d, 1025, 3, 2, 983, ASC);
}

static void
sort_utils_binarysort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_dsc_d, 1024, 3, 2, 114, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_dsc_d, 1023, 3, 2, 563, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_dsc_d, 1025, 3, 2, 940, DSC);
}

static void
sort_utils_binarysort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_binarysort_asc_d, 1024, 8, 2, 175, ASC);
}



/***************************************/
/* Unit tests for heap-sort algorithm. */
/***************************************/

static void
sort_utils_heapsort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_asc_d, 1024, 1, 0, 176, ASC);
}

static void
sort_utils_heapsort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_dsc_d, 1024, 1, 0, 174, DSC);
}

static void
sort_utils_heapsort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_asc_d, 1024, 3, 2, 326, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_asc_d, 1023, 3, 2, 643, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_asc_d, 1025, 3, 2, 987, ASC);
}

static void
sort_utils_heapsort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_dsc_d, 1024, 3, 2, 133, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_dsc_d, 1023, 3, 2, 545, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_dsc_d, 1025, 3, 2, 540, DSC);
}

static void
sort_utils_heapsort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_heapsort_asc_d, 1024, 15, 2, 175, ASC);
}



/*****************************************/
/* Unit tests for smooth-sort algorithm. */
/*****************************************/

static void
sort_utils_smoothsort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_smoothsort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_smoothsort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_smoothsort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_smoothsort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_smoothsort_asc_d, 1024, 15, 2, 175, ASC);
}



/****************************************/
/* Unit tests for quick-sort algorithm. */
/****************************************/

static void
sort_utils_quicksort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_quicksort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_dsc_d, 1024, 1, 0, 121, DSC);
}

static void
sort_utils_quicksort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_asc_d, 1024, 3, 2, 346, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_asc_d, 1023, 3, 2, 673, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_asc_d, 1025, 3, 2, 227, ASC);
}

static void
sort_utils_quicksort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_dsc_d, 1024, 3, 2, 433, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_dsc_d, 1023, 3, 2, 375, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_dsc_d, 1025, 3, 2, 560, DSC);
}

static void
sort_utils_quicksort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_quicksort_asc_d, 1024, 15, 2, 175, ASC);
}



/****************************************/
/* Unit tests for shell-sort algorithm. */
/****************************************/

static void
sort_utils_shellsort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_shellsort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_shellsort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_shellsort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_shellsort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_shellsort_asc_d, 1024, 15, 2, 175, ASC);
}



/****************************************/
/* Unit tests for merge-sort algorithm. */
/****************************************/

static void
sort_utils_mergesort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_mergesort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_mergesort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_mergesort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_mergesort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_mergesort_asc_d, 1024, 15, 2, 175, ASC);
}



/**************************************/
/* Unit tests for tim-sort algorithm. */
/**************************************/

static void
sort_utils_timsort_asc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_timsort_asc_d, 1024, 1, 0, 175, ASC);
}

static void
sort_utils_timsort_dsc_d_1_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_timsort_dsc_d, 1024, 1, 0, 171, DSC);
}

static void
sort_utils_timsort_asc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_timsort_asc_d, 1024, 3, 2, 366, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_timsort_asc_d, 1023, 3, 2, 683, ASC);
  hlp_run_sort_d_random_test(t, sort_utils_timsort_asc_d, 1025, 3, 2, 557, ASC);
}

static void
sort_utils_timsort_dsc_d_n_rand_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_timsort_dsc_d, 1024, 3, 2, 163, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_timsort_dsc_d, 1023, 3, 2, 785, DSC);
  hlp_run_sort_d_random_test(t, sort_utils_timsort_dsc_d, 1025, 3, 2, 650, DSC);
}

static void
sort_utils_timsort_asc_d_rand_perf_t (ut_test_t *const t)
{
  hlp_run_sort_d_random_test(t, sort_utils_timsort_asc_d, 1024, 15, 2, 175, ASC);
}

static void
abc_perf_t (ut_test_t *const t)
{
  assert(sort_function_count == (sizeof(sort_function_names) / sizeof(sort_function_names[0])));

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  const size_t n_base = 10000;
  const size_t step = 1;
  const size_t growth = 1;
  const size_t iterations = 64;

  const double jitters = 0.001;
  const int seed_base = 589;
  const int seed_increment = 7;

  const sorting_versus_t versus = ASC;

  for (int i = 0; i < sort_function_count; i++) {
    if (ut_run_time_is_verbose(t)) {
      fprintf(stdout, "  Function [%d]: %20s\n", i, sort_function_names[i]);
    }
    const sort_utils_sort_function sort = sort_functions[i];

    size_t n = n_base;
    int seed = seed_base;
    for (int i = 1; i <= iterations; i++) {

      test_case_t *tc = hlp_organpipe_int64_new(n, jitters, seed, versus);

      /* Sets the test start time. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      (void) start_time;

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      sort(tc->elements, tc->elements_count, tc->element_size, sort_utils_int64_t_cmp);

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Sets the test end time. */
      clock_gettime(CLOCK_REALTIME, &end_time);
      (void) end_time;

      /* Computes the time taken, and updates the test cpu_time. */
      ret = timespec_diff(&cpu_time, &time_0, &time_1);
      (void) ret; assert(ret == 0);

      if (ut_run_time_is_verbose(t)) {
        fprintf(stdout, "  Sorting %10u items: [%6lld.%9ld]\n", tc->elements_count, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
      }

      test_case_verify_result(t, tc, sort_utils_int64_t_cmp);
      test_case_free(tc);

      n = (n * growth) + step;
      seed += seed_increment;
    }
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

  ut_suite_t *const s = ut_suite_new(&config, "sort_utils");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_double_compare", sort_utils_double_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_int_compare", sort_utils_int_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_uint64_t_compare", sort_utils_uint64_t_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_int64_t_compare", sort_utils_int64_t_compare_t);

  /* Qsort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_qsort",
                            &twsf_double_base_qsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_qsort",
                            &twsf_int_base_qsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_qsort",
                            &twsf_uint64_t_base_qsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_qsort",
                            &twsf_int64_t_base_qsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_qsort_asc_d_1_rand", sort_utils_qsort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_qsort_dsc_d_1_rand", sort_utils_qsort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_qsort_asc_d_n_rand", sort_utils_qsort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_qsort_dsc_d_n_rand", sort_utils_qsort_dsc_d_n_rand_t);

  /* Insertion sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_insertionsort",
                            &twsf_double_base_insertionsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_insertionsort",
                            &twsf_int_base_insertionsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_insertionsort",
                            &twsf_uint64_t_base_insertionsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_insertionsort",
                            &twsf_int64_t_base_insertionsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_insertionsort_asc_d_1_rand", sort_utils_insertionsort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_insertionsort_dsc_d_1_rand", sort_utils_insertionsort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_01,   "sort_utils_insertionsort_asc_d_n_rand", sort_utils_insertionsort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_01,   "sort_utils_insertionsort_dsc_d_n_rand", sort_utils_insertionsort_dsc_d_n_rand_t);

  /* Binary sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_binarysort",
                            &twsf_double_base_binarysort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_binarysort",
                            &twsf_int_base_binarysort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_binarysort",
                            &twsf_uint64_t_base_binarysort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_binarysort",
                            &twsf_int64_t_base_binarysort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_binarysort_asc_d_1_rand", sort_utils_binarysort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_binarysort_dsc_d_1_rand", sort_utils_binarysort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_binarysort_asc_d_n_rand", sort_utils_binarysort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_binarysort_dsc_d_n_rand", sort_utils_binarysort_dsc_d_n_rand_t);

  /* Heap sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_heapsort",
                            &twsf_double_base_heapsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_heapsort",
                            &twsf_int_base_heapsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_heapsort",
                            &twsf_uint64_t_base_heapsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_heapsort",
                            &twsf_int64_t_base_heapsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_heapsort_asc_d_1_rand", sort_utils_heapsort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_heapsort_dsc_d_1_rand", sort_utils_heapsort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_heapsort_asc_d_n_rand", sort_utils_heapsort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_heapsort_dsc_d_n_rand", sort_utils_heapsort_dsc_d_n_rand_t);

  /* Smooth sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_smoothsort",
                            &twsf_double_base_smoothsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_smoothsort",
                            &twsf_int_base_smoothsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_smoothsort",
                            &twsf_uint64_t_base_smoothsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_smoothsort",
                            &twsf_int64_t_base_smoothsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_smoothsort_asc_d_1_rand", sort_utils_smoothsort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_smoothsort_dsc_d_1_rand", sort_utils_smoothsort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_smoothsort_asc_d_n_rand", sort_utils_smoothsort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_smoothsort_dsc_d_n_rand", sort_utils_smoothsort_dsc_d_n_rand_t);

  /* Quick sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_quicksort",
                            &twsf_double_base_quicksort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_quicksort",
                            &twsf_int_base_quicksort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_quicksort",
                            &twsf_uint64_t_base_quicksort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_quicksort",
                            &twsf_int64_t_base_quicksort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_quicksort_asc_d_1_rand", sort_utils_quicksort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_quicksort_dsc_d_1_rand", sort_utils_quicksort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_quicksort_asc_d_n_rand", sort_utils_quicksort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_quicksort_dsc_d_n_rand", sort_utils_quicksort_dsc_d_n_rand_t);

  /* Shell sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_shellsort",
                            &twsf_double_base_shellsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_shellsort",
                            &twsf_int_base_shellsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_shellsort",
                            &twsf_uint64_t_base_shellsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_shellsort",
                            &twsf_int64_t_base_shellsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_shellsort_asc_d_1_rand", sort_utils_shellsort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_shellsort_dsc_d_1_rand", sort_utils_shellsort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_shellsort_asc_d_n_rand", sort_utils_shellsort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_shellsort_dsc_d_n_rand", sort_utils_shellsort_dsc_d_n_rand_t);

  /* Merge sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_mergesort",
                            &twsf_double_base_mergesort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_mergesort",
                            &twsf_int_base_mergesort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_mergesort",
                            &twsf_uint64_t_base_mergesort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_mergesort",
                            &twsf_int64_t_base_mergesort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_mergesort_asc_d_1_rand", sort_utils_mergesort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_mergesort_dsc_d_1_rand", sort_utils_mergesort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_mergesort_asc_d_n_rand", sort_utils_mergesort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_mergesort_dsc_d_n_rand", sort_utils_mergesort_dsc_d_n_rand_t);

  /* Tim sort. */
  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "double_base_timsort",
                            &twsf_double_base_timsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int_base_timsort",
                            &twsf_int_base_timsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "uint64_t_base_timsort",
                            &twsf_uint64_t_base_timsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "int64_t_base_timsort",
                            &twsf_int64_t_base_timsort,
                            fixture_setup,
                            hlp_run_tests_with_sorting_function,
                            fixture_teardown);

  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_timsort_asc_d_1_rand", sort_utils_timsort_asc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_0001, "sort_utils_timsort_dsc_d_1_rand", sort_utils_timsort_dsc_d_1_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_timsort_asc_d_n_rand", sort_utils_timsort_asc_d_n_rand_t);
  ut_suite_add_simple_test(s, UT_MODE_STND,   UT_QUICKNESS_001,  "sort_utils_timsort_dsc_d_n_rand", sort_utils_timsort_dsc_d_n_rand_t);

  /* Performance tests. */
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_qsort_asc_d_rand_perf", sort_utils_qsort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_insertionsort_asc_d_rand_perf", sort_utils_insertionsort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_binarysort_asc_d_rand_perf", sort_utils_binarysort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_heapsort_asc_d_rand_perf", sort_utils_heapsort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_smoothsort_asc_d_rand_perf", sort_utils_smoothsort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_quicksort_asc_d_rand_perf", sort_utils_quicksort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_shellsort_asc_d_rand_perf", sort_utils_shellsort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_mergesort_asc_d_rand_perf", sort_utils_mergesort_asc_d_rand_perf_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "sort_utils_timsort_asc_d_rand_perf", sort_utils_timsort_asc_d_rand_perf_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_0001, "abc_perf", abc_perf_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
