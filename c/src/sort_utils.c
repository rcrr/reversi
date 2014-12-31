/**
 * @file
 *
 * @todo Refactor quick-sort.
 *       The swap macro is complicated .... and does not bring performance in.
 *
 * @todo Add tim-sort implementation to the list of available choices.
 *
 * @todo Add compare functions for pointers, and relative dedicated sort functions.
 *
 * @todo Add dedicated sort functions for object (pointer and structure) sorting.
 *       This version can have a dedicated swap macro/function.
 *
 * @todo Prepare tests for:
 *         - Added compare functions.
 *         - Dedicated integer, pointer, object sorting functions.
 *         - Write a sanity check test that verifies the list of sort functions comparing each other results.
 *
 * @todo Write performance tests for ordered and mostly ordered data, organ-pipe data
 *       few values data (eg. a long array of ones and zeroes).
 *       Rewrite test helper function (has to support more use cases than just random order).
 *
 * @todo Write a complete introduction to document the module.
 *
 * @todo Prepare one or more plots to document performances of the different functions.
 *
 *
 *
 * @brief Sort utilities module implementation.
 *
 * @details The functions made available by this module are all in-memory algorithms for "internal sorting".
 *
 * A lot of effort has been devoted to study and research sorting procedures, but a silver bullet has still to come.
 * Depending on the sorting data distribution, the patterns in the data, the type and number of the records,
 * the cost of comparing versus swapping, the availability of extra space, and finally our tolerance to the risk
 * of exploding running time and memory consumption caused by unpredicted data distributions that drive the
 * algorithm into the abyss, one procedure can be more efficient then the others.
 *
 * The algorithms here proposed are:
 *   - Insertion-sort
 *   - Heap-sort
 *   - Smooth-sort
 *   - Quick-sort
 *   - Shell-sort
 *   - Merge-sort
 *
 * There are mainly three different approaches when we have to rearrange records of information in a given order:
 *   - Address table sorting that means moving the complete records around.
 *   - Key-sorting that is carried out by preparing an auxiliary array of references and sort them.
 *   - List sorting that is done organizing auxiliary references into a linked list.
 *
 * The first one is more appealing when the data to be sorted is not too big when compared with the pointer size
 * of the machine, on the other side linked lists are very efficient when we have to make an insertion and
 * key-sorting shines when two elements has to be swapped.
 *
 *
 * @par sort_utils.c
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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include <glib.h>

#include "sort_utils.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
copy (void *const dest,
      const void *const src,
      const size_t element_size);

static void
swap (void *const a,
      void *const b,
      const size_t element_size);

/**
 * @endcond
 */



/*************************************/
/* Compare function implementations. */
/*************************************/

/* double */

/**
 * @brief Compares double values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `+1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `-1` when `a` is less then `b`
 *
 * @param a a pointer to the first double
 * @param b a pointer to the second double
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_double_cmp (const void *const a,
                       const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return (*x > *y) - (*x < *y);
}

/**
 * @brief Compares double values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `-1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `+1` when `a` is less then `b`
 *
 * @param a a pointer to the first double
 * @param b a pointer to the second double
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_double_icmp (const void *const a,
                        const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return (*x < *y) - (*x > *y);
}



/* int */

/**
 * @brief Compares int values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `+1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `-1` when `a` is less then `b`
 *
 * @param a a pointer to the first integer
 * @param b a pointer to the second integer
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_int_cmp (const void *const a,
                    const void *const b)
{
  const int *const x = (const int *const) a;
  const int *const y = (const int *const) b;
  return (*x > *y) - (*x < *y);
}

/**
 * @brief Compares int values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `-1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `+1` when `a` is less then `b`
 *
 * @param a a pointer to the first integer
 * @param b a pointer to the second integer
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_int_icmp (const void *const a,
                     const void *const b)
{
  const int *const x = (const int *const) a;
  const int *const y = (const int *const) b;
  return (*x < *y) - (*x > *y);
}



/* uint64_t */

/**
 * @brief Returns true when `a` is equal to `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `uint64_t` value pointed
 *          by `a` is equal to the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  `TRUE` when `a` is equal `b`.
 */
int
sort_utils_uint64_t_eq (const void *const a,
                        const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return *x == *y;
}

/**
 * @brief Returns true when `a` is greater than `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `uint64_t` value pointed
 *          by `a` is greater than the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  `TRUE` when `a` is greater than `b`.
 */
int
sort_utils_uint64_t_gt (const void *const a,
                        const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return *x > *y;
}

/**
 * @brief Returns true when `a` is greater than or equal to `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `uint64_t` value pointed
 *          by `a` is greater than or equal to the one pointed by `b`,
 *          otherwise it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  `TRUE` when `a` is greater than or equal to `b`.
 */
int
sort_utils_uint64_t_ge (const void *const a,
                        const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return *x >= *y;
}

/**
 * @brief Returns true when `a` is less than `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `uint64_t` value pointed
 *          by `a` is less than the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  `TRUE` when `a` is less than `b`.
 */
int
sort_utils_uint64_t_lt (const void *const a,
                        const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return *x < *y;
}

/**
 * @brief Returns true when `a` is less than or equal to `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `uint64_t` value pointed
 *          by `a` is less than or equal to the one pointed by `b`,
 *          otherwise it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  `TRUE` when `a` is less than or equal to `b`.
 */
int
sort_utils_uint64_t_le (const void *const a,
                        const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return *x <= *y;
}

/**
 * @brief Compares `uint64_t` values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `+1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `-1` when `a` is less then `b`
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_uint64_t_cmp (const void *const a,
                         const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return (*x > *y) - (*x < *y);
}

/**
 * @brief Compares `uint64_t` values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `-1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `+1` when `a` is less then `b`
 *
 * @param a a pointer to the first `uint64_t` value
 * @param b a pointer to the second `uint64_t` value
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_uint64_t_icmp (const void *const a,
                          const void *const b)
{
  const uint64_t *const x = (const uint64_t *const) a;
  const uint64_t *const y = (const uint64_t *const) b;
  return (*x < *y) - (*x > *y);
}



/* int64_t */

/**
 * @brief Returns true when `a` is equal to `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `int64_t` value pointed
 *          by `a` is equal to the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  `TRUE` when `a` is equal `b`.
 */
int
sort_utils_int64_t_eq (const void *const a,
                       const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return *x == *y;
}

/**
 * @brief Returns true when `a` is greater than `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `int64_t` value pointed
 *          by `a` is greater than the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  `TRUE` when `a` is greater than `b`.
 */
int
sort_utils_int64_t_gt (const void *const a,
                       const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return *x > *y;
}

/**
 * @brief Returns true when `a` is greater than or equal to `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `int64_t` value pointed
 *          by `a` is greater than or equal to the one pointed by `b`,
 *          otherwise it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  `TRUE` when `a` is greater than or equal to `b`.
 */
int
sort_utils_int64_t_ge (const void *const a,
                       const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return *x >= *y;
}

/**
 * @brief Returns true when `a` is less than `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `int64_t` value pointed
 *          by `a` is less than the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  `TRUE` when `a` is less than `b`.
 */
int
sort_utils_int64_t_lt (const void *const a,
                       const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return *x < *y;
}

/**
 * @brief Returns true when `a` is less than or equal to `b`.
 *
 * @details Compare function that returns `TRUE`, so a value
 *          different from zero, when the `int64_t` value pointed
 *          by `a` is less than or equal to the one pointed by `b`,
 *          otherwise it returns zero (`FALSE`).
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  `TRUE` when `a` is less than or equal to `b`.
 */
int
sort_utils_int64_t_le (const void *const a,
                       const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return *x <= *y;
}

/**
 * @brief Compares `int64_t` values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `+1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `-1` when `a` is less then `b`
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_int64_t_cmp (const void *const a,
                        const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return (*x > *y) - (*x < *y);
}

/**
 * @brief Compares `int64_t` values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `-1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `+1` when `a` is less then `b`
 *
 * @param a a pointer to the first `int64_t` value
 * @param b a pointer to the second `int64_t` value
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_int64_t_icmp (const void *const a,
                         const void *const b)
{
  const int64_t *const x = (const int64_t *const) a;
  const int64_t *const y = (const int64_t *const) b;
  return (*x < *y) - (*x > *y);
}



/*********************************/
/* Sort function implementations */
/*********************************/

/******************/
/* Insertion-sort */
/******************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the insertion-sort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 *          Insertion-sort is a naive algorithm with asymptotic time complexity of O(n^2),
 *          so quick-sort or heap-sort should be preferred even for small arrays.
 *          Nevertheless sometimes its simplicity makes it a valid choice. The given
 *          implementation is between two or three percent slower compared to a bare
 *          metal version, sorting doubles values, like the one here transcribed:
 * @code
 * void
 * sort_utils_insertionsort_asc_d (double *const a,
 *                                 const int count)
 *  for (int i = 1; i < count; i++) {
 *    for (int j = i; j > 0 && a[j - 1] > a[j]; j--) {
 *      double t = a[j]; a[j] = a[j - 1]; a[j - 1] = t;
 *    }
 *  }
 * @endcode
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_insertionsort (void *const a,
                          const size_t count,
                          const size_t element_size,
                          const sort_utils_compare_function cmp)
{
  char *ca = (char *) a;
  for (int i = 1; i < count; i++) {
    int j = i;
    for (;;) {
      if (j == 0 || cmp(ca + (j - 1) * element_size, ca + j * element_size) < 0) break;
      swap(ca + j * element_size, ca + (j - 1) * element_size, element_size);
      j--;
    }
  }
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the insertion-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_insertionsort_asc_d (double *const a,
                                const int count)
{
  sort_utils_insertionsort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the insertion-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_insertionsort_dsc_d (double *const a,
                                const int count)
{
  sort_utils_insertionsort(a, count, sizeof(double), sort_utils_double_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the insertion-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_insertionsort_asc_i (int *const a,
                                const int count)
{
  sort_utils_insertionsort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the insertion-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_insertionsort_dsc_i (int *const a,
                                const int count)
{
  sort_utils_insertionsort(a, count, sizeof(int), sort_utils_int_icmp);
}



/*************/
/* Heap-sort */
/*************/

/**
 * @cond
 */

/**
 * @brief Sift down function used by the heap-sort algorithm.
 *
 * @param [in,out] a            the array being sorted
 * @param [in]     start        tbd
 * @param [in]     end          tbd
 * @param [in]     element_size tbd
 * @param [in]     cmp          tbd
 */
static void
hps_sift_down (void *const a,
               const int start,
               const int end,
               const size_t element_size,
               const sort_utils_compare_function cmp)
{
  char *const ca = (char *const) a;
  int root = start;
  for (int child = 2 * root + 1; child < end; child = 2 * root + 1) {
    char *child_ptr = (char *) ca + child * element_size;
    char *const root_ptr = (char *const) ca + root * element_size;
    if ((child < end - 1) && cmp(child_ptr, child_ptr + element_size) < 0) {
      child += 1;
      child_ptr += element_size;
    }
    if (cmp(root_ptr, child_ptr) < 0) {
      swap(child_ptr, root_ptr, element_size);
      root = child;
    }
    else
      return;
  }
}

/**
 * @endcond
 */

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the heap-sort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_heapsort (void *const a,
                     const size_t count,
                     const size_t element_size,
                     const sort_utils_compare_function cmp)
{
  char *ca = (char *) a;
  for (int start = (count - 2) / 2; start >= 0; start--) {
    hps_sift_down(a, start, count, element_size, cmp);
  }
  for (int end = count - 1; end > 0; end--) {
    swap(ca + end * element_size, ca, element_size);
    hps_sift_down(a, 0, end, element_size, cmp);
  }
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the heap-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_asc_d (double *const a,
                           const int count)
{
  sort_utils_heapsort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the heap-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_dsc_i (int *const a,
                           const int count)
{
  sort_utils_heapsort(a, count, sizeof(int), sort_utils_int_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the heap-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_asc_i (int *const a,
                           const int count)
{
  sort_utils_heapsort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the heap-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_dsc_d (double *const a,
                           const int count)
{
  sort_utils_heapsort(a, count, sizeof(double), sort_utils_double_icmp);
}



/***************/
/* Smooth-sort */
/***************/

/**
 * @cond
 */

/**
 * @brief Function up as described by the sooth-sort paper.
 */
#define sms_up(ia, ib) do { unsigned long long int temp = ia; ia += ib + 1; ib = temp; } while (0)

/**
 * @brief Function down as described by the sooth-sort paper.
 */
#define sms_down(ia, ib) do { unsigned long long int temp = ib; ib = ia - ib - 1; ia = temp; } while (0)

/**
 * @brief Function sift as defined by the smooth-sort paper.
 *
 * @details When stretches thus parsed are viewed as post-order traversals of binary-trees,
 *          trustiness means that no son exceeds its father. A dubious stretch is made into
 *          a trusty one by applying the operation "sift" –a direct inheritance from heap-sort– to its root,
 *          where sift is defined as follow: sift applied to an element m[r1] that is exceeded by its
 *          largest son m[r2] consists of a swap of these two values, followed by an application of sift to m[r2].
 *
 * @param [in]     cmp compare function
 * @param [in]     es  element size
 * @param [in,out] r1  one of the reference described in the smooth-sort paper
 * @param [in]     tmp a reference to a temporary value having the same size of the array elements.
 * @param [in,out] b1  a reference to the b1 variable described in the paper
 * @param [in,out] c1  a reference to the c1 variable also described in the paper
 */
static void
sms_sift (const sort_utils_compare_function cmp,
          const size_t es,
          char *r1,
          char *const tmp,
          unsigned long long int *const b1,
          unsigned long long int *const c1)
{
  char *r0 = r1;
  copy(tmp, r0, es);
  while (*b1 >= 3) {
    char *r2 = r1 + (*c1 - *b1) * es;
    if (cmp(r1 - es, r2) >= 0) {
      r2 = r1 - es;
      sms_down(*b1, *c1);
    }
    if (cmp(r2, tmp) < 0) {
      *b1 = 1;
    } else {
      copy(r1, r2, es);
      r1 = r2;
      sms_down(*b1, *c1);
    }
  }
  if (r1 - r0) {
    copy(r1, tmp, es);
  }
}

/**
 * @brief Function trinkle as defined by the smooth-sort paper.
 *
 * @details In the case `p mod 4 = 1`, the standard concatenation ends on a dubious stretch of length b,
 *          which in this step becomes the last but one stretch of the standard concatenation and,
 *          hence, must be made trusty. In the case `q + c < N`, it suffices to apply sift to m[r] as before,
 *          since this stretch will later disappear from the standard concatenation. In the case `q + c >= N`,
 *          however, just applying sift to m[r] might violate P4' since this stretch of length b also occurs
 *          in the standard concatenation of length N. Making such a dubious stretch trusty and including its
 *          root in the sequence of ascending roots is achieved by applying "trinkle“ to m[r].
 *
 * @param [in]     cmp compare function
 * @param [in]     es  element size
 * @param [in,out] r1  one of the reference described in the smooth-sort paper
 * @param [in]     tmp a reference to a temporary value having the same size of the array elements.
 * @param [in]     p   the corresponding variable described in the paper
 * @param [in]     b   the corresponding variable described in the paper
 * @param [in]     c   the corresponding variable described in the paper
 * @param [in,out] b1  a reference to the b1 variable described in the paper
 * @param [in,out] c1  a reference to the c1 variable also described in the paper
 */
static void
sms_trinkle (const sort_utils_compare_function cmp,
             const size_t es,
             char *r1,
             char *const tmp,
             const unsigned long long int p,
             const unsigned long long int b,
             const unsigned long long int c,
             unsigned long long int *const b1,
             unsigned long long int *const c1)
{
  unsigned long long int p1 = p;
  *b1 = b;
  *c1 = c;
  char *r0 = r1;
  copy(tmp, r0, es);
  while (p1 > 0) {
    while ((p1 & 1) == 0) {
      p1 >>= 1;
      sms_up(*b1, *c1);
    }
    char *r3 = r1 - *b1 * es;
    if ((p1 == 1) || cmp(r3, tmp) < 0) {
      p1 = 0;
    } else {
      p1--;
      if (*b1 == 1) {
        copy(r1, r3, es);
        r1 = r3;
      } else {
        if (*b1 >= 3) {
          char *r2 = r1 + (*c1 - *b1) * es;
          if (cmp(r1 - es, r2) >= 0) {
            r2 = r1 - es;
            sms_down(*b1, *c1);
            p1 <<= 1;
          }
          if (cmp(r2, r3) < 0) {
            copy(r1, r3, es);
            r1 = r3;
          } else {
            copy(r1, r2, es);
            r1 = r2;
            sms_down(*b1, *c1);
            p1 = 0;
          }
        }
      }
    }
  }
  if (r0 - r1) {
    copy(r1, tmp, es);
  }
  sms_sift(cmp, es, r1, tmp, b1, c1);
}

/**
 * @brief Function semi-trinkle as defined by the smooth-sort paper.
 *
 * @details In the case `b >= 3`, the rightmost stretch of length b is replaced by two trusty ones; hence P3 is maintained.
 *          To restore P4 it would suffice to apply trinkle first to the root of the first new stretch and then to the
 *          root of the second new stretch, but this would fail to exploit the fact that the new stretches are already
 *          trusty to start with. This is exploited by applying "semitrinkle“ in order to those roots.
 *
 * @param [in]     cmp compare function
 * @param [in]     es  element size
 * @param [in,out] r   one of the reference described in the smooth-sort paper
 * @param [in,out] r1  one of the reference described in the smooth-sort paper
 * @param [in]     tmp a reference to a temporary value having the same size of the array elements.
 * @param [in]     p   the corresponding variable described in the paper
 * @param [in]     b   the corresponding variable described in the paper
 * @param [in]     c   the corresponding variable described in the paper
 * @param [in,out] b1  a reference to the b1 variable described in the paper
 * @param [in,out] c1  a reference to the c1 variable also described in the paper
 */
static void
sms_semitrinkle (const sort_utils_compare_function cmp,
                 const size_t es,
                 char *const r,
                 char *r1,
                 char *const tmp,
                 const unsigned long long int p,
                 const unsigned long long int b,
                 const unsigned long long int c,
                 unsigned long long int *const b1,
                 unsigned long long int *const c1)
{
  r1 = r - c * es;
  if (cmp(r1, r) >= 0) {
    swap(r, r1, es);
    sms_trinkle(cmp, es, r1, tmp, p, b, c, b1, c1);
  }
}

/**
 * @endcond
 */

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the smooth-sort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 *          Adapted from Dijkstra's paper: http://www.enterag.ch/hartwig/order/smoothsort.pdf
 *          See also: http://en.wikipedia.org/wiki/Smoothsort
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_smoothsort (void *const a,
                       const size_t count,
                       const size_t element_size,
                       const sort_utils_compare_function cmp)
{
  static const size_t size_of_char = sizeof(char);
  char *r = a;
  unsigned long long int c = 1;
  unsigned long long int p = 1;
  unsigned long long int b = 1;
  unsigned long long int b1;
  unsigned long long int c1;
  char *tmp = malloc(element_size * size_of_char);

  unsigned long long int q = 1;

  /* Building tree. */
  while (q < count) {
    char *r1 = r;
    if ((p & 7) == 3) {
      b1 = b;
      c1 = c;
      sms_sift(cmp, element_size, r1, tmp, &b1, &c1);
      p = (p + 1) >> 2;
      sms_up(b, c);
      sms_up(b, c);
    } else if ((p & 3) == 1) {
      if (q + c < count) {
        b1 = b;
        c1 = c;
        sms_sift(cmp, element_size, r1, tmp, &b1, &c1);
      } else {
        sms_trinkle(cmp, element_size, r1, tmp, p, b, c, &b1, &c1);
      }
      sms_down(b, c);
      p <<= 1;
      while (b > 1) {
        sms_down(b, c);
        p <<= 1;
      }
      p++;
    }
    q++;
    r += element_size;
  }
  char *r1 = r;
  sms_trinkle(cmp, element_size, r1, tmp, p, b, c, &b1, &c1);

  /* Building sorted array. */
  while (q > 1) {
    q--;
    if (b == 1) {
      r -= element_size;
      p--;
      while ((p & 1) == 0) {
        p >>= 1;
        sms_up(b, c);
      }
    } else {
      if (b >= 3) {
        p--;
        r = r + (c - b) * element_size;
        if (p > 0) {
          sms_semitrinkle(cmp, element_size, r, r1, tmp, p, b, c, &b1, &c1);
        }
        sms_down(b, c);
        p = (p << 1) + 1;
        r = r + c * element_size;
        sms_semitrinkle(cmp, element_size, r, r1, tmp, p, b, c, &b1, &c1);
        sms_down(b, c);
        p = (p << 1) + 1;
      }
    }
    /* Element q processed. */
  }
  /* Element 0 processed. */
  free(tmp);
}

#undef sms_up
#undef sms_down

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the smooth-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_smoothsort_asc_d (double *const a,
                             const int count)
{
  sort_utils_smoothsort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the smooth-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_smoothsort_dsc_d (double *const a,
                             const int count)
{
  sort_utils_smoothsort(a, count, sizeof(double), sort_utils_double_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the smooth-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_smoothsort_asc_i (int *const a,
                             const int count)
{
  sort_utils_smoothsort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the smooth-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_smoothsort_dsc_i (int *const a,
                             const int count)
{
  sort_utils_smoothsort(a, count, sizeof(int), sort_utils_int_icmp);
}



/**************/
/* Quick-sort */
/**************/

/**
 * @cond
 */

/**
 * @brief Used to optimize the swap operation for quick-sort.
 */
typedef long long int QksSwapWord;

/**
 * @brief The size of a register, on x86_64 it is eight.
 */
#define qks_sws sizeof(QksSwapWord)

/**
 * @brief This macro must be called at the beginning of a function using qks_swap.
 *        Variables `size_t es` and `QksSwapWord t` must be defined.
 */
#define qks_swap_init(a, es) swaptype =                                 \
    ((a - (char *) 0) | es) % qks_sws ? 2 : es > qks_sws ? 1 : 0

/**
 * @brief Inline exchange of values among `a` and `b`.
 */
#define qks_exch(a, b, t) (t = a, a = b, b = t)

/**
 * @brief Swaps values among `a` and `b`.
 *        A call to qks_swap_init macro must be done once in advance.
 */
#define qks_swap(a, b)                                                  \
  swaptype != 0 ? qks_swapfunc(a, b, es, swaptype) :                    \
    (void) qks_exch(*(QksSwapWord *) (a), *(QksSwapWord *) (b), t)

/**
 * @brief Swaps vectors.
 */
#define qks_vec_swap(a, b, n) if (n > 0) qks_swapfunc(a, b, n, swaptype)

/**
 * @brief Prepares pivot elements.
 */
#define qks_pv_init(pv, pm)                             \
  if (swaptype != 0) pv = a, qks_swap(pv, pm);          \
  else pv = (char *) &v, v = *(QksSwapWord *) pm

/**
 * @brief Swaps values among pointers `a` and `b`.
 */
static void
qks_swapfunc (char *a,
              char *b,
              size_t n,
              const int swaptype)
{
  if (swaptype <= 1) {
    QksSwapWord t;
    for ( ; n > 0; a += qks_sws, b += qks_sws, n -= qks_sws)
      qks_exch(*(QksSwapWord *) a, *(QksSwapWord *) b, t);
  } else {
    char t;
    for ( ; n > 0; a += 1, b += 1, n -= 1)
      qks_exch(*a, *b, t);
  }
}

/**
 * @brief Returns the median element among `a`, `b`, and `c`.
 */
static char *
qks_med3 (char *const a,
          char *const b,
          char *const c,
          int (*const cmp) ())
{
  return cmp(a, b) < 0 ?
    (cmp(b, c) < 0 ? b : cmp(a, c) < 0 ? c : a) :
    (cmp(b, c) > 0 ? b : cmp(a, c) > 0 ? c : a);
}

/**
 * @brief Returns the minimum between pointer differences `a` and `b`.
 */
static ptrdiff_t
qks_min (const ptrdiff_t a,
         const ptrdiff_t b)
{
  return (a < b) ? a : b;
}

/**
 * @endcond
 */

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the quick-sort algorithm.
 *          The compare function is a comparator and must return `-1`, `0`, or `+1` constrained by
 *          the sorting order of the two elements.
 *
 *          Adapted from the paper "Engineering a Sort Function" by Jon L. Bentley and M. Douglas McIlroy
 *          See: http://www.skidmore.edu/~meckmann/2009Spring/cs206/papers/spe862jb.pdf
 *          See also: http://en.wikipedia.org/wiki/Quicksort
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_quicksort (void *const a,
                      const size_t count,
                      const size_t element_size,
                      const sort_utils_compare_function cmp)
{
  static const int small_array_threshold = 7;
  static const int fst_threshold_for_med = 7;
  static const int snd_threshold_for_med = 40;

  char *ca = (char *) a;
  const size_t es = element_size;
  char *pa, *pb, *pc, *pd, *pl, *pm, *pn, *pv;
  int r, swaptype;
  QksSwapWord t, v;
  size_t s;

  qks_swap_init(ca, es);

  /* Runs insertion sort on arrays smaller than small_array_threshold. */
  if (count < small_array_threshold) {
    for (pm = ca + es; pm < ca + count * es; pm += es)
      for (pl = pm; pl > ca && cmp(pl - es, pl) > 0; pl -= es)
        qks_swap(pl, pl - es);
    return;
  }

  /* Computes the partition value, adopting three different strategies based on array length:
   *   - on small arrays, having length less than fst_threshold_for_med, it takes the middle element
   *   - on mid size arrays, having length in between small and large ones, it takes the median
   *     of the first, the last, and the middle elements.
   *   - on large arrays, having length equal or greater than snd_threshold_for_med, it takes
   *     the median of the medians of three groups of three elements taken at equal distance
   *     from each other.
   */
  pm = ca + (count >> 1) * es; /* Small arrays, middle element. */
  if (count > fst_threshold_for_med) {
    pl = ca;
    pn = ca + (count - 1) * es;
    if (count > snd_threshold_for_med) { /* Big arrays, pseudo-median of nine. */
      s = (count >> 3) * es;
      pl = qks_med3(pl, pl + s, pl + 2 * s, cmp);
      pm = qks_med3(pm - s, pm, pm + s, cmp);
      pn = qks_med3(pn - 2 * s, pn - s, pn, cmp);
    }
    pm = qks_med3(pl, pm, pn, cmp); /* Mid-size, med of three. */
  }

  /* Partitions the array in five groups:
   *  - the central one has one element, pointed by variable pv
   *  - on the left there are all the elements strictly smaller than pv
   *  - on the right there are all the element strictly larger than pv
   *  - on the far left, beginning from pointer pa are positioned elements
   *    equal to pv found on the left during partitioning
   *  - on the far right, beginning from pointer pd are positioned elements
   *    equal to pv found on the right during partitioning
   */
  qks_pv_init(pv, pm); /* Variable pv points to the partition value. */
  pa = pb = ca;
  pc = pd = ca + (count - 1) * es;
  for (;;) {
    while (pb <= pc && (r = cmp(pb, pv)) <= 0) {
      if (r == 0) { qks_swap(pa, pb); pa += es; }
      pb += es;
    }
    while (pc >= pb && (r = cmp(pc, pv)) >= 0) {
      if (r == 0) { qks_swap(pc, pd); pd -= es; }
      pc -= es;
    }
    if (pb > pc) break;
    qks_swap(pb, pc);
    pb += es;
    pc -= es;
  }

  /* Swaps far left and far right groups adjacent to pv value.*/
  pn = ca + count * es;
  s = qks_min(pa - ca, pb - pa); qks_vec_swap(ca, pb - s, s);
  s = qks_min(pd - pc, pn - pd - es); qks_vec_swap(pb, pn -s, s);

  /* Recurs on smaller, and larger elements. */
  if ((s = pb - pa) > es) sort_utils_quicksort(ca, s / es, es, cmp);
  if ((s = pd - pc) > es) sort_utils_quicksort(pn - s, s / es, es, cmp);
}

#undef qks_sws
#undef qks_swap_init
#undef qks_exch
#undef qks_swap
#undef qks_vec_swap
#undef qks_pv_init

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the quick-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_quicksort_asc_d (double *const a,
                            const int count)
{
  sort_utils_quicksort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the quick-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_quicksort_dsc_d (double *const a,
                            const int count)
{
  sort_utils_quicksort(a, count, sizeof(double), sort_utils_double_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the quick-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_quicksort_asc_i (int *const a,
                            const int count)
{
  sort_utils_quicksort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the quick-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_quicksort_dsc_i (int *const a,
                            const int count)
{
  sort_utils_quicksort(a, count, sizeof(int), sort_utils_int_icmp);
}



/**************/
/* Shell-sort */
/**************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the shell-sort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 * See:
 *
 * - The Wikipedia page: <a href="http://en.wikipedia.org/wiki/Shellsort" target="_blank">Shellsort</a>.
 *
 * - Donald E. Knuth (1998), Sorting and Searching (par. 5.2.1 - pp. 83-95). The Art of Computer Programming, Vol. 3 (2nd ed.). Boston: Addison–Wesley.
 *
 * - N. Tokuda, An Improved Shellsort, IFIP Transactions, A-12 (1992) 449-457.
 *
 * - Marcin Ciura, Best Increments for the Average Case of Shellsort,
 *     13th International Symposium on Fundamentals of Computation Theory, Riga, Latvia, Aug 22 2001;
 *     Lecture Notes in Computer Science 2001; 2138: 106-117.
 *     <a href="http://sun.aei.polsl.pl/~mciura/publikacje/shellsort.pdf" target="_blank">Download PDF</a>.
 *
 * A few sequences has been tested:
 *  1. `h(0) = 1, h[s+1] = 3 * h[s] + 1`, and stop with `h[t-1] when h[t+1] > count`.<br>
 *     The beginning of the sequence is: `1, 4, 13, 40, 121, 364, 1093, 3280, 9841, 29524, 88573, 265720, 797161, 2391484, 7174453, 21523360`.<br>
 *     The web site "The On-Line Encyclopedia of Integer  Sequences" has registered it as: <a href="https://oeis.org/A003462" target="_blank">A003462</a>.
 *  2. `h(0) = 1, h[s+1] = 2.25 * h[s] + 1`, and stop with `h[t-1] when h[t+1] > count`.<br>
 *     The beginning of the sequence is: `1, 4, 10, 24, 55, 125, 283, 638, 1437, 3235, 7280, 16381, 36859, 82934, 186603, 419858, 944682, 2125536, 4782457, 10760530, 24211194`.<br>
 *     It is not registered at "The On-Line Encyclopedia of Integer  Sequences".
 *  3. `h(k) = ceil((pow(9, k) - pow(4, k)) / (5 * pow(4, k - 1)))`.<br>
 *     The beginning of the sequence is: `1, 4, 9, 20, 46, 103, 233, 525, 1182, 2660, 5985, 13467, 30301, 68178, 153401, 345152`.<br>
 *     It is known as the "Tokuda's good set of increments for Shell sort" and it is registered as: <a href="https://oeis.org/A108870" target="_blank">A108870</a>.
 *  4. `1, 4, 10, 23, 57, 132, 301, 701`.<br>
 *     No other number is known after `701`, the sequence has been discovered by Marcin Ciura by empirical evidence, and is the best known sequence of increments for shell sort.<br>
 *     It is registered at OEIS as: <a href="https://oeis.org/A102549" target="_blank">A102549</a>.
 *
 * The first sequence is described by Knuth and works quite well, the second one is a sensible improvement, Knuth himself reports that it has been suggested by Tokuda.
 * The sequence number three, the Tokuda sequence, is slightly better than the second one, and it has been selected and applied here. The last one is too short to be adopted.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_shellsort (void *const a,
                      const size_t count,
                      const size_t element_size,
                      const sort_utils_compare_function cmp)
{
  static const unsigned long long int gap_seq[] = { 1,           4,           9,           20,
                                                    46,          103,         233,         525,
                                                    1182,        2660,        5985,        13467,
                                                    30301,       68178,       153401,      345152,
                                                    776591,      1747331,     3931496,     8845866,
                                                    19903198,    44782196,    100759940,   226709866,
                                                    510097200,   1147718700,  2582367076,  5810325920,
                                                    13073233321, 29414774973, 66183243690, 148912298303 };

  /* This code is here for documentation purposes, it computes the gap sequence. */
  if (FALSE) {
    for (int k = 0; k < 32; k++) {
      unsigned long long int h = ceil((pow(9, k + 1) - pow(4, k + 1)) / (5 * pow(4, k)));
      printf("h[%2d] = %12llu\n", k, h);
    }
    if (TRUE) return;
  }

  if (count < 2) return;

  const size_t array_size = element_size * count;

  long long int t = 0;
  for (; t < sizeof(gap_seq); t++) {
    if (gap_seq[t] > count) break;
  }
  t--;

  for (; t >= 0; t--) {
    const size_t gap = gap_seq[t];
    const size_t scaled_gap = element_size * gap;
    for (long long int i = scaled_gap; i < array_size; i += element_size) {
      for (long long int j = i - scaled_gap; ; j -= scaled_gap) {
        char *one_element = j + (char *) a;
        char *another_one = one_element + scaled_gap;
        if (cmp(one_element, another_one) < 0)
          break;
        swap(one_element, another_one, element_size);
        if (j < scaled_gap)
          break;
      }
    }
  }
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the shell-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_shellsort_asc_d (double *const a,
                            const int count)
{
  sort_utils_shellsort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the shell-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_shellsort_dsc_d (double *const a,
                            const int count)
{
  sort_utils_shellsort(a, count, sizeof(double), sort_utils_double_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the shell-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_shellsort_asc_i (int *const a,
                            const int count)
{
  sort_utils_shellsort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the shell-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_shellsort_dsc_i (int *const a,
                            const int count)
{
  sort_utils_shellsort(a, count, sizeof(int), sort_utils_int_icmp);
}



/**************/
/* Merge-sort */
/**************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          using auxiliary space applying the merge-sort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_mergesort (void *const a,
                      const size_t count,
                      const size_t element_size,
                      const sort_utils_compare_function cmp)
{
  void *aux = malloc(sizeof(element_size) * count);
  sort_utils_mergesort_a(a, count, element_size, cmp, aux);
  free(aux);
}

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          using auxiliary space applying the merge-sort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *          The auxiliary space must have the same size of the `a` array or larger,
 *          the content of it when the function returns is garbage.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element in array
 * @param [in]     es    the number of bytes used by one element
 * @param [in]     cmp   the compare function applied by the algorithm
 * @param [in]     aux   an auxiliary array
 */
void
sort_utils_mergesort_a (void *const a,
                        const size_t count,
                        const size_t es,
                        const sort_utils_compare_function cmp,
                        void *const aux)
{
  static const int small_array_threshold = 7;

  char *ca = (char *) a;
  char *caux = (char *) aux;

  /* Termination condition, it uses insertion-sort. */
  if (count < small_array_threshold) {
    sort_utils_insertionsort(a, count, es, cmp);
    return;
  }

  const size_t hc = count / 2;

  /* Recurs on the two half of the array. */
  sort_utils_mergesort_a(ca, hc, es, cmp, caux);
  sort_utils_mergesort_a(ca + hc * es, count - hc, es, cmp, caux);

  /* Merge phase. */
  char *left = ca;
  char *right = ca + hc * es;
  char *aux_ptr = caux;
  char *one_past_last_for_left = ca + hc * es;
  char *one_past_last_for_right = ca + count * es;
  while (left < one_past_last_for_left && right < one_past_last_for_right) {
    if (cmp(left, right) < 0) {
      copy(aux_ptr, left, es);
      left += es;
    } else {
      copy(aux_ptr, right, es);
      right += es;
    }
    aux_ptr += es;
  }
  while (left < one_past_last_for_left) {
    copy(aux_ptr, left, es);
    left += es;
    aux_ptr += es;
  }
  while (right < one_past_last_for_right) {
    copy(aux_ptr, right, es);
    right += es;
    aux_ptr += es;
  }
  memcpy(ca, caux, es * count);
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the merge-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_mergesort_asc_d (double *const a,
                            const int count)
{
  sort_utils_mergesort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the merge-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_mergesort_dsc_d (double *const a,
                            const int count)
{
  sort_utils_mergesort(a, count, sizeof(double), sort_utils_double_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the merge-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_mergesort_asc_i (int *const a,
                            const int count)
{
  sort_utils_mergesort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the merge-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_mergesort_dsc_i (int *const a,
                            const int count)
{
  sort_utils_mergesort(a, count, sizeof(int), sort_utils_int_icmp);
}



/************/
/* Tim-sort */
/************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the tim-sort algorithm.
 *          The compare function is a comparator and must return `-1`, `0`, or `+1` constrained by
 *          the sorting order of the two elements.
 *
 *          Tim-sort is a stable, adaptive, iterative merge-sort that requires far fewer than
 *          n lg(n) comparisons when running on partially sorted arrays, while
 *          offering performance comparable to a traditional merge-sort when run
 *          on random arrays.
 *
 *          This work is heavily inspired by the code written by Josh Bloch in Java
 *          and released to the public as part of the OpneJDK library.
 *
 *          The original implementation is found in Tim Peters's list sort for
 *          Python, which is described in detail here:
 *
 *          http://svn.python.org/projects/python/trunk/Objects/listsort.txt
 *
 *          Tim's C code may be found here:
 *
 *          http://svn.python.org/projects/python/trunk/Objects/listobject.c
 *
 *          The underlying techniques are described in this paper (and may have
 *          even earlier origins):
 *
 *          "Optimistic Sorting and Information Theoretic Complexity"
 *          Peter McIlroy
 *          SODA (Fourth Annual ACM-SIAM Symposium on Discrete Algorithms),
 *          pp 467-474, Austin, Texas, 25-27 January 1993.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_timsort (void *const a,
                    const size_t count,
                    const size_t element_size,
                    const sort_utils_compare_function cmp)
{
  g_assert(a);
  g_assert(element_size > 0);
  g_assert(cmp);
  if (count < 2) return;

  static const int min_merge = 32;

  if (count < min_merge) {
    sort_utils_insertionsort(a, count, element_size, cmp);
    /*
    int initRunLen = countRunAndMakeAscending(a, lo, hi, c);
    binarySort(a, lo, hi, lo + initRunLen, c);
    return;
    */
  }

  char *ca = (char *) a;
  for (int i = 1; i < count; i++) {
    int j = i;
    for (;;) {
      if (j == 0 || cmp(ca + (j - 1) * element_size, ca + j * element_size)) break;
      swap(ca + j * element_size, ca + (j - 1) * element_size, element_size);
      j--;
    }
  }
}



/**
 * @cond
 */

/*
 * Internal functions used by more than one algorithm.
 */

/**
 * @brief Copies values from `src` to `dest` pointers.
 *
 * @details It is an enhanced version of memcpy function,
 *          that leverages the dimension of the data copied
 *          when it fits into a standard register size.
 *
 * @param [in,out] dest         a pointer to the first byte to be overwritten
 * @param [in]     src          a pointer to the first byte of data to be copied
 * @param [in]     element_size number of bytes to be copied
 */
static void
copy (void *const dest,
      const void *const src,
      const size_t element_size)
{
  if (element_size == sizeof(uint64_t)) {
    uint64_t *i64_src = (uint64_t *) src;
    uint64_t *i64_dest = (uint64_t *) dest;
    *i64_dest = *i64_src;
    return;
  }
  memcpy(dest, src, element_size);
}

/**
 * @brief Swaps values pointed by `a` with `b`.
 *
 * @param [in,out] a            first value
 * @param [in,out] b            second value
 * @param [in]     element_size number of bytes occupied by a and b values
 */
static void
swap (void *const a,
      void *const b,
      const size_t element_size)
{
  if (element_size == sizeof(uint64_t)) {
    uint64_t *i64_a = (uint64_t *) a;
    uint64_t *i64_b = (uint64_t *) b;
    const uint64_t t = *i64_a;
    *i64_a = *i64_b;
    *i64_b = t;
    return;
  }
  size_t n = element_size;
  char *ca = (char *) a;
  char *cb = (char *) b;
  do {
    const char c = *ca;
    *ca++ = *cb;
    *cb++ = c;
  } while (--n > 0);
}

/**
 * @endcond
 */
