/**
 * @file
 *
 * @todo Refactor quick-sort.
 *       The swap macro is complicated .... and does not bring performance in.
 *
 * @todo Add tim-sort implementation to the list of available choices.
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
 * of exploding running time and memory consumption caused by unpredicted data distributions which might drive the
 * algorithm into the abyss, one procedure can be more suitable than the others.
 *
 * The algorithms here proposed are:
 *   - Insertion-sort
 *   - Binary-sort
 *   - Heap-sort
 *   - Smooth-sort
 *   - Quick-sort
 *   - Shell-sort
 *   - Merge-sort
 *   - Tim-sort
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
 * @details The vector `a`, collecting elements of size `element_size`, having length equal to `count`,
 *          is sorted in place applying the insertion-sort algorithm.
 *          The compare function `cmp` has to comply with the signature of #sort_utils_compare_function.
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
      if (j == 0 || cmp(ca + (j - 1) * element_size, ca + j * element_size) <= 0) break;
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



/***************/
/* Binary-sort */
/***************/

/**
 * @cond
 */

/**
 * @brief Sorts the specified portion of the `a` array using a binary
 * insertion sort.
 *
 * @details If the initial part of the array is already sorted,
 * this function assumes that the elements from the beginning,
 * to `start`, exclusive, are already sorted.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     start        the index of the first element out of sequence
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
static void
bnr_sort_from_ordered_initial_run  (void *const a,
                                    const size_t count,
                                    const size_t start,
                                    const size_t element_size,
                                    const sort_utils_compare_function cmp)
{
  g_assert(count >= 0);
  g_assert(start >= 0 && start <= count);
  if (count < 2) return;
  const size_t es = element_size;
  char *ca = (char *) a;
  for (size_t i = 1; i < start; i++) {
    g_assert(cmp(ca + i * es, ca + (i - 1) * es) >= 0);
  }
  char *tmp = malloc(element_size * sizeof(char));
  for (size_t i = start; i < count; i++) {
    copy(tmp, ca + i * es, es);
    size_t left = 0;
    size_t right = i;
    while (left < right) {
      size_t mid = (left + right) >> 1;
      if (cmp(tmp, ca + mid * element_size) < 0)
        right = mid;
      else
        left = mid + 1;
    }
    const size_t n = i - left;
    memmove(ca + (left + 1) * element_size, ca + left * element_size, n * element_size);
    copy(ca + left * element_size, tmp, element_size);
  }
  free(tmp);
}

/**
 * @endcond
 */

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a`, collecting elements of size `element_size`, having length equal to `count`,
 *          is sorted in place applying the binary-sort algorithm.
 *          The compare function `cmp` has to comply with the signature of #sort_utils_compare_function.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applied by the algorithm
 */
void
sort_utils_binarysort (void *const a,
                       const size_t count,
                       const size_t element_size,
                       const sort_utils_compare_function cmp)
{
  bnr_sort_from_ordered_initial_run(a, count, 0, element_size, cmp);
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the binary-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_binarysort_asc_d (double *const a,
                             const int count)
{
  sort_utils_binarysort(a, count, sizeof(double), sort_utils_double_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the binary-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_binarysort_dsc_d (double *const a,
                             const int count)
{
  sort_utils_binarysort(a, count, sizeof(double), sort_utils_double_icmp);
}

/**
 * @brief Sorts in ascending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the binary-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_binarysort_asc_i (int *const a,
                             const int count)
{
  sort_utils_binarysort(a, count, sizeof(int), sort_utils_int_cmp);
}

/**
 * @brief Sorts in descending order the `a` array of integers.
 *
 * @details The vector of integers `a` having length equal to `count` is sorted
 *          in place in descending order applying the binary-sort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_binarysort_dsc_i (int *const a,
                             const int count)
{
  sort_utils_binarysort(a, count, sizeof(int), sort_utils_int_icmp);
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
 * @cond
 */

static const size_t tms_min_merge = 32;
static const long long int tms_min_gallop = 7;

/**
 * @brief A team sort structure holds internal data for the algorithm.
 *
 * @details Variables `stack_size`, `run_base`, and `run_len` define a stack
 *          of pending runs yet to be merged.  Run i starts at
 *          address base[i] and extends for len[i] elements.  It's always
 *          true (so long as the indices are in bounds) that:
 *
 *             run_base[i] + run_len[i] == run_base[i + 1]
 *
 *          so we could cut the storage for this, but it's a minor amount,
 *          and keeping all the info explicit simplifies the code.
 */
typedef struct {
  void                       *a;            /**< @brief The array to be sorted. */
  size_t                      count;        /**< @brief The number of elements in array. */
  size_t                      element_size; /**< @brief The number of bytes used by one element. */
  sort_utils_compare_function cmp;          /**< @brief The compare function applied by the algorithm. */
  long long int               min_gallop;   /**< @brief This controls when we get *into* galloping mode. It is initialized to tms_min_gallop.
                                             *          The merge_lo and merge_hi methods nudge it higher for random data,
                                             *          and lower for highly structured data. */
  void                       *tmp;          /**< @brief Temp storage for merges. */
  size_t                      tmp_count;    /**< @brief The max number of elements contained by temp storage. */
  size_t                      stack_size;   /**< @brief Number of pending runs on stack. */
  size_t                     *run_base;     /**< @brief Index of first element for the ith run. */
  size_t                     *run_len;      /**< @brief Run Lenght. */
} TimSort;


/**
 * @brief Checks that from_index and to_index are in range.
 *
 * @param [in] array_len  the length of the array
 * @param [in] from_index the index of the first element of the range
 * @param [in] to_index   the index after the last element of the range
 */
static void
range_check (const size_t array_len,
             const size_t from_index,
             const size_t to_index)
{
  g_assert(to_index >= from_index);
  g_assert(from_index >= 0);
  g_assert(to_index <= array_len);
}


/**
 * Ensures that the external array tmp has at least the specified
 * number of elements, increasing its size if necessary.  The size
 * increases exponentially to ensure amortized linear time complexity.
 *
 * @param minCapacity the minimum required capacity of the tmp array
 * @return tmp, whether or not it grew
 */
static void *
ensure_capacity (TimSort *ts,
                 size_t min_capacity)
{
  if (ts->tmp_count < min_capacity) {
    // Compute smallest power of 2 > min_capacity
    size_t new_size = min_capacity;
    new_size |= new_size >> 1;
    new_size |= new_size >> 2;
    new_size |= new_size >> 4;
    new_size |= new_size >> 8;
    new_size |= new_size >> 16;
    new_size |= new_size >> 32;
    new_size++;

    size_t half_a_size = ts->count >> 1;
    new_size = (new_size < half_a_size) ? new_size : half_a_size;

    ts->tmp_count = new_size;
    ts->tmp = realloc(ts->tmp, ts->tmp_count * ts->element_size);
  }
  return ts->tmp;
}

/**
 * Locates the position at which to insert the specified key into the
 * specified sorted range; if the range contains an element equal to key,
 * returns the index of the leftmost equal element.
 *
 * @param key the key whose insertion point to search for
 * @param a the array in which to search
 * @param es element size
 * @param base the index of the first element in the range
 * @param len the length of the range; must be > 0
 * @param hint the index at which to begin the search, 0 <= hint < n.
 *     The closer hint is to the result, the faster this method will run.
 * @param c the comparator used to order the range, and to search
 * @return the int k,  0 <= k <= n such that a[b + k - 1] < key <= a[b + k],
 *    pretending that a[b - 1] is minus infinity and a[b + n] is infinity.
 *    In other words, key belongs at index b + k; or in other words,
 *    the first k elements of a should precede key, and the last n - k
 *    should follow it.
 */
static size_t
gallop_left (void *key,
             void *a,
             size_t es,
             size_t base,
             size_t len,
             size_t hint,
             sort_utils_compare_function cmp)
{
  g_assert(len > 0 && hint >= 0 && hint < len);
  char *ca = (char *) a;
  long long int last_ofs = 0;
  long long int  ofs = 1;
  if (cmp(key, ca + (base + hint) * es) > 0) {
    // Gallop right until a[base+hint+lastOfs] < key <= a[base+hint+ofs]
    long long int max_ofs = len - hint;
    while (ofs < max_ofs && cmp(key, ca + (base + hint + ofs) * es) > 0) {
      last_ofs = ofs;
      ofs = (ofs << 1) + 1;
      if (ofs <= 0)   // int overflow
        ofs = max_ofs;
    }
    if (ofs > max_ofs)
      ofs = max_ofs;

    // Make offsets relative to base
    last_ofs += hint;
    ofs += hint;
  } else { // key <= a[base + hint]
    // Gallop left until a[base+hint-ofs] < key <= a[base+hint-lastOfs]
    const long long  int max_ofs = hint + 1;
    while (ofs < max_ofs && cmp(key, ca + (base + hint - ofs) * es) <= 0) {
      last_ofs = ofs;
      ofs = (ofs << 1) + 1;
      if (ofs <= 0)   // int overflow
        ofs = max_ofs;
    }
    if (ofs > max_ofs)
      ofs = max_ofs;

    // Make offsets relative to base
    long long int tmp = last_ofs;
    last_ofs = hint - ofs;
    ofs = hint - tmp;
  }
  g_assert(last_ofs >= 0 && ofs >= 0);
  g_assert(-1 <= last_ofs && last_ofs < ofs && ofs <= len);

  /*
   * Now a[base+lastOfs] < key <= a[base+ofs], so key belongs somewhere
   * to the right of lastOfs but no farther right than ofs.  Do a binary
   * search, with invariant a[base + lastOfs - 1] < key <= a[base + ofs].
   */
  last_ofs++;
  while (last_ofs < ofs) {
    long long int m = last_ofs + ((ofs - last_ofs) >> 1);
    if (cmp(key, ca + (base + m) * es) > 0)
      last_ofs = m + 1;  // a[base + m] < key
    else
      ofs = m;           // key <= a[base + m]
  }
  g_assert(last_ofs == ofs); // so a[base + ofs - 1] < key <= a[base + ofs]
  return ofs;
}
/**
 * Like gallopLeft, except that if the range contains an element equal to
 * key, gallopRight returns the index after the rightmost equal element.
 *
 * @param key the key whose insertion point to search for
 * @param a the array in which to search
 * @param es element size
 * @param base the index of the first element in the range
 * @param len the length of the range; must be > 0
 * @param hint the index at which to begin the search, 0 <= hint < n.
 *     The closer hint is to the result, the faster this method will run.
 * @param c the comparator used to order the range, and to search
 * @return the int k,  0 <= k <= n such that a[b + k - 1] <= key < a[b + k]
 */
static size_t
gallop_right (void *key,
              void *a,
              size_t es,
              size_t base,
              size_t len,
              size_t hint,
              sort_utils_compare_function cmp)
{
  g_assert(len > 0 && hint >= 0 && hint < len);
  char *ca = (char *) a;

  long long int ofs = 1;
  long long int last_ofs = 0;
  if (cmp(key, ca + (base + hint) * es) < 0) {
    // Gallop left until a[b+hint - ofs] <= key < a[b+hint - lastOfs]
    int max_ofs = hint + 1;
    while (ofs < max_ofs && cmp(key, ca + (base + hint - ofs) * es) < 0) {
      last_ofs = ofs;
      ofs = (ofs << 1) + 1;
      if (ofs <= 0)   // int overflow
        ofs = max_ofs;
    }
    if (ofs > max_ofs)
      ofs = max_ofs;

    // Make offsets relative to b
    long long int tmp = last_ofs;
    last_ofs = hint - ofs;
    ofs = hint - tmp;
  } else { // a[b + hint] <= key
    // Gallop right until a[b+hint + lastOfs] <= key < a[b+hint + ofs]
    long long int max_ofs = len - hint;
    while (ofs < max_ofs && cmp(key, ca + (base + hint + ofs) * es) >= 0) {
    last_ofs = ofs;
      ofs = (ofs << 1) + 1;
      if (ofs <= 0)   // int overflow
        ofs = max_ofs;
    }
    if (ofs > max_ofs)
      ofs = max_ofs;

    // Make offsets relative to b
    last_ofs += hint;
    ofs += hint;
  }
  g_assert(-1 <= last_ofs && last_ofs < ofs && ofs <= len);

  /*
   * Now a[b + lastOfs] <= key < a[b + ofs], so key belongs somewhere to
   * the right of lastOfs but no farther right than ofs.  Do a binary
   * search, with invariant a[b + lastOfs - 1] <= key < a[b + ofs].
   */
  last_ofs++;
  while (last_ofs < ofs) {
    int m = last_ofs + ((ofs - last_ofs) >> 1);

    if (cmp(key, ca + (base + m) * es) < 0)
      ofs = m;          // key < a[b + m]
    else
      last_ofs = m + 1;  // a[b + m] <= key
  }
  g_assert(last_ofs == ofs);    // so a[b + ofs - 1] <= key < a[b + ofs]
  return ofs;
}

/**
 * Merges two adjacent runs in place, in a stable fashion.  The first
 * element of the first run must be greater than the first element of the
 * second run (a[base1] > a[base2]), and the last element of the first run
 * (a[base1 + len1-1]) must be greater than all elements of the second run.
 *
 * For performance, this method should be called only when len1 <= len2;
 * its twin, mergeHi should be called if len1 >= len2.  (Either method
 * may be called if len1 == len2.)
 *
 * @param base1 index of first element in first run to be merged
 * @param len1  length of first run to be merged (must be > 0)
 * @param base2 index of first element in second run to be merged
 *        (must be aBase + aLen)
 * @param len2  length of second run to be merged (must be > 0)
 */
static void
merge_lo (TimSort *ts,
          size_t base1,
          size_t len1,
          size_t base2,
          size_t len2)
{
  g_assert(len1 > 0 && len2 > 0 && base1 + len1 == base2);

  /* Copy first run into temp array. */
  char *ca = (char *) ts->a;
  char *tmp = (char *) ensure_capacity(ts, len1);
  const size_t es = ts->element_size;
  memcpy(ca + base1 * es, tmp, len1 * es);

  size_t cursor1 = 0;       // Indexes into tmp array
  size_t cursor2 = base2;   // Indexes into a array
  size_t dest = base1;      // Indexes into a array

  // Move first element of second run and deal with degenerate cases
  copy(ca + dest++ * es, ca + cursor2++ * es, es);
  if (--len2 == 0) {
    memcpy(tmp + cursor1 * es, ca + dest * es, len1 * es);
    return;
  }
  if (len1 == 1) {
    memcpy(ca + cursor2 * es, ca + dest * es, len2 * es);
    copy(ca + (dest + len2) * es, tmp + cursor1 * es, es); // Last elt of run 1 to end of merge
    return;
  }

  sort_utils_compare_function cmp = ts->cmp;
  long long int  min_gallop = ts->min_gallop;

 outer:
  while (TRUE) {
    size_t count1 = 0; // Number of times in a row that first run won
    size_t count2 = 0; // Number of times in a row that second run won

    /*
     * Do the straightforward thing until (if ever) one run starts
     * winning consistently.
     */
    do {
      g_assert(len1 > 1 && len2 > 0);
      if (cmp(ca + cursor2 * es, tmp + cursor1 * es) < 0) {
        copy(ca + dest++ * es, ca + cursor2++ * es, es);
        count2++;
        count1 = 0;
        if (--len2 == 0)
          goto outer;
      } else {
        copy(ca + dest++ * es, tmp + cursor1++ * es, es);
        count1++;
        count2 = 0;
        if (--len1 == 1)
          goto outer;
      }
    } while ((count1 | count2) < min_gallop);

    /*
     * One run is winning so consistently that galloping may be a
     * huge win. So try that, and continue galloping until (if ever)
     * neither run appears to be winning consistently anymore.
     */
    do {
      g_assert(len1 > 1 && len2 > 0);
      count1 = gallop_right(ca + cursor2 * es, tmp, es, cursor1, len1, 0, cmp);
      if (count1 != 0) {
        memcpy(tmp + cursor1 * es, ca + dest * es, count1 * es);
        dest += count1;
        cursor1 += count1;
        len1 -= count1;
        if (len1 <= 1) // len1 == 1 || len1 == 0
          goto outer;
      }
      copy(ca + dest++ * es, ca + cursor2++ * es, es);
      if (--len2 == 0)
        goto outer;

      count2 = gallop_left(tmp + cursor1 * es, ca, es, cursor2, len2, 0, cmp);
      if (count2 != 0) {
        memmove(ca + cursor2 * es, ca + dest * es, count2 * es);
        dest += count2;
        cursor2 += count2;
        len2 -= count2;
        if (len2 == 0)
          goto outer;
      }
      copy(ca + dest++ * es, tmp + cursor1++ * es, es);
      if (--len1 == 1)
        goto outer;
      min_gallop--;
    } while ((count1 >= tms_min_gallop) | (count2 >= tms_min_gallop));
    if (min_gallop < 0)
      min_gallop = 0;
    min_gallop += 2;  // Penalize for leaving gallop mode
  }  // End of "outer" loop
  ts->min_gallop = min_gallop < 1 ? 1 : min_gallop; // Write back to field

  if (len1 == 1) {
    g_assert(len2 > 0);
    memmove(ca + cursor2 * es, ca + dest * es, len2 * es);
    copy(ca + (dest + len2) * es, tmp + cursor1 * es, es); // Last elt of run 1 to end of merge
  } else if (len1 == 0) {
    printf("Comparison method violates its general contract!\n");
    g_assert(FALSE);
  } else {
    g_assert(len2 == 0);
    g_assert(len1 > 1);
    memcpy(tmp + cursor1 * es, ca + dest * es, len1 * es);
  }
}

/**
 * @brief Merges two adjacent runs in place, in a stable fashion.
 *
 * @details Like mergeLo, except that this method should be called only if
 *          len1 >= len2; mergeLo should be called if len1 <= len2.
 *          Either method may be called if len1 == len2.
 *
 * @param [in,out] ts    tim sort structure
 * @param [in]     base1 index of first element in first run to be merged
 * @param [in]     len1  length of first run to be merged (must be > 0)
 * @param [in]     base2 index of first element in second run to be merged (must be aBase + aLen)
 * @param [in]     len2  length of second run to be merged (must be > 0)
 */
static void
merge_hi (TimSort *ts,
          size_t base1,
          size_t len1,
          size_t base2,
          size_t len2)
{
  g_assert(len1 > 0 && len2 > 0 && base1 + len1 == base2);

  /* Copy first run into temp array. */
  char *ca = (char *) ts->a;
  char *tmp = (char *) ensure_capacity(ts, len2);
  const size_t es = ts->element_size;
  memcpy(ca + base2 * es, tmp, len2 * es);

  size_t cursor1 = base1 + len1 - 1;  // Indexes into a array
  size_t cursor2 = len2 -1;           // Indexes into tmp array
  size_t dest = base2 + len2 - 1;     // Indexes into a array

  // Move first element of second run and deal with degenerate cases
  copy(ca + dest-- * es, ca + cursor1-- * es, es);
  if (--len1 == 0) {
    memcpy(tmp, ca + (dest - (len2 - 1)) * es, len2 * es);
    return;
  }
  if (len2 == 1) {
    dest -= len1;
    cursor1 -= len1;
    memmove(ca + (cursor1 + 1) * es, ca + (dest + 1) * es, len1 * es);
    copy(ca + dest * es, tmp + cursor2 * es, es);
    return;
  }

  sort_utils_compare_function cmp = ts->cmp;
  long long int  min_gallop = ts->min_gallop;

 outer:
  while (TRUE) {
    size_t count1 = 0; // Number of times in a row that first run won
    size_t count2 = 0; // Number of times in a row that second run won

    /*
     * Do the straightforward thing until (if ever) one run
     * appears to win consistently.
     */
    do {
      g_assert(len1 > 0 && len2 > 1);
      if (cmp(tmp + cursor2 * es, ca + cursor1 * es) < 0) {
        copy(ca + dest-- * es, ca + cursor1-- * es, es);
        count1++;
        count2 = 0;
        if (--len1 == 0)
          goto outer;
      } else {
        copy(ca + dest-- * es, tmp + cursor2-- * es, es);
        count2++;
        count1 = 0;
        if (--len2 == 1)
          goto outer;
      }
    } while ((count1 | count2) < min_gallop);

    /*
     * One run is winning so consistently that galloping may be a
     * huge win. So try that, and continue galloping until (if ever)
     * neither run appears to be winning consistently anymore.
     */
    do {
      g_assert(len1 > 0 && len2 > 1);
      count1 = len1 - gallop_right(tmp + cursor2 * es, ca, es, base1, len1, len1 - 1, cmp);
      if (count1 != 0) {
        dest -= count1;
        cursor1 -= count1;
        len1 -= count1;
        memmove(ca + (cursor1 + 1) * es, ca + (dest + 1) * es, count1 * es);
        if (len1 == 0)
          goto outer;
      }
      copy(ca + dest-- * es, tmp + cursor2-- * es, es);
      if (--len2 == 1)
        goto outer;

      count2 = len2 - gallop_left(ca + cursor1 * es, tmp, es, 0, len2, len2 - 1, cmp);
      if (count2 != 0) {
        dest -= count2;
        cursor2 -= count2;
        len2 -= count2;
        memcpy(tmp + (cursor2 + 1) * es, ca + (dest + 1) * es, count2 * es);
        if (len2 <= 1) // len2 == 1 || len2 == 0
          goto outer;
      }
      copy(ca + dest-- * es, ca + cursor1-- * es, es);
      if (--len1 == 0)
        goto outer;
      min_gallop--;
    } while ((count1 >= tms_min_gallop) | (count2 >= tms_min_gallop));
    if (min_gallop < 0)
      min_gallop = 0;
    min_gallop += 2;  // Penalize for leaving gallop mode
  }  // End of "outer" loop
  ts->min_gallop = min_gallop < 1 ? 1 : min_gallop; // Write back to field

  if (len2 == 1) {
    g_assert(len1 > 0);
    dest -= len1;
    cursor1 -= len1;
    memmove(ca + (cursor1 + 1) * es, ca + (dest + 1) * es, len1 * es);
    copy(ca + dest * es, tmp + cursor2 * es, es); // Move first elt of run2 to front of merge
  } else if (len2 == 0) {
    printf("Comparison method violates its general contract!\n");
    g_assert(FALSE);
  } else {
    g_assert(len1 == 0);
    g_assert(len2 > 0);
    memcpy(tmp, ca + (dest - (len2 - 1)) * es, len2 * es);
  }
}

/**
 * Merges the two runs at stack indices i and i+1.  Run i must be
 * the penultimate or antepenultimate run on the stack.  In other words,
 * i must be equal to stackSize-2 or stackSize-3.
 *
 * @param [in,out] ts tim sort structure
 * @param [in]     i  stack index of the first of the two runs to merge
 */
static void
merge_at (TimSort *ts,
          size_t i)
{
  g_assert(ts->stack_size >= 2);
  g_assert(i >= 0);
  g_assert(i == ts->stack_size - 2 || i == ts->stack_size - 3);

  char *ca = (char *) ts-> a;
  const size_t es = ts->element_size;

  size_t base1 = ts->run_base[i];
  size_t len1 = ts->run_len[i];
  size_t base2 = ts->run_base[i + 1];
  size_t len2 = ts->run_len[i + 1];
  g_assert(len1 > 0 && len2 > 0);
  g_assert(base1 + len1 == base2);

  /*
   * Record the length of the combined runs; if i is the 3rd-last
   * run now, also slide over the last run (which isn't involved
   * in this merge).  The current run (i+1) goes away in any case.
   */
  ts->run_len[i] = len1 + len2;
  if (i == ts->stack_size - 3) {
    ts->run_base[i + 1] = ts->run_base[i + 2];
    ts->run_len[i + 1] = ts->run_len[i + 2];
  }
  ts->stack_size--;

  /*
   * Find where the first element of run2 goes in run1. Prior elements
   * in run1 can be ignored (because they're already in place).
   */
  size_t k = gallop_right(ca + base2 * es, ca, es, base1, len1, 0, ts->cmp);
  g_assert(k >= 0);
  base1 += k;
  len1 -= k;
  if (len1 == 0)
    return;

  /*
   * Find where the last element of run1 goes in run2. Subsequent elements
   * in run2 can be ignored (because they're already in place).
   */
  len2 = gallop_left(ca + (base1 + len1 - 1) * es, ca, es, base2, len2, len2 - 1, ts->cmp);
  g_assert(len2 >= 0);
  if (len2 == 0)
    return;

  /* Merge remaining runs, using tmp array with min(len1, len2) elements. */
  if (len1 <= len2)
    merge_lo(ts, base1, len1, base2, len2);
  else
    merge_hi(ts, base1, len1, base2, len2);
}

/**
 * Examines the stack of runs waiting to be merged and merges adjacent runs
 * until the stack invariants are reestablished:
 *
 *     1. runLen[i - 3] > runLen[i - 2] + runLen[i - 1]
 *     2. runLen[i - 2] > runLen[i - 1]
 *
 * This method is called each time a new run is pushed onto the stack,
 * so the invariants are guaranteed to hold for i < stackSize upon
 * entry to the method.
 *
 * @param [in,out] ts tim sort structure
 */
static void
merge_collapse (TimSort *ts)
{
  while (ts->stack_size > 1) {
    size_t n = ts->stack_size - 2;
    if (n > 0 && ts->run_len[n - 1] <= ts->run_len[n] + ts->run_len[n + 1]) {
      if (ts->run_len[n - 1] < ts->run_len[n + 1])
        n--;
      merge_at(ts, n);
    } else if (ts->run_len[n] <= ts->run_len[n + 1]) {
      merge_at(ts, n);
    } else {
      break; // Invariant is established
    }
  }
}

/**
 * Merges all runs on the stack until only one remains.  This method is
 * called once, to complete the sort.
 *
 * @param [in,out] ts tim sort structure
 */
static void
merge_force_collapse (TimSort *ts) {
  while (ts->stack_size > 1) {
    size_t n = ts->stack_size - 2;
    if (n > 0 && ts->run_len[n - 1] < ts->run_len[n + 1])
      n--;
    merge_at(ts, n);
  }
}

/**
 * @brief TimSort structure constructor.
 *
 * @param [in] a            the array to be sorted
 * @param [in] count        the number of element in array
 * @param [in] element_size the number of bytes used by one element
 * @param [in] cmp          the compare function applied by the algorithm
 * @return                  a pointer to a new tim sort structure
 */
static TimSort *
tim_sort_new (void *const a,
              const size_t count,
              const size_t element_size,
              const sort_utils_compare_function cmp)
{
  TimSort *ts;
  static const size_t size_of_ts = sizeof(TimSort);

  static const size_t initial_tmp_storage_length = 256;
  static const size_t stack_len = 85;

  ts = (TimSort *) malloc(size_of_ts);
  g_assert(ts);

  ts->a = a;
  ts->count = count;
  ts->element_size = element_size;
  ts->cmp = cmp;
  ts->min_gallop = tms_min_gallop;

  ts->tmp_count = count < 2 * initial_tmp_storage_length ? count >> 1 : initial_tmp_storage_length;
  ts->tmp = malloc(ts->tmp_count * element_size);

  ts->stack_size = 0;
  ts->run_base = malloc(stack_len * sizeof(size_t));
  ts->run_len = malloc(stack_len * sizeof(size_t));

  return ts;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #tim_sort_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] ts the pointer to be deallocated
 */
static void
tim_sort_free (TimSort *ts)
{
  free(ts->tmp);
  free(ts->run_base);
  free(ts->run_len);
  free(ts);
}


/**
 * Pushes the specified run onto the pending-run stack.
 *
 * @param ts       tim sort structure pointer
 * @param run_base index of the first element in the run
 * @param run_len  the number of elements in the run
 */
static void
push_run (TimSort *const ts,
          const size_t run_base,
          const size_t run_len) {
  ts->run_base[ts->stack_size] = run_base;
  ts->run_len[ts->stack_size] = run_len;
  ts->stack_size++;
}

/**
 * Returns the minimum acceptable run length for an array of the specified
 * length. Natural runs shorter than this will be extended with
 * {@link #binarySort}.
 *
 * Roughly speaking, the computation is:
 *
 *  If n < tms_min_merge, return n (it's too small to bother with fancy stuff).
 *  Else if n is an exact power of 2, return tms_min_merge/2.
 *  Else return an int k, tms_min_merge/2 <= k <= tms_min_merge, such that n/k
 *   is close to, but strictly less than, an exact power of 2.
 *
 * For the rationale, see listsort.txt.
 *
 * @param n the length of the array to be sorted
 * @return the length of the minimum run to be merged
 */
static size_t
min_run_length (size_t n) {
  g_assert(n >= 0);
  size_t r = 0;      // Becomes 1 if any 1 bits are shifted off
  while (n >= tms_min_merge) {
    r |= (n & 1);
    n >>= 1;
  }
  return n + r;
}

/**
 * @brief Reverse the specified range of the specified array.
 *
 * @param a            the array in which a range is to be reversed
 * @param element_size number of bytes used by one element
 * @param lo           the index of the first element in the range to be reversed
 * @param hi           the index after the last element in the range to be reversed
 */
static void
reverse_range (void *const a,
               const size_t element_size,
               size_t lo,
               size_t hi)
{
  char *ca = (char *) a;
  hi--;
  while (lo < hi) {
    swap(ca + lo * element_size, ca + hi * element_size, element_size);
    lo++; hi--;
  }
}

/**
 * Returns the length of the run beginning at the specified position in
 * the specified array and reverses the run if it is descending (ensuring
 * that the run will always be ascending when the method returns).
 *
 * A run is the longest ascending sequence with:
 *
 *    a[lo] <= a[lo + 1] <= a[lo + 2] <= ...
 *
 * or the longest descending sequence with:
 *
 *    a[lo] >  a[lo + 1] >  a[lo + 2] >  ...
 *
 * For its intended use in a stable mergesort, the strictness of the
 * definition of "descending" is needed so that the call can safely
 * reverse a descending sequence without violating stability.
 *
 * @param a the array in which a run is to be counted and possibly reversed
 * @param lo index of the first element in the run
 * @param hi index after the last element that may be contained in the run.
 *           It is required that @code{lo < hi}.
 * @param c the comparator to used for the sort
 * @return  the length of the run beginning at the specified position in
 *          the specified array
 */
static size_t
count_run_and_make_ascending (void *const a,
                              const size_t element_size,
                              size_t lo,
                              size_t hi,
                              const sort_utils_compare_function cmp)
{
  g_assert(lo < hi);
  size_t run_hi = lo + 1;
  if (run_hi == hi) return 1;

  char *ca = (char *) a;

  /* Finds end of run, and reverses range if descending. */
  if (cmp(ca + run_hi++ * element_size, ca + lo * element_size) < 0) { // Descending.
    while (run_hi < hi && cmp(ca + run_hi * element_size, ca + (run_hi -1) * element_size) < 0)
      run_hi++;
    reverse_range(a, element_size, lo, run_hi);
  } else { // Ascending.
    while (run_hi < hi && cmp(ca + run_hi * element_size, ca + (run_hi -1) * element_size) >= 0)
      run_hi++;
  }
  return run_hi - lo;
}

/**
 * @endcond
 */

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

  char *ca = (char *) a;

  size_t lo = 0;
  size_t hi = count;

  range_check(count, lo, hi);

  size_t n_remaining = hi - lo;
  if (n_remaining < 2) return;

  if (n_remaining < tms_min_merge) {
    size_t init_run_len = count_run_and_make_ascending(a, element_size, lo, hi, cmp);
    bnr_sort_from_ordered_initial_run(ca + lo * element_size, hi, init_run_len, element_size, cmp);
    return;
  }

  /*
   * March over the array once, left to right, finding natural runs,
   * extending short natural runs to min_run elements, and merging runs
   * to maintain stack invariant.
   */
  TimSort *ts = tim_sort_new(a, count, element_size, cmp);
  size_t min_run = min_run_length(n_remaining);
  do {
    /* Identify next run. */
    size_t run_len = count_run_and_make_ascending(a, element_size, lo, hi, cmp);

    // If run is short, extend to min(min_run, n_remaining)
    if (run_len < min_run) {
      size_t force = n_remaining <= min_run ? n_remaining : min_run;
      bnr_sort_from_ordered_initial_run(ca + lo * element_size, lo + force, lo + run_len, element_size, cmp);
      run_len = force;
    }

    // Push run onto pending-run stack, and maybe merge
    push_run(ts, lo, run_len);
    merge_collapse(ts);

    // Advance to find next run
    lo += run_len;
    n_remaining -= run_len;
  } while (n_remaining != 0);

  /* Merge all remaining runs to complete sort. */
  g_assert(lo == hi);
  merge_force_collapse(ts);
  g_assert(ts->stack_size == 1);

  tim_sort_free(ts);

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
