/**
 * @file
 *
 * @brief Sort Utils module implementation.
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

#include "sort_utils.h"

/**
 * @brief Returns true if `v1` is smaller than `v2` for all data types
 *        for which the `<` operator is defined.
 */
#define is_less(v1, v2) (v1 < v2)

/**
 * @brief Exchanges values among two double variables.
 */
#define swap_d(r,s)  do { double t = r; r = s; s = t; } while(0)

/**
 * @brief Exchanges values among two pointers.
 */
#define swap_p(r,s)  do { void *t = r; r = s; s = t; } while(0)



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
sift_down_d (double *const a,
             const int start,
             const int end);

static void
sift_down_p (void **const a,
             const int start,
             const int end);

/**
 * @endcond
 */



/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the heapsort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_d (double *const a,
                       const int count)
{
  for (int start = (count - 2) / 2; start >= 0; start--) {
    sift_down_d(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    swap_d(a[end], a[0]);
    sift_down_d(a, 0, end);
  }
}

/**
 * @brief Sorts in ascending order the `a` array of pointers.
 *
 * @details The vector of pointers `a` having length equal to `count` is sorted
 *          in place in ascending order applying the heapsort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_p (void **const a,
                       const int count)
{
  for (int start = (count - 2) / 2; start >= 0; start--) {
    sift_down_p(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    swap_p(a[end], a[0]);
    sift_down_p(a, 0, end);
  }
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Sift down function for double arrays.
 *
 * @details The sift-down function extends the heap property (heapify) the
 *          segment of array `a` starting from index equal to `start` and
 *          ending to index equal to `end`.
 *
 * @param [in,out] a     the array to sift
 * @param [in]     start index of the initial position
 * @param [in]     end   index of the final position
 */
static void
sift_down_d (double *const a,
             const int start,
             const int end)
{
  int root = start;
  while (root * 2 + 1 < end) {
    int child = 2 * root + 1;
    if ((child + 1 < end) && is_less(a[child], a[child + 1])) {
      child += 1;
    }
    if (is_less(a[root], a[child])) {
      swap_d(a[child], a[root]);
      root = child;
    }
    else
      return;
  }
}

/**
 * @brief Sift down function for array of pointers.
 */
static void
sift_down_p (void **const a,
             const int start,
             const int end)
{
  int root = start;
  while (root * 2 + 1 < end) {
    int child = 2 * root + 1;
    if ((child + 1 < end) && is_less(a[child], a[child + 1])) {
      child += 1;
    }
    if (is_less(a[root], a[child])) {
      swap_p(a[child], a[root]);
      root = child;
    }
    else
      return;
  }
}

/**
 * @endcond
 */
