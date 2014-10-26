/**
 * @file
 *
 * @todo Naming has to be reorganized with helper function having prefixes like ss_ and hs_
 *
 * @todo Static variables for smoothsort has to be removed.
 *
 * @todo Smoothsort unction has to be ported to double and pointer ....
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
#include <string.h>

#include "sort_utils.h"

/**
 * @brief Returns true if `v1` is smaller than `v2` for all data types
 *        for which the `<` operator is defined.
 */
#define is_less(v1, v2) (v1 < v2)

/**
 * @brief Returns true if `v1` is smaller than or equal to `v2` for all
 *        data types for which the `<` operator is defined.
 */
#define is_less_or_equal(v1, v2) (v1 <= v2)

/**
 * @brief Exchanges values among two double variables.
 */
#define swap_d(r,s) do { double t = r; r = s; s = t; } while(0)

/**
 * @brief Exchanges values among two pointers.
 */
#define swap_p(r,s) do { void *t = r; r = s; s = t; } while(0)

#define up(ia, ib) do { int temp = ia; ia += ib + 1; ib = temp; } while (0)
#define down(ia, ib) do { int temp = ib; ib = ia - ib - 1; ia = temp; } while (0)



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


/**
 * @brief Parameters used and shared by the smoothsort algorithm.
 */
typedef struct {
  double                  *a;
  unsigned long long int   r;
  unsigned long long int  r1;
  unsigned long long int   p;
  unsigned long long int   b;
  unsigned long long int   c;
  unsigned long long int  b1;
  unsigned long long int  c1;
} SmoothsortSharedVariables;

void
smoothsort_sift (SmoothsortSharedVariables shrd)
{
  double tmp;
  unsigned long long r0, r2;
  r0 = shrd.r1;
  tmp = shrd.a[r0];
  while (shrd.b1 >= 3) {
    r2 = shrd.r1 - shrd.b1 + shrd.c1;
    if (! is_less_or_equal(shrd.a[shrd.r1 - 1], shrd.a[r2])) {
      r2 = shrd.r1 - 1;
      down(shrd.b1, shrd.c1);
    }
    if (is_less_or_equal(shrd.a[r2], tmp)) {
      shrd.b1 = 1;
    } else {
      shrd.a[shrd.r1] = shrd.a[r2];
      shrd.r1 = r2;
      down(shrd.b1, shrd.c1);
    }
  }
  if (shrd.r1 - r0) {
    shrd.a[shrd.r1] = tmp;
  }
}

void
smoothsort_trinkle (SmoothsortSharedVariables shrd)
{
  double tmp;
  unsigned long long r0, r2, r3, p1;
  p1 = shrd.p;
  shrd.b1 = shrd.b;
  shrd.c1 = shrd.c;
  r0 = shrd.r1;
  tmp = shrd.a[r0];
  while (p1 > 0) {
    while ((p1 & 1) == 0) {
      p1 >>= 1;
      up(shrd.b1, shrd.c1);
    }
    r3 = shrd.r1 - shrd.b1;
    if ((p1 == 1) || is_less_or_equal(shrd.a[r3], tmp)) {
      p1 = 0;
    } else {
      p1--;
      if (shrd.b1 == 1) {
        shrd.a[shrd.r1] = shrd.a[r3];
        shrd.r1 = r3;
      } else {
        if (shrd.b1 >= 3) {
          r2 = shrd.r1 - shrd.b1 + shrd.c1;
          if (! is_less_or_equal(shrd.a[shrd.r1 - 1], shrd.a[r2])) {
            r2 = shrd.r1 - 1;
            down(shrd.b1, shrd.c1);
            p1 <<= 1;
          }
          if (is_less_or_equal(shrd.a[r2], shrd.a[r3])) {
            shrd.a[shrd.r1] = shrd.a[r3];
            shrd.r1 = r3;
          } else {
            shrd.a[shrd.r1] = shrd.a[r2];
            shrd.r1 = r2;
            down(shrd.b1, shrd.c1);
            p1 = 0;
          }
        }
      }
    }
  }
  if (r0 - shrd.r1) {
    shrd.a[shrd.r1] = tmp;
  }
  smoothsort_sift(shrd);
}

void
smoothsort_semitrinkle (SmoothsortSharedVariables shrd)
{
  double tmp;
  shrd.r1 = shrd.r - shrd.c;
  if (!is_less_or_equal(shrd.a[shrd.r1], shrd.a[shrd.r])) {
    tmp = shrd.a[shrd.r];
    shrd.a[shrd.r] = shrd.a[shrd.r1];
    shrd.a[shrd.r1] = tmp;
    smoothsort_trinkle(shrd);
  }
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the smoothsort algorithm.
 *          Adapted from Dijkstra's paper: http://www.enterag.ch/hartwig/order/smoothsort.pdf
 *          See also: http://en.wikipedia.org/wiki/Smoothsort
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_smoothsort_d (double *const a,
                         const int count)
{
  SmoothsortSharedVariables shrd;
  shrd.a = a;
  shrd.r = 0;
  shrd.c = 1;
  shrd.p = 1;
  shrd.b = 1;

  unsigned long long int q = 1;

  /* building tree */
  while (q < count) {
    shrd.r1 = shrd.r;
    if ((shrd.p & 7) == 3) {
      shrd.b1 = shrd.b;
      shrd.c1 = shrd.c;
      smoothsort_sift(shrd);
      shrd.p = (shrd.p + 1) >> 2;
      up(shrd.b, shrd.c);
      up(shrd.b, shrd.c);
    } else if ((shrd.p & 3) == 1) {
      if (q + shrd.c < count) {
        shrd.b1 = shrd.b;
        shrd.c1 = shrd.c;
        smoothsort_sift(shrd);
      } else {
        smoothsort_trinkle(shrd);
      }
      down(shrd.b, shrd.c);
      shrd.p <<= 1;
      while (shrd.b > 1) {
        down(shrd.b, shrd.c);
        shrd.p <<= 1;
      }
      shrd.p++;
    }
    q++;
    shrd.r++;
  }
  shrd.r1 = shrd.r;
  smoothsort_trinkle(shrd);

  /* building sorted array */
  while (q > 1) {
    q--;
    if (shrd.b == 1) {
      shrd.r--;
      shrd.p--;
      while ((shrd.p & 1) == 0) {
        shrd.p >>= 1;
        up(shrd.b, shrd.c);
      }
    } else {
      if (shrd.b >= 3) {
        shrd.p--;
        shrd.r = shrd.r - shrd.b + shrd.c;
        if (shrd.p > 0) {
          smoothsort_semitrinkle(shrd);
        }
        down(shrd.b, shrd.c);
        shrd.p = (shrd.p << 1) + 1;
        shrd.r = shrd.r + shrd.c;
        smoothsort_semitrinkle(shrd);
        down(shrd.b, shrd.c);
        shrd.p = (shrd.p << 1) + 1;
      }
    }
    /* element q processed */
  }
  /* element 0 processed */
}
