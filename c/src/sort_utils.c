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

static int q, r, p, b, c, r1, b1, c1;
static SItem *A;

void
sift (void)
{
  int r0, r2;
  SItem T;
  r0 = r1;
  T = A[r0];
  while (b1 >= 3) {
    r2 = r1 - b1 + c1;
    if (! is_less_or_equal(A[r1 - 1], A[r2])) {
      r2 = r1 - 1;
      down(b1, c1);
    }
    if (is_less_or_equal(A[r2], T)) b1 = 1;
    else {
      A[r1] = A[r2];
      r1 = r2;
      down(b1, c1);
    }
  }
  if (r1 - r0) A[r1] = T;
}

void
trinkle (void)
{
  int p1, r2, r3, r0;
  SItem T;
  p1 = p; b1 = b; c1 = c;
  r0 = r1; T = A[r0];
  while (p1 > 0) {
    while ((p1 & 1)==0) {
      p1 >>= 1;
      up(b1, c1);
    }
    r3 = r1 - b1;
    if ((p1 == 1) || is_less_or_equal(A[r3], T)) p1 = 0;
    else {
      p1--;
      if (b1 == 1) {
        A[r1] = A[r3];
        r1 = r3;
      }
      else
        if (b1 >= 3) {
          r2 = r1 - b1 + c1;
          if (! is_less_or_equal(A[r1 - 1], A[r2])) {
            r2 = r1 - 1;
            down(b1, c1);
            p1 <<= 1;
          }
          if (is_less_or_equal(A[r2], A[r3])) {
            A[r1] = A[r3]; r1 = r3;
          }
          else {
            A[r1] = A[r2];
            r1 = r2;
            down(b1, c1);
            p1 = 0;
          }
        }
    }
  }
  if (r0 - r1) A[r1] = T;
  sift();
}

void
semitrinkle (void)
{
  SItem T;
  r1 = r - c;
  if (! is_less_or_equal(A[r1], A[r])) {
    T = A[r]; A[r] = A[r1]; A[r1] = T;
    trinkle();
  }
}


/**
 * Adapted from Delphi implementation of Dijkstra's algorithm.
 */
void
sort_utils_smoothsort (SItem Aarg[],
                       const int N)
{
  A=Aarg; /* 0-base array; warning: A is shared by other functions */
  q = 1; r = 0; p = 1; b = 1; c = 1;

  /* building tree */
  while (q < N) {
    r1 = r;
    if ((p & 7)==3) {
      b1 = b; c1 = c; sift();
      p = (p + 1) >> 2;
      up(b, c);
      up(b, c);
    }
    else if ((p & 3)==1) {
      if (q + c < N) {
        b1 = b; c1 = c; sift();
      }
      else trinkle();
      down(b, c);
      p <<= 1;
      while (b > 1) {
        down(b, c);
        p <<= 1;
      }
      p++;
    }
    q++; r++;
  }
  r1 = r; trinkle();

  /* building sorted array */
  while (q > 1) {
    q--;
    if (b == 1) {
      r--; p--;
      while ((p & 1) == 0) {
        p >>= 1;
        up(b, c);
      }
    }
    else
      if (b >= 3) {
        p--; r = r - b + c;
        if (p > 0) semitrinkle();
        down(b, c);
        p = (p << 1) + 1;
        r = r+c;  semitrinkle();
        down(b, c);
        p = (p << 1) + 1;
      }
    /* element q processed */
  }
  /* element 0 processed */
}
