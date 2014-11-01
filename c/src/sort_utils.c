/**
 * @file
 *
 * @todo Write tests for ordered and mostly ordered data.
 *
 * @todo Write sort_utils_smoothsorth_p.
 *
 * @todo Add quicksort to the list of available choices.
 *
 * @todo Write a function that receives the array of pointers to be sorted,
 *       the count, a function pointer for compare, a void pointer to data,
 *       an enum that identify the sorting algorithm.
 *
 * @todo Write tests for corner cases like zero, one, two elements to be sorted.
 *
 *
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
 * @cond
 */

/*
 * Internal struct and type definitions.
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



/*
 * Internal macros.
 */

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

/**
 * @brief Function up as described by the soothsort paper.
 */
#define ss_up(ia, ib) do { unsigned long long int temp = ia; ia += ib + 1; ib = temp; } while (0)

/**
 * @brief Function down as described by the soothsort paper.
 */
#define ss_down(ia, ib) do { unsigned long long int temp = ib; ib = ia - ib - 1; ia = temp; } while (0)



/*
 * Prototypes for internal functions.
 */

static void
hps_sift_down (void *const a,
               const int start,
               const int end,
               const size_t element_size,
               const sort_utils_compare_function cmp);

static void
hs_sift_down_d (double *const a,
                const int start,
                const int end);

static void
hs_sift_down_p (void **const a,
                const int start,
                const int end);

static void
ss_sift (SmoothsortSharedVariables shrd);

static void
ss_trinkle (SmoothsortSharedVariables shrd);

static void
ss_semitrinkle (SmoothsortSharedVariables shrd);


/**
 * @endcond
 */

int
double_cmp (const void *const a, const void *const b)
{
  const double *const x = a;
  const double *const y = b;
  return (*x > *y) - (*x < *y);
}

void
swap (void *const a,
      void *const b,
      const size_t element_size)
{
  size_t n = element_size;
  char *ca = (char*) a;
  char *cb = (char*) b;
  do {
    const char c = *ca;
    *ca++ = *cb;
    *cb++ = c;
  } while (--n > 0);
}

void
sort_utils_heapsort (void *const a,
                     const size_t count,
                     const size_t element_size,
                     const sort_utils_compare_function cmp)
{
  char *a_ptr = (char *) a;
  for (int start = (count - 2) / 2; start >= 0; start--) {
    hps_sift_down(a, start, count, element_size, cmp);
  }
  for (int end = count - 1; end > 0; end--) {
    swap(a_ptr + end * element_size, a_ptr, element_size);
    hps_sift_down(a, 0, end, element_size, cmp);
  }
}

void
sort_utils_heapsort_X_d (double *const a,
                         const int count)
{
  sort_utils_heapsort(a, count, sizeof(double), double_cmp);
}


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
    hs_sift_down_d(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    swap_d(a[end], a[0]);
    hs_sift_down_d(a, 0, end);
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
    hs_sift_down_p(a, start, count);
  }
  for (int end = count - 1; end > 0; end--) {
    swap_p(a[end], a[0]);
    hs_sift_down_p(a, 0, end);
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
      ss_sift(shrd);
      shrd.p = (shrd.p + 1) >> 2;
      ss_up(shrd.b, shrd.c);
      ss_up(shrd.b, shrd.c);
    } else if ((shrd.p & 3) == 1) {
      if (q + shrd.c < count) {
        shrd.b1 = shrd.b;
        shrd.c1 = shrd.c;
        ss_sift(shrd);
      } else {
        ss_trinkle(shrd);
      }
      ss_down(shrd.b, shrd.c);
      shrd.p <<= 1;
      while (shrd.b > 1) {
        ss_down(shrd.b, shrd.c);
        shrd.p <<= 1;
      }
      shrd.p++;
    }
    q++;
    shrd.r++;
  }
  shrd.r1 = shrd.r;
  ss_trinkle(shrd);

  /* building sorted array */
  while (q > 1) {
    q--;
    if (shrd.b == 1) {
      shrd.r--;
      shrd.p--;
      while ((shrd.p & 1) == 0) {
        shrd.p >>= 1;
        ss_up(shrd.b, shrd.c);
      }
    } else {
      if (shrd.b >= 3) {
        shrd.p--;
        shrd.r = shrd.r - shrd.b + shrd.c;
        if (shrd.p > 0) {
          ss_semitrinkle(shrd);
        }
        ss_down(shrd.b, shrd.c);
        shrd.p = (shrd.p << 1) + 1;
        shrd.r = shrd.r + shrd.c;
        ss_semitrinkle(shrd);
        ss_down(shrd.b, shrd.c);
        shrd.p = (shrd.p << 1) + 1;
      }
    }
    /* element q processed */
  }
  /* element 0 processed */
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static void
hps_sift_down (void *const a,
               const int start,
               const int end,
               const size_t element_size,
               const sort_utils_compare_function cmp)
{
  char *a_ptr = (char *) a;
  int root = start;
  while (root * 2 + 1 < end) {
    int child = 2 * root + 1;
    if ((child + 1 < end) && cmp(a_ptr + child * element_size, a_ptr + (child + 1) * element_size) < 0) {
      child += 1;
    }
    if (cmp(a_ptr + root * element_size, a_ptr + child * element_size) < 0) {
      swap(a_ptr + child * element_size, a_ptr + root * element_size, element_size);
      root = child;
    }
    else
      return;
  }
}


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
hs_sift_down_d (double *const a,
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
hs_sift_down_p (void **const a,
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
 * @brief Function sift as defined by the smoothsort paper.
 *
 * @brief When stretches thus parsed areviewed as postorder traversals of binarytrees,
 *        trustiness means that no son exceeds its father. A dubious stretch is made into
 *        a trusty one by applying the operation "sift" –a direct inheritance from heapsort– to its root,
 *        where sift is defined as follow: sift applied to an element m[r1] that is exceede by its
 *        largest son m[r2] consists of a swap of these two values, followed by an application of sift to m[r2].
 *
 * @param shrd shared variables used by the functions composing smoothsort
 */
void
ss_sift (SmoothsortSharedVariables shrd)
{
  double tmp;
  unsigned long long r0, r2;
  r0 = shrd.r1;
  tmp = shrd.a[r0];
  while (shrd.b1 >= 3) {
    r2 = shrd.r1 - shrd.b1 + shrd.c1;
    if (! is_less_or_equal(shrd.a[shrd.r1 - 1], shrd.a[r2])) {
      r2 = shrd.r1 - 1;
      ss_down(shrd.b1, shrd.c1);
    }
    if (is_less_or_equal(shrd.a[r2], tmp)) {
      shrd.b1 = 1;
    } else {
      shrd.a[shrd.r1] = shrd.a[r2];
      shrd.r1 = r2;
      ss_down(shrd.b1, shrd.c1);
    }
  }
  if (shrd.r1 - r0) {
    shrd.a[shrd.r1] = tmp;
  }
}

/**
 * @brief Function trinkle as defined by the smoothsort paper.
 *
 * @details In the case `p mod 4 = 1`, the standard concatenation ends on a dubious stretch of length b,
 *          which in this step becomes the last but one stretch of the standard concatenation and,
 *          hence, must be made trusty. In the case `q + c < N`, it suffices to apply sift to m[r] as before,
 *          since this stretch will later disappear from the standard concatenation. In the case `q + c >= N`,
 *          however, just applying sift to m[r] might violate P4' since this stretch of length b also occurs
 *          in the standard concatenation of length N. Making such a dubious stretch trusty and including its
 *          root in the sequence of ascending roots is achieved by applying "trinkle“ to m[r].
 *
 * @param shrd shared variables used by the functions composing smoothsort
 */
void
ss_trinkle (SmoothsortSharedVariables shrd)
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
      ss_up(shrd.b1, shrd.c1);
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
            ss_down(shrd.b1, shrd.c1);
            p1 <<= 1;
          }
          if (is_less_or_equal(shrd.a[r2], shrd.a[r3])) {
            shrd.a[shrd.r1] = shrd.a[r3];
            shrd.r1 = r3;
          } else {
            shrd.a[shrd.r1] = shrd.a[r2];
            shrd.r1 = r2;
            ss_down(shrd.b1, shrd.c1);
            p1 = 0;
          }
        }
      }
    }
  }
  if (r0 - shrd.r1) {
    shrd.a[shrd.r1] = tmp;
  }
  ss_sift(shrd);
}

/**
 * @brief Function semitrinkle as defined by the smoothsort paper.
 *
 * @details In the case `b >= 3`, the rightmost stretch of length b is replaced by two trusty ones; hence P3 is maintained.
 *          To restore P4 it would suffice to apply trinkle first to the root of the first new stretch and then to the
 *          root of the second new stretch, but this would fail to exploit the fact that the new stretches are already
 *          trusty to start with. This is exploited by applying "semitrinkle“ in order to those roots.
 *
 * @param shrd shared variables used by the functions composing smoothsort
 */
void
ss_semitrinkle (SmoothsortSharedVariables shrd)
{
  double tmp;
  shrd.r1 = shrd.r - shrd.c;
  if (!is_less_or_equal(shrd.a[shrd.r1], shrd.a[shrd.r])) {
    tmp = shrd.a[shrd.r];
    shrd.a[shrd.r] = shrd.a[shrd.r1];
    shrd.a[shrd.r1] = tmp;
    ss_trinkle(shrd);
  }
}

/**
 * @endcond
 */

void
sort_utils_insertionsort_d (double *const a,
                            const int n)
{
  int i, j;
  for (i = 1; i < n; i++) {
    for (j = i; j > 0 && a[j - 1] > a[j]; j--) {
      swap_d(a[j], a[j - 1]);
    }
  }
}





static void
qs_swap (void *x,
         void *y,
         size_t l)
{
  char *a = x, *b = y, c;
  while(l--) {
    c = *a;
    *a++ = *b;
    *b++ = c;
  }
}

static void
qs_sort (char *array,
         size_t size,
         int (*cmp)(void*, void*),
         int begin,
         int end)
{
  if (end > begin) {
    void *pivot = array + begin;
    int l = begin + size;
    int r = end;
    while(l < r) {
      if (cmp(array+l,pivot) <= 0) {
        l += size;
      } else if ( cmp(array+r, pivot) > 0 )  {
        r -= size;
      } else if ( l < r ) {
        qs_swap(array+l, array+r, size);
      }
    }
    l -= size;
    qs_swap(array+begin, array+l, size);
    qs_sort(array, size, cmp, begin, l);
    qs_sort(array, size, cmp, r, end);
  }
}

/**
 * @brief Sorts the `a` array according to the `cmp` function.
 *
 * @details See: http://en.wikibooks.org/wiki/Algorithm_Implementation/Sorting/Quicksort#C
 */
void
sort_utils_quicksort (void *a,
                      size_t nitems,
                      size_t size,
                      int (*cmp)(void*, void*))
{
  qs_sort(a, size, cmp, 0, (nitems - 1) * size);
}
