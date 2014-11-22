/**
 * @file
 *
 * @todo Write tests for ordered and mostly ordered data.
 *
 * @todo Write the generic sort_utils_smoothsorth.
 *
 * @todo Add one shellsort implementation to the list of available choices.
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
#include <stdint.h>
#include <string.h>

#include <glib.h>

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
  void                        *a;        /**< @brief The array that is going to be sorted. */
  unsigned long long int       r;        /**< @brief . */
  unsigned long long int       r1;       /**< @brief . */
  unsigned long long int       p;        /**< @brief . */
  unsigned long long int       b;        /**< @brief . */
  unsigned long long int       c;        /**< @brief . */
  unsigned long long int       b1;       /**< @brief . */
  unsigned long long int       c1;       /**< @brief . */
  size_t                       es;       /**< @brief . */
  sort_utils_compare_function  cmp;      /**< @brief . */
  char                        *tmp;      /**< @brief . */
} SmoothsortSharedVariables;

/**
 * @brief Parameters used and shared by the smoothsort algorithm.
 */
typedef struct {
  double                      *a;        /**< @brief The array that is going to be sorted. */
  unsigned long long int       r;        /**< @brief . */
  unsigned long long int       r1;       /**< @brief . */
  unsigned long long int       p;        /**< @brief . */
  unsigned long long int       b;        /**< @brief . */
  unsigned long long int       c;        /**< @brief . */
  unsigned long long int       b1;       /**< @brief . */
  unsigned long long int       c1;       /**< @brief . */
  size_t                       es;       /**< @brief . */
  sort_utils_compare_function  cmp;      /**< @brief . */
  char                        *tmp;      /**< @brief . */
} SmoothsortSharedVariablesX;



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

/**
 * @brief Function up as described by the soothsort paper.
 */
#define sms_up(ia, ib) do { unsigned long long int temp = ia; ia += ib + 1; ib = temp; } while (0)

/**
 * @brief Function down as described by the soothsort paper.
 */
#define sms_down(ia, ib) do { unsigned long long int temp = ib; ib = ia - ib - 1; ia = temp; } while (0)



/*
 * Prototypes for internal functions.
 */


static void
copy (void *const dest,
      void *const src,
      const size_t element_size);

static void
swap (void *const a,
      void *const b,
      const size_t element_size);

static void
hps_sift_down (void *const a,
               const int start,
               const int end,
               const size_t element_size,
               const sort_utils_compare_function cmp);

static void
ss_sift (SmoothsortSharedVariablesX shrd);

static void
ss_trinkle (SmoothsortSharedVariablesX shrd);

static void
ss_semitrinkle (SmoothsortSharedVariablesX shrd);

static void
sms_sift (SmoothsortSharedVariables *s);

static void
sms_trinkle (SmoothsortSharedVariables *s);

static void
sms_semitrinkle (SmoothsortSharedVariables *s);

/**
 * @endcond
 */



/*************************************/
/* Compare function implementations. */
/*************************************/

/* double */

/**
 * @brief Returns true when `a` is equal to `b`.
 *
 * @details Compare funtion that returns `TRUE`, so a value
 *          different from zero, when the double value pointed
 *          by `a` is equal to the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first double
 * @param b a ponter to the second double
 * @return  `TRUE` when `a` is equal `b`.
 */
int
sort_utils_double_eq (const void *const a,
                      const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return *x == *y;
}

/**
 * @brief Returns true when `a` is greater than `b`.
 *
 * @details Compare funtion that returns `TRUE`, so a value
 *          different from zero, when the double value pointed
 *          by `a` is greater than the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first double
 * @param b a ponter to the second double
 * @return  `TRUE` when `a` is greater than `b`.
 */
int
sort_utils_double_gt (const void *const a,
                      const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return *x > *y;
}

/**
 * @brief Returns true when `a` is greater than or equal to `b`.
 *
 * @details Compare funtion that returns `TRUE`, so a value
 *          different from zero, when the double value pointed
 *          by `a` is greater than or equal to the one pointed by `b`,
 *          otherwise it returns zero (`FALSE`).
 *
 * @param a a pointer to the first double
 * @param b a ponter to the second double
 * @return  `TRUE` when `a` is greater than or equal to `b`.
 */
int
sort_utils_double_ge (const void *const a,
                      const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return *x >= *y;
}

/**
 * @brief Returns true when `a` is less than `b`.
 *
 * @details Compare funtion that returns `TRUE`, so a value
 *          different from zero, when the double value pointed
 *          by `a` is less than the one pointed by `b`, otherwise
 *          it returns zero (`FALSE`).
 *
 * @param a a pointer to the first double
 * @param b a ponter to the second double
 * @return  `TRUE` when `a` is less than `b`.
 */
int
sort_utils_double_lt (const void *const a,
                      const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return *x < *y;
}

/**
 * @brief Returns true when `a` is less than or equal to `b`.
 *
 * @details Compare funtion that returns `TRUE`, so a value
 *          different from zero, when the double value pointed
 *          by `a` is less than or equal to the one pointed by `b`,
 *          otherwise it returns zero (`FALSE`).
 *
 * @param a a pointer to the first double
 * @param b a ponter to the second double
 * @return  `TRUE` when `a` is less than or equal to `b`.
 */
int
sort_utils_double_le (const void *const a,
                      const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return *x <= *y;
}

/**
 * @brief Compares double values pointed by `a` and `b`.
 *
 * @details Compare funtion that returns:
 *          - `+1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `-1` when `a` is less then `b`
 *
 * @param a a pointer to the first double
 * @param b a ponter to the second double
 * @return  a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
sort_utils_double_cmp (const void *const a, const void *const b)
{
  const double *const x = (const double *const) a;
  const double *const y = (const double *const) b;
  return (*x > *y) - (*x < *y);
}



/********************************/
/* Sort function implementatons */
/********************************/

/******************/
/* Insertion-sort */
/******************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the insertionsort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 *          Insertion-sort is a naive algorithm with asimptotic time complexity of O(n^2),
 *          so quicksort or heapsort should be preferred even for small arrays.
 *          Nevertheless sometimes its semplicity makes it a valid choice. The given
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
 * @param [in]     cmp          the compare function applyed by the algorithm
 */
void
sort_utils_insertionsort (void *const a,
                          const size_t count,
                          const size_t element_size,
                          const sort_utils_compare_function cmp)
{
  char *a_ptr = (char *) a;
  for (int i = 1; i < count; i++) {
    int j = i;
    for (;;) {
      if (j == 0 || cmp(a_ptr + (j - 1) * element_size, a_ptr + j * element_size)) break;
      swap(a_ptr + j * element_size, a_ptr + (j - 1) * element_size, element_size);
      j--;
    }
  }
}

/**
 * @brief Sorts in ascending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in ascending order applying the insertionsort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_insertionsort_asc_d (double *const a,
                                const int count)
{
  sort_utils_insertionsort(a, count, sizeof(double), sort_utils_double_lt);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the insertionsort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_insertionsort_dsc_d (double *const a,
                                const int count)
{
  sort_utils_insertionsort(a, count, sizeof(double), sort_utils_double_gt);
}



/*************/
/* Heap-sort */
/*************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the heapsort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applyed by the algorithm
 */
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
sort_utils_heapsort_asc_d (double *const a,
                           const int count)
{
  sort_utils_heapsort(a, count, sizeof(double), sort_utils_double_lt);
}

/**
 * @brief Sorts in descending order the `a` array of doubles.
 *
 * @details The vector of doubles `a` having length equal to `count` is sorted
 *          in place in descending order applying the heapsort algorithm.
 *
 * @param [in,out] a     the array to be sorted
 * @param [in]     count the number of element of array a
 */
void
sort_utils_heapsort_dsc_d (double *const a,
                           const int count)
{
  sort_utils_heapsort(a, count, sizeof(double), sort_utils_double_gt);
}



/***************/
/* Smooth-sort */
/***************/

/**
 * @brief Sorts the `a` array.
 *
 * @details The vector `a` having length equal to `count` is sorted
 *          in place applying the smoothsort algorithm.
 *          The compare function is a predicate and must return `TRUE` or `FALSE`.
 *
 *          Adapted from Dijkstra's paper: http://www.enterag.ch/hartwig/order/smoothsort.pdf
 *          See also: http://en.wikipedia.org/wiki/Smoothsort
 *
 * @param [in,out] a            the array to be sorted
 * @param [in]     count        the number of element in array
 * @param [in]     element_size the number of bytes used by one element
 * @param [in]     cmp          the compare function applyed by the algorithm
 */
void
sort_utils_smoothsort (void *const a,
                       const size_t count,
                       const size_t element_size,
                       const sort_utils_compare_function cmp)
{
  static const size_t size_of_char = sizeof(char);
  SmoothsortSharedVariables shrd;
  shrd.a = a;
  shrd.r = 0;
  shrd.c = 1;
  shrd.p = 1;
  shrd.b = 1;
  shrd.es = element_size;
  shrd.cmp = cmp;
  shrd.tmp = malloc(element_size * size_of_char);

  unsigned long long int q = 1;

  /* building tree */
  while (q < count) {
    shrd.r1 = shrd.r;
    if ((shrd.p & 7) == 3) {
      shrd.b1 = shrd.b;
      shrd.c1 = shrd.c;
      sms_sift(&shrd);
      shrd.p = (shrd.p + 1) >> 2;
      sms_up(shrd.b, shrd.c);
      sms_up(shrd.b, shrd.c);
    } else if ((shrd.p & 3) == 1) {
      if (q + shrd.c < count) {
        shrd.b1 = shrd.b;
        shrd.c1 = shrd.c;
        sms_sift(&shrd);
      } else {
        sms_trinkle(&shrd);
      }
      sms_down(shrd.b, shrd.c);
      shrd.p <<= 1;
      while (shrd.b > 1) {
        sms_down(shrd.b, shrd.c);
        shrd.p <<= 1;
      }
      shrd.p++;
    }
    q++;
    shrd.r++;
  }
  shrd.r1 = shrd.r;
  sms_trinkle(&shrd);

  /* building sorted array */
  while (q > 1) {
    q--;
    if (shrd.b == 1) {
      shrd.r--;
      shrd.p--;
      while ((shrd.p & 1) == 0) {
        shrd.p >>= 1;
        sms_up(shrd.b, shrd.c);
      }
    } else {
      if (shrd.b >= 3) {
        shrd.p--;
        shrd.r = shrd.r - shrd.b + shrd.c;
        if (shrd.p > 0) {
          sms_semitrinkle(&shrd);
        }
        sms_down(shrd.b, shrd.c);
        shrd.p = (shrd.p << 1) + 1;
        shrd.r = shrd.r + shrd.c;
        sms_semitrinkle(&shrd);
        sms_down(shrd.b, shrd.c);
        shrd.p = (shrd.p << 1) + 1;
      }
    }
    /* element q processed */
  }
  /* element 0 processed */

  free(shrd.tmp);
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
  SmoothsortSharedVariablesX s;
  s.a = a;
  s.r = 0;
  s.c = 1;
  s.p = 1;
  s.b = 1;

  unsigned long long int q = 1;

  /* building tree */
  while (q < count) {
    s.r1 = s.r;
    if ((s.p & 7) == 3) {
      s.b1 = s.b;
      s.c1 = s.c;
      ss_sift(s);
      s.p = (s.p + 1) >> 2;
      ss_up(s.b, s.c);
      ss_up(s.b, s.c);
    } else if ((s.p & 3) == 1) {
      if (q + s.c < count) {
        s.b1 = s.b;
        s.c1 = s.c;
        ss_sift(s);
      } else {
        ss_trinkle(s);
      }
      ss_down(s.b, s.c);
      s.p <<= 1;
      while (s.b > 1) {
        ss_down(s.b, s.c);
        s.p <<= 1;
      }
      s.p++;
    }
    q++;
    s.r++;
  }
  s.r1 = s.r;
  ss_trinkle(s);

  /* building sorted array */
  while (q > 1) {
    q--;
    if (s.b == 1) {
      s.r--;
      s.p--;
      while ((s.p & 1) == 0) {
        s.p >>= 1;
        ss_up(s.b, s.c);
      }
    } else {
      if (s.b >= 3) {
        s.p--;
        s.r = s.r - s.b + s.c;
        if (s.p > 0) {
          ss_semitrinkle(s);
        }
        ss_down(s.b, s.c);
        s.p = (s.p << 1) + 1;
        s.r = s.r + s.c;
        ss_semitrinkle(s);
        ss_down(s.b, s.c);
        s.p = (s.p << 1) + 1;
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

/**
 * @brief Copies values from `src` to `dest` pointers.
 *
 * @details It is an enhanced version of memcpy function,
 *          that leverages the dimension of the data copied
 *          when it fits into a standard register size.
 *
 * @param dest         a pointer to the first byte to be overwritten
 * @param src          a pointer to the first byte of data to be copied
 * @param element_size number of bytes to be copied
 */
static void
copy (void *const dest,
      void *const src,
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
 * @param [in]     element_size number of bytes occupaid by a and b values
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
 * @brief Sift down function used by the heapsort algorithm.
 */
static void
hps_sift_down (void *const a,
               const int start,
               const int end,
               const size_t element_size,
               const sort_utils_compare_function cmp)
{
  char *const a_ptr = (char *const) a;
  int root = start;
  for (int child = 2 * root + 1; child < end; child = 2 * root + 1) {
    char *child_ptr = (char *) a_ptr + child * element_size;
    char *const root_ptr = (char *const) a_ptr + root * element_size;
    if ((child < end - 1) && cmp(child_ptr, child_ptr + element_size)) {
      child += 1;
      child_ptr += element_size;
    }
    if (cmp(root_ptr, child_ptr)) {
      swap(child_ptr, root_ptr, element_size);
      root = child;
    }
    else
      return;
  }
}

/**
 * @brief Function sift as defined by the smoothsort paper.
 *
 * @details When stretches thus parsed areviewed as postorder traversals of binarytrees,
 *          trustiness means that no son exceeds its father. A dubious stretch is made into
 *          a trusty one by applying the operation "sift" –a direct inheritance from heapsort– to its root,
 *          where sift is defined as follow: sift applied to an element m[r1] that is exceede by its
 *          largest son m[r2] consists of a swap of these two values, followed by an application of sift to m[r2].
 *
 * @param s shared variables used by the functions composing smoothsort
 */
void
sms_sift (SmoothsortSharedVariables *s)
{
  const size_t es = s->es;
  char *a = (char *) s->a;
  unsigned long long r0, r2;
  r0 = s->r1;
  copy(s->tmp, a + r0 * es, es);
  while (s->b1 >= 3) {
    r2 = s->r1 - s->b1 + s->c1;
    if (!s->cmp(a + (s->r1 - 1) * es, a + r2 * es)) {
      r2 = s->r1 - 1;
      sms_down(s->b1, s->c1);
    }
    if (s->cmp(a + r2 * es, s->tmp)) {
      s->b1 = 1;
    } else {
      copy(a + s->r1 * es, a + r2 * es, es);
      s->r1 = r2;
      sms_down(s->b1, s->c1);
    }
  }
  if (s->r1 - r0) {
    copy(a + s->r1 * es, s->tmp, es);
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
 * @param s shared variables used by the functions composing smoothsort
 */
void
sms_trinkle (SmoothsortSharedVariables *s)
{
  const size_t es = s->es;
  char *a = (char *) s->a;
  unsigned long long r0, r2, r3, p1;
  p1 = s->p;
  s->b1 = s->b;
  s->c1 = s->c;
  r0 = s->r1;
  copy(s->tmp, a + r0 * es, es);
  while (p1 > 0) {
    while ((p1 & 1) == 0) {
      p1 >>= 1;
      sms_up(s->b1, s->c1);
    }
    r3 = s->r1 - s->b1;
    if ((p1 == 1) || s->cmp(a + r3 * es, s->tmp)) {
      p1 = 0;
    } else {
      p1--;
      if (s->b1 == 1) {
        copy(a + s->r1 * es, a + r3 * es, es);
        s->r1 = r3;
      } else {
        if (s->b1 >= 3) {
          r2 = s->r1 - s->b1 + s->c1;
          if (!s->cmp(a + (s->r1 - 1) * es, a + r2 * es)) {
            r2 = s->r1 - 1;
            sms_down(s->b1, s->c1);
            p1 <<= 1;
          }
          if (s->cmp(a + r2 * es, a + r3 * es)) {
            copy(a + s->r1 * es, a + r3 * es, es);
            s->r1 = r3;
          } else {
            copy(a + s->r1 * es, a + r2 * es, es);
            s->r1 = r2;
            sms_down(s->b1, s->c1);
            p1 = 0;
          }
        }
      }
    }
  }
  if (r0 - s->r1) {
    copy(a + s->r1 * es, s->tmp, es);
  }
  sms_sift(s);
}

/**
 * @brief Function semitrinkle as defined by the smoothsort paper.
 *
 * @details In the case `b >= 3`, the rightmost stretch of length b is replaced by two trusty ones; hence P3 is maintained.
 *          To restore P4 it would suffice to apply trinkle first to the root of the first new stretch and then to the
 *          root of the second new stretch, but this would fail to exploit the fact that the new stretches are already
 *          trusty to start with. This is exploited by applying "semitrinkle“ in order to those roots.
 *
 * @param s shared variables used by the functions composing smoothsort
 */
void
sms_semitrinkle (SmoothsortSharedVariables *s)
{
  const size_t es = s->es;
  char *a = (char *) s->a;
  s->r1 = s->r - s->c;
  if (!s->cmp(a + s->r1 * es, a + s->r * es)) {
    swap(a + s->r * es, a + s->r1 * es, es);
    sms_trinkle(s);
  }
}

/*---------------------------------------------------------------------*/

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
ss_sift (SmoothsortSharedVariablesX shrd)
{
  double tmp;
  unsigned long long r0, r2;
  r0 = shrd.r1;
  tmp = shrd.a[r0];
  while (shrd.b1 >= 3) {
    r2 = shrd.r1 - shrd.b1 + shrd.c1;
    if (!is_less_or_equal(shrd.a[shrd.r1 - 1], shrd.a[r2])) {
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
ss_trinkle (SmoothsortSharedVariablesX shrd)
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
          if (!is_less_or_equal(shrd.a[shrd.r1 - 1], shrd.a[r2])) {
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
ss_semitrinkle (SmoothsortSharedVariablesX shrd)
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
