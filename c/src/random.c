/**
 * @file
 *
 * @brief Random module implementation.
 *
 * @par random.c
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
#include <time.h>

#include <glib.h>

#include <gsl/gsl_rng.h>

#include "random.h"



/**
 * @brief Initializes the seed used by the random functions.
 *
 * @details Set a new seed taken in a not repeatable way from the `time()` function.
 */
void
random_init_seed (void)
{
  srand(time(NULL));
}

/**
 * @brief Initializes the seed used by the random functions with the given value.
 *
 * @param seed te value used to initialize the random generator
 */
void
random_init_seed_with_value (const unsigned int seed)
{
  srand(seed);
}

/**
 * @brief Returns a random integer selected in the window between
 * the `low` and `high` function parameters, having the two boundaries
 * in the valid range for the returned value.
 *
 * @invariant Parameter `high` cannot be lower then parameter `low`.
 * The invariant is guarded by an assertion.
 *
 * @details When `low == high` the function returns the `low` value
 * without consuming a position from the random sequence.
 *
 * The implementations is build upon the standard `random()` function,
 * see UNIX/Linux manpages for help.
 *
 * Calling the function #random_init_seed set a new seed taken in a not
 * repeatable way from the `time()` function.
 *
 * @param [in] low  the lower bound
 * @param [in] high the upper bound
 * @return          a random value in the range U[low..high]
 */
int
random_get_number_in_range (const int low,
                            const int high)
{
  g_assert (low <= high);
  if (low == high) return low;
  int upper_range_boundary = high - low + 1;
  int random = (int) ((double) upper_range_boundary * (rand() / (RAND_MAX + 1.0)));
  return random + low;
  /*
   * Call example:
   * args: low=3, high=10
   * upper_range_boundary: 8
   * rand: U[0..7]
   * return: U[3..10]
   */
}

/**
 * @brief Shuffles the given array.
 *
 * @details Arrange the `n` elements of `array` in random order.
 * Only effective if `n` is much smaller than `RAND_MAX`.
 *
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
random_shuffle_array_uint8 (uint8_t *array,
                            const int n)
{
  if (n > 1) {
    for (int i = 0; i < n - 1; i++) {
      int j = i + (double) rand() * (n - i) / RAND_MAX;
      const uint8_t t = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = t;
    }
  }
}

/**
 * @brief `RandomNumberGenerator` structure constructor.
 *
 * @details An assertion checks that the received pointer to the allocated
 * rng (random number generator) structure is not `NULL`.
 *
 * Returns a new rng used by functions such as #rng_random_choice_from_finite_set.
 * When the structure is no more used it must be freed bya call to #rng_free.
 *
 * @param [in] seed the seed for the random number generator
 * @return          a pointer to a new random number generator structure
 */
RandomNumberGenerator *
rng_new (const unsigned long int seed)
{
  RandomNumberGenerator *rng;
  static const size_t size_of_rng = sizeof(RandomNumberGenerator);

  rng = (RandomNumberGenerator *) malloc(size_of_rng);
  g_assert(rng);

  rng->r = gsl_rng_alloc(gsl_rng_mt19937);
  g_assert(rng->r);

  rng->seed = seed;
  gsl_rng_set(rng->r, rng->seed);

  return rng;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #rng_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] rng the pointer to be deallocated
 */
void
rng_free (RandomNumberGenerator *rng)
{
  if (rng) {
    gsl_rng_free(rng->r);
    free(rng);
  }
}

/**
 * @brief Initializes seed used by the random functions.
 *
 * @return
 */
unsigned long int
rng_random_seed (void)
{
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  return (unsigned long int) t.tv_nsec;
}

/**
 * @brief Returns a random integer uniformly distributed between `0` and `k - 1` included.
 *
 * @details It get a random value from U[0..k). The function is built
 * on the GNU GSL library, applying the `gsl_rng_mt19937` function.
 *
 * When the parameter `k` is equal to `1` the function returns `0` without consuming a
 * value from the rng sequence.
 *
 * The argument `rng` must be obtained calling #rng_new.
 *
 * Taking an extract from the GSL documentation:
 *
 * The MT19937 generator of Makoto Matsumoto and Takuji Nishimura is a variant
 * of the twisted generalized feedback shift-register algorithm, and is known as
 * the “Mersenne Twister” generator. It has a Mersenne prime period of 2^19937 - 1 (about 10^6000)
 * and is equi-distributed in 623 dimensions. It has passed the DIEHARD statistical tests.
 *
 * For more information see,
 *
 * <em>
 * Makoto Matsumoto and Takuji Nishimura, “Mersenne Twister: A 623-dimensionally equidistributed
 * uniform pseudorandom number generator”.
 * ACM Transactions on Modeling and Computer Simulation, Vol. 8, No. 1 (Jan. 1998), Pages 3–30
 * </em>
 *
 * @invariant Parameter `k` cannot be `0`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] rng the random number generator to use
 * @param [in] k       the size of the set to select from
 * @return             an integer in the range [0..k)
 */
unsigned long int
rng_random_choice_from_finite_set (RandomNumberGenerator *rng,
                                   const unsigned long int k)
{
  g_assert(k != 0);
  if (k == 1) return 0;
  return gsl_rng_uniform_int(rng->r, k);
}

/**
 * @brief Shuffles the given array.
 *
 * @details Arrange in place the `n` elements of `array` in random order.
 *
 * See:
 * - the Wikipedia page: <a href="http://en.wikipedia.org/wiki/Knuth_shuffle" target="_blank">
 *   Fisher–Yates shuffle</a>.
 * - Knuth (1998). Seminumerical algorithms. The Art of Computer Programming 2 (3rd ed.). Boston: Addison–Wesley. pp. 145–146.
 *
 * When `n` is equal to `0` or to `1 no random number is consumed from the rng sequence.
 *
 * The argument `rng` must be obtained calling #rng_new.
 *
 * @invariant Parameter `n` cannot be negative.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] rng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
rng_shuffle_array_uint8 (RandomNumberGenerator *rng,
                         uint8_t *array,
                         const int n)
{
  g_assert(n >= 0);
  if (n > 1) {
    for (int i = n - 1; i > 0; i--) {
      int j = rng_random_choice_from_finite_set(rng, i + 1);
      const uint8_t t = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = t;
    }
  }
}

/**
 * @brief Shuffles the given array of pointers.
 *
 * @details Arrange in place the `n` elements of `array` in random order.
 *
 * See:
 * - the Wikipedia page: <a href="http://en.wikipedia.org/wiki/Knuth_shuffle" target="_blank">
 *   Fisher–Yates shuffle</a>.
 * - Knuth (1998). Seminumerical algorithms. The Art of Computer Programming 2 (3rd ed.). Boston: Addison–Wesley. pp. 145–146.
 *
 * When `n` is equal to `0` or to `1 no random number is consumed from the rng sequence.
 *
 * The argument `rng` must be obtained calling #rng_new.
 *
 * @invariant Parameter `n` cannot be negative.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] rng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
rng_shuffle_array_p (RandomNumberGenerator *const rng,
                     void **const array,
                     const int n)
{
  g_assert(n >= 0);
  if (n > 1) {
    for (int i = n - 1; i > 0; i--) {
      int j = rng_random_choice_from_finite_set(rng, i + 1);
      void *const element = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = element;
    }
  }
}

/**
 * @brief Shuffles the given array of doubles.
 *
 * @details Arrange in place the `n` elements of `array` in random order.
 *
 * See:
 * - the Wikipedia page: <a href="http://en.wikipedia.org/wiki/Knuth_shuffle" target="_blank">
 *   Fisher–Yates shuffle</a>.
 * - Knuth (1998). Seminumerical algorithms. The Art of Computer Programming 2 (3rd ed.). Boston: Addison–Wesley. pp. 145–146.
 *
 * When `n` is equal to `0` or to `1 no random number is consumed from the rng sequence.
 *
 * The argument `rng` must be obtained calling #rng_new.
 *
 * @invariant Parameter `n` cannot be negative.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] rng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
rng_shuffle_array_double (RandomNumberGenerator *const rng,
                          double *const array,
                          const int n)
{
  g_assert(n >= 0);
  if (n > 1) {
    for (int i = n - 1; i > 0; i--) {
      int j = rng_random_choice_from_finite_set(rng, i + 1);
      const double element = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = element;
    }
  }
}
