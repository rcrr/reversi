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
 * @brief Initializes seed used by the random functions.
 */
void
utils_init_random_seed (void)
{
  srand(time(NULL));
}

/**
 * @brief Returns a random integer selected in the window between
 * the `low` and `high` function parameters, having the two boundaries
 * in the valid range for the returned value.
 *
 * @param [in] low  the lower bound
 * @param [in] high the upper bound
 * @return          a random value in the given range
 */
int
utils_random_number (int low, int high)
{
  g_assert (low <= high);
  if (low == high) return low;
  int upper_range_boundary = high - low + 1;
  int random = (int) ((double) upper_range_boundary * (rand() / (RAND_MAX + 1.0)));
  return random + low;
  /*
   * args: 3, 10
   * upper_range_boundary: 8
   * rand: [0..7]
   * return: [3..10]
   */
  //return low + (double) rand() * (high - low + 1) / RAND_MAX;
}

/**
 * @brief Shuffles the given array.
 * 
 * @details Arrange the N elements of ARRAY in random order.
 * Only effective if N is much smaller than RAND_MAX.
 *
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
utils_shuffle_uint8 (uint8_t *array, int n)
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
 * @brief RandomNumberGenerator structure destructor.
 *
 * @invariant Parameter `rng` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] rng the pointer to be deallocated
 * @return         always the NULL pointer
 */
RandomNumberGenerator *
rng_free (RandomNumberGenerator *rng)
{
  g_assert(rng);

  gsl_rng_free(rng->r);
  free(rng);
  rng = NULL;

  return rng;
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
 * @param [in] rng the random number generator to use
 * @param [in] k   the size of the set to select from
 * @return         an integer in the range [0..k)
 */
unsigned long int
rng_random_choice_from_finite_set (RandomNumberGenerator *rng,
                                   const unsigned long int k)
{
  g_assert(k != 0);
  if (k == 1) return 0;
  return gsl_rng_uniform_int(rng->r, k);
}
