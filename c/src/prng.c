/**
 * @file
 *
 * @brief PRNG: Pseudo Random Number Generator module implementation.
 * @details This module provides random utilities and an underline pseudo
 * random number generator.
 *
 *
 * There are two groups of functions and a few spare utilities. The first, very simple is just
 * a wrapper for the C standard library. It collects:
 *
 *  - #prng_stdlib_init_seed()
 *  - #prng_stdlib_init_seed_with_value()
 *  - #prng_stdlib_get_number_in_range()
 *  - #prng_stdlib_shuffle_array_uint8()
 *
 * The second group is build around the Mersenne Twister Pseudo Random Number Generator and the
 * original implementation by Makoto Matsumoto and Takuji Nishimura.
 * Belonging to it there are the following functions:
 *
 *  - #prng_mt19937_new()
 *  - #prng_mt19937_free()
 *  - #prng_mt19937_init_by_seed()
 *  - #prng_mt19937_init_by_array()
 *  - #prng_mt19937_get_uint64()
 *  - #prng_mt19937_get_double_in_c0_c1()
 *  - #prng_mt19937_get_double_in_c0_o1()
 *  - #prng_mt19937_get_double_in_o0_o1()
 *  - #prng_mt19937_random_choice_from_finite_set()
 *  - #prng_mt19937_shuffle_array_uint8()
 *  - #prng_mt19937_shuffle_array_p()
 *  - #prng_mt19937_shuffle_array_double()
 *  - #prng_mt19937_shuffle_array_int()
 *  - #prng_mt19937_random_string_az()
 *
 * A useful function that the module provide is #prng_uint64_from_clock_random_seed(). It generates useful "random"
 * seed from the system clock.
 *
 * A common usage scenario is to generate the PRNG status, contained into an object of type #prng_mt19937_t, calling
 * the constructor function #prng_mt19937_new(). Then initialize it by means of either #prng_mt19937_init_by_seed(),
 * or #prng_mt19937_init_by_array(), and after having used it, release the memory calling #prng_mt19937_free().
 * The main function used to generate new elements of the pseudo-random sequence is #prng_mt19937_get_uint64(), it
 * is called by all the other methods.
 *
 * The MT19937 generator of Makoto Matsumoto and Takuji Nishimura is a variant
 * of the twisted generalized feedback shift-register algorithm, and is known as
 * the “Mersenne Twister” generator. It has a Mersenne prime period of 2^19937 - 1 (about 10^6000)
 * and is equi-distributed in 623 dimensions. It has passed the DIE-HARD statistical tests.
 *
 * For more information see:
 *
 * <em>
 * Makoto Matsumoto and Takuji Nishimura, “Mersenne Twister: A 623-dimensionally equidistributed
 * uniform pseudorandom number generator”.
 * ACM Transactions on Modeling and Computer Simulation, Vol. 8, No. 1 (Jan. 1998), Pages 3–30
 * </em>
 *
 * Useful information is also available at the
 * Wikipedia page: <a href="https://en.wikipedia.org/wiki/Mersenne_Twister" target="_blank">
 * Mersenne-Twister</a>.
 *
 * The source code for some functions implemented in this file is derived from the original work written by
 * Takuji Nishimura and Makoto Matsumoto. This original work can be found at the web page:
 * <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/mt19937-64.c" target="_blank">
 * mt19937-64.c</a>. Here is also reported verbatim an extract of the original header notice:
 *
 * <DIV style="margin-left: 40px"><kbd>
 *
 * A C-program for MT19937-64 (2004/9/29 version).
 * Coded by Takuji Nishimura and Makoto Matsumoto.
 *
 * Copyright (C) 2004, Makoto Matsumoto and Takuji Nishimura,
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. The names of its contributors may not be used to endorse or promote
 *      products derived from this software without specific prior written
 *      permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,<br>
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.<br>
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,<br>
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;<br>
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,<br>
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,<br>
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.<br>
 *
 * \n
 *  References:
 *  T. Nishimura, ``Tables of 64-bit Mersenne Twisters''
 *    ACM Transactions on Modeling and Computer Simulation 10. (2000) 348--357.
 *  M. Matsumoto and T. Nishimura,
 *    ``Mersenne Twister: a 623-dimensionally equidistributed uniform pseudorandom number generator''
 *    ACM Transactions on Modeling and Computer Simulation 8. (Jan. 1998) 3--30.
 *
 * \n
 * Any feedback is very welcome.<br>
 * http://www.math.hiroshima-u.ac.jp/~m-mat/MT/emt.html<br>
 * email: m-mat@math.sci.hiroshima-u.ac.jp<br>
 *
 * </kbd></DIV>
 *
 * @par prng.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Makoto Matsumoto mailto:m-mat@math.sci.hiroshima-u.ac.jp
 * @author Takuji Nishimura mailto:mat@math.sci.hiroshima-u.ac.jp
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2004 Makoto Matsumoto and Takuji Nishimura. All rights reserved.
 * @copyright 2017 Roberto Corradini. All rights reserved.
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
#include <math.h>
#include <assert.h>

#include "prng.h"
#include "time_utils.h"



static const size_t nn = 312;
static const size_t mm = 156;
static const uint64_t um = 0xFFFFFFFF80000000ULL; /* Most significant 33 bits */
static const uint64_t lm = 0x000000007FFFFFFFULL; /* Least significant 31 bits */
static const uint64_t mag01[2] = { 0ULL, 0xB5026F5AA96619E9ULL };




/**
 * @brief Initializes the seed used by the random functions.
 *
 * @details Set a new seed taken in a not repeatable way from the `time()` function.
 */
void
prng_stdlib_init_seed (void)
{
  srand(time(NULL));
}

/**
 * @brief Initializes the seed used by the random functions with the given value.
 *
 * @param seed the value used to initialize the random generator
 */
void
prng_stdlib_init_seed_with_value (const unsigned int seed)
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
 * Calling the function #prng_stdlib_init_seed set a new seed taken in a not
 * repeatable way from the `time()` function.
 *
 * @param [in] low  the lower bound
 * @param [in] high the upper bound
 * @return          a random value in the range U[low..high]
 */
int
prng_stdlib_get_number_in_range (const int low,
                                 const int high)
{
  assert (low <= high);
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
prng_stdlib_shuffle_array_uint8 (uint8_t *const array,
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
 * @brief Generates a seed, used by the random functions, obtained from the internal clock.
 *
 * @return an almost unexpected unsigned integer
 */
unsigned long int
prng_uint64_from_clock_random_seed (void)
{
  timespec_t t;
  clock_gettime(CLOCK_REALTIME, &t);
  return (unsigned long int) timespec_get_nsec(&t);
}

/**
 * @brief prng_mt19937_t structure constructor.
 *
 * @return a pointer to a new mt19937 pseudo-random number generator structure
 */
prng_mt19937_t *
prng_mt19937_new (void)
{
  prng_mt19937_t *st = (prng_mt19937_t *) malloc(sizeof(prng_mt19937_t));
  assert(st);

  st->mt = (uint64_t *) malloc(nn * sizeof(uint64_t));
  assert(st->mt);

  /* mti == nn + 1 means that the mt array is not initialized. */
  st->mti = nn + 1;

  return st;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #prng_mt19937_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] st the pointer to be deallocated
 */
void
prng_mt19937_free (prng_mt19937_t *st)
{
  if (st) {
    free(st->mt);
    free(st);
  }
}

/**
 * @brief Initializes the `st` structure with a seed.
 *
 * @details The status of the MT-19937 PRNG, previously allocated by a call
 * to #prng_mt19937_new, is reset.
 *
 * @invariant Parameter `st` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [out] st   the MT-19937 prng status
 * @param [in]  seed the seed number
 */
void
prng_mt19937_init_by_seed (prng_mt19937_t *st,
                           const uint64_t seed)
{
  assert(st);
  assert(st->mt);

  st->mt[0] = seed;
  for (st->mti = 1; st->mti < nn; st->mti++)
    st->mt[st->mti] = (6364136223846793005ULL * (st->mt[st->mti - 1] ^ (st->mt[st->mti - 1] >> 62)) + st->mti);
}

/**
 * @brief Initializes the `st` structure with an array.
 *
 * @details The status of the MT-19937 PRNG, previously allocated by a call
 * to #prng_mt19937_new, is reset.
 *
 * @invariant Parameter `st` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `init_key` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [out] st         the MT-19937 prng status
 * @param [in]  init_key   the array of values used to initialize the prng
 * @param [in]  key_length the number of elements in the array
 */
void
prng_mt19937_init_by_array (prng_mt19937_t *st,
                            const uint64_t init_key[],
                            const size_t key_length)
{
  assert(st);
  assert(st->mt);
  assert(init_key);

  uint64_t i = 1;
  uint64_t j = 0;
  uint64_t k = (nn > key_length ? nn : key_length);

  prng_mt19937_init_by_seed(st, 19650218ULL);

  for (; k; k--) {
    st->mt[i] = (st->mt[i] ^ ((st->mt[i - 1] ^ (st->mt[i - 1] >> 62)) * 3935559000370003845ULL)) + init_key[j] + j; /* non linear */
    i++; j++;
    if (i >= nn) { st->mt[0] = st->mt[nn - 1]; i = 1; }
    if (j >= key_length) j = 0;
  }

  for (k = nn - 1; k; k--) {
    st->mt[i] = (st->mt[i] ^ ((st->mt[i - 1] ^ (st->mt[i - 1] >> 62)) * 2862933555777941757ULL)) - i; /* non linear */
    i++;
    if (i >= nn) { st->mt[0] = st->mt[nn - 1]; i = 1; }
  }

  st->mt[0] = 1ULL << 63; /* MSB is 1; assuring non-zero initial array */
}

/**
 * @brief Generates a random number on `[0, 2^64 - 1]-interval`.
 *
 * @details The status of the MT-19937 PRNG is altered; a new
 * pseudo-random number is then returned.
 *
 * @invariant Parameter `st` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] st the MT-19937 prng status
 * @return            next pseudo-random number from the sequence
 */
uint64_t
prng_mt19937_get_uint64 (prng_mt19937_t *st)
{
  assert(st);
  assert(st->mt);

  int i;
  uint64_t x;

  if (st->mti >= nn) { /* generate nn words at one time */

    /* If prng_mt19937_init_by_seed has not been called, a default initial seed is used. */
    if (st->mti == nn + 1) prng_mt19937_init_by_seed(st, 5489ULL);

    for (i = 0; i < nn - mm; i++) {
      x = (st->mt[i] & um) | (st->mt[i + 1] & lm);
      st->mt[i] = st->mt[i + mm] ^ (x >> 1) ^ mag01[(int) (x & 1ULL)];
    }

    for ( ; i < nn - 1; i++) {
      x = (st->mt[i] & um) | (st->mt[i + 1] & lm);
      st->mt[i] = st->mt[i + (mm - nn)] ^ (x >> 1) ^ mag01[(int) (x & 1ULL)];
    }

    x = (st->mt[nn - 1] & um) | (st->mt[0] & lm);
    st->mt[nn - 1] = st->mt[mm - 1] ^ (x >> 1) ^ mag01[(int) (x & 1ULL)];

    st->mti = 0;
  }

  x = st->mt[st->mti++];

  x ^= (x >> 29) & 0x5555555555555555ULL;
  x ^= (x << 17) & 0x71D67FFFEDA60000ULL;
  x ^= (x << 37) & 0xFFF7EEE000000000ULL;
  x ^= (x >> 43);

  return x;
}

/**
 * @brief Generates a random number on `[0, 1]-real-interval`.
 *
 * @details The status of the MT-19937 PRNG is altered; a new
 * pseudo-random number is then returned.
 *
 * @invariant Parameter `st` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] st the MT-19937 prng status
 * @return            next pseudo-random number from the sequence
 */
double
prng_mt19937_get_double_in_c0_c1 (prng_mt19937_t *st)
{
  return (prng_mt19937_get_uint64(st) >> 11) * (1.0 / 9007199254740991.0);
}

/**
 * @brief Generates a random number on `[0, 1)-real-interval`.
 *
 * @details The status of the MT-19937 PRNG is altered; a new
 * pseudo-random number is then returned.
 *
 * @invariant Parameter `st` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] st the MT-19937 prng status
 * @return            next pseudo-random number from the sequence
 */
double
prng_mt19937_get_double_in_c0_o1 (prng_mt19937_t *st)
{
  return (prng_mt19937_get_uint64(st) >> 11) * (1.0 / 9007199254740992.0);
}

/**
 * @brief Generates a random number on `(0, 1)-real-interval`.
 *
 * @details The status of the MT-19937 PRNG is altered; a new
 * pseudo-random number is then returned.
 *
 * @invariant Parameter `st` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] st the MT-19937 prng status
 * @return            next pseudo-random number from the sequence
 */
double
prng_mt19937_get_double_in_o0_o1 (prng_mt19937_t *st)
{
  return ((prng_mt19937_get_uint64(st) >> 12) + 0.5) * (1.0 / 4503599627370496.0);
}

/**
 * @brief Returns a random integer uniformly distributed between `0` and `k - 1` included.
 *
 * @details It get a random value from U[0..k). The function is built
 * on a #prng_mt19937_t Marsenne-Twister pseudo-random number generator.
 *
 * When the parameter `k` is equal to `1` the function returns `0` without consuming a
 * value from the prng sequence, otherwise the prng status is altered.
 *
 * The argument `prng` must be obtained calling #prng_mt19937_new, and then it must be initialized once.
 *
 * @invariant Parameter `prng` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `k` cannot be `0`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] prng the pseudo-random number generator to use
 * @param [in] k        the size of the set to select from
 * @return              an integer in the range `[0..k)`
 */
uint64_t
prng_mt19937_random_choice_from_finite_set (prng_mt19937_t *prng,
                                            const uint64_t k)
{
  assert(prng);
  assert(prng->mt);
  assert(k != 0);
  if (k == 1) return 0;
  const double r = prng_mt19937_get_double_in_c0_o1(prng);
  const uint64_t c = (uint64_t) floor(r * k);
  return c;
}

/**
 * @brief Shuffles the given array of `uint8_t` elements.
 *
 * @details Arrange in place the `n` elements of `array` in random order.
 *
 * See:
 * - the Wikipedia page: <a href="http://en.wikipedia.org/wiki/Knuth_shuffle" target="_blank">
 *   Fisher–Yates shuffle</a>.
 * - Knuth (1998). Seminumerical algorithms. The Art of Computer Programming 2 (3rd ed.). Boston: Addison–Wesley. pp. 145–146.
 *
 * When `n` is equal to `0` or to `1 no random number is consumed from the prng sequence.
 *
 * The argument `prng` must be obtained calling #prng_mt19937_new, and then it must be initialized once.
 *
 * @invariant Parameter `prng` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `array` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] prng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
prng_mt19937_shuffle_array_uint8 (prng_mt19937_t *prng,
                                  uint8_t *const array,
                                  const size_t n)
{
  assert(prng);
  assert(array);
  if (n > 1) {
    for (size_t i = n - 1; i > 0; i--) {
      size_t j = prng_mt19937_random_choice_from_finite_set(prng, i + 1);
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
 * When `n` is equal to `0` or to `1 no random number is consumed from the prng sequence.
 *
 * The argument `prng` must be obtained calling #prng_mt19937_new, and then it must be initialized once.
 *
 * @invariant Parameter `prng` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `array` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] prng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
prng_mt19937_shuffle_array_p (prng_mt19937_t *prng,
                              void **array,
                              const size_t n)
{
  assert(prng);
  assert(array);
  if (n > 1) {
    for (size_t i = n - 1; i > 0; i--) {
      size_t j = prng_mt19937_random_choice_from_finite_set(prng, i + 1);
      void *const t = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = t;
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
 * When `n` is equal to `0` or to `1 no random number is consumed from the prng sequence.
 *
 * The argument `prng` must be obtained calling #prng_mt19937_new, and then it must be initialized once.
 *
 * @invariant Parameter `prng` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `array` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] prng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
prng_mt19937_shuffle_array_double (prng_mt19937_t *prng,
                                   double *const array,
                                   const size_t n)
{
  assert(prng);
  assert(array);
  if (n > 1) {
    for (size_t i = n - 1; i > 0; i--) {
      size_t j = prng_mt19937_random_choice_from_finite_set(prng, i + 1);
      const double t = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = t;
    }
  }
}

/**
 * @brief Shuffles the given array of integers.
 *
 * @details Arrange in place the `n` elements of `array` in random order.
 *
 * See:
 * - the Wikipedia page: <a href="http://en.wikipedia.org/wiki/Knuth_shuffle" target="_blank">
 *   Fisher–Yates shuffle</a>.
 * - Knuth (1998). Seminumerical algorithms. The Art of Computer Programming 2 (3rd ed.). Boston: Addison–Wesley. pp. 145–146.
 *
 * When `n` is equal to `0` or to `1 no random number is consumed from the prng sequence.
 *
 * The argument `prng` must be obtained calling #prng_mt19937_new, and then it must be initialized once.
 *
 * @invariant Parameter `prng` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `array` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] prng the random number generator to use
 * @param [in,out] array the array to be shuffled
 * @param [in]     n     the number of elements in the array
 */
void
prng_mt19937_shuffle_array_int (prng_mt19937_t *prng,
                                int *const array,
                                const size_t n)
{
  assert(prng);
  assert(array);
  if (n > 1) {
    for (size_t i = n - 1; i > 0; i--) {
      size_t j = prng_mt19937_random_choice_from_finite_set(prng, i + 1);
      const int t = *(array + j);
      *(array + j) = *(array + i);
      *(array + i) = t;
    }
  }
}

/**
 * @brief Prepares a random string.
 *
 * @details `rs` receives a sequence of `n` random characters in the range `[a..z]`.
 *
 * The argument `prng` must be obtained calling #prng_mt19937_new, and then it must be initialized once.
 * Buffer `rs` must be large enought to receive the characters and the termination, so it must be
 * `n + 1` long or more.
 *
 * @invariant Parameter `prng` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `rs` must not be null.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] prng the random number generator to use
 * @param [out]    rs   the generated random string
 * @param [in]     n    the number of characters n the string
 */
void
prng_mt19937_random_string_az (prng_mt19937_t *prng,
                               char *const rs,
                               const size_t n)
{
  assert(prng);
  assert(rs);

  const char a = 'a';
  const char z = 'z';
  const unsigned int range = z - a;

  for (size_t i = 0; i < n; i++) {
    const unsigned int shift = prng_mt19937_random_choice_from_finite_set(prng, range);
    const char c = a + shift;
    rs[i] = c;
  }
  rs[n] = '\0';
}
