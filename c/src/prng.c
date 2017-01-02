/**
 * @file
 *
 * @brief PRNG: Pseudo Random Number Generator module implementation.
 * @details This module provides random utilities and an underline pseudo
 * random number generator.
 *
 * Description of the functions ....
 *
 * Description of the usage ...
 *
 * The MT19937 generator of Makoto Matsumoto and Takuji Nishimura is a variant
 * of the twisted generalized feedback shift-register algorithm, and is known as
 * the “Mersenne Twister” generator. It has a Mersenne prime period of 2^19937 - 1 (about 10^6000)
 * and is equi-distributed in 623 dimensions. It has passed the DIE-HARD statistical tests.
 *
 * For more information see,
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
#include <stdint.h>

#define NN 312
#define MM 156



static const uint64_t um = 0xFFFFFFFF80000000ULL; /* Most significant 33 bits */
static const uint64_t lm = 0x000000007FFFFFFFULL; /* Least significant 31 bits */

static const uint64_t mag01[2] = { 0ULL, 0xB5026F5AA96619E9ULL };


/* The array for the state vector */
static uint64_t mt[NN];

/* mti == NN + 1 means mt[NN] is not initialized */
static int mti = NN + 1;

/* initializes mt[NN] with a seed */
void
prng_mt19937_init_by_seed (uint64_t seed)
{
  mt[0] = seed;
  for (mti = 1; mti < NN; mti++)
    mt[mti] =  (6364136223846793005ULL * (mt[mti - 1] ^ (mt[mti - 1] >> 62)) + mti);
}

/*
 * initialize by an array with array-length
 * init_key is the array for initializing keys
 * key_length is its length
*/
void
prng_mt19937_init_by_array (uint64_t init_key[],
                            uint64_t key_length)
{
  uint64_t i, j, k;
  prng_mt19937_init_by_seed(19650218ULL);
  i=1; j=0;
  k = (NN > key_length ? NN : key_length);
  for (; k; k--) {
    mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 62)) * 3935559000370003845ULL)) + init_key[j] + j; /* non linear */
    i++; j++;
    if (i >= NN) { mt[0] = mt[NN - 1]; i = 1; }
    if (j >= key_length) j = 0;
  }
  for (k = NN - 1; k; k--) {
    mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 62)) * 2862933555777941757ULL)) - i; /* non linear */
    i++;
    if (i >= NN) { mt[0] = mt[NN - 1]; i = 1; }
  }

  mt[0] = 1ULL << 63; /* MSB is 1; assuring non-zero initial array */
}

/* generates a random number on [0, 2^64-1]-interval */
uint64_t
prng_mt19937_get_uint64 (void)
{
  int i;
  uint64_t x;

  if (mti >= NN) { /* generate NN words at one time */

    /* if init_genrand64() has not been called, a default initial seed is used */
    if (mti == NN + 1) prng_mt19937_init_by_seed(5489ULL);

    for (i = 0; i < NN - MM; i++) {
      x = (mt[i] & um) | (mt[i + 1] & lm);
      mt[i] = mt[i + MM] ^ (x >> 1) ^ mag01[(int) (x & 1ULL)];
    }
    for ( ; i < NN - 1; i++) {
      x = (mt[i] & um) | (mt[i + 1] & lm);
      mt[i] = mt[i + (MM -NN)] ^ (x >> 1) ^ mag01[(int) (x & 1ULL)];
    }
    x = (mt[NN - 1] & um) | (mt[0] & lm);
    mt[NN - 1] = mt[MM - 1] ^ (x >> 1) ^ mag01[(int) (x & 1ULL)];

    mti = 0;
  }

  x = mt[mti++];

  x ^= (x >> 29) & 0x5555555555555555ULL;
  x ^= (x << 17) & 0x71D67FFFEDA60000ULL;
  x ^= (x << 37) & 0xFFF7EEE000000000ULL;
  x ^= (x >> 43);

  return x;
}

/* generates a random number on [0, 1]-real-interval */
double
prng_mt19937_get_double_in_c0_c1 (void)
{
  return (prng_mt19937_get_uint64() >> 11) * (1.0 / 9007199254740991.0);
}

/* generates a random number on [0, 1)-real-interval */
double
prng_mt19937_get_double_in_c0_o1 (void)
{
  return (prng_mt19937_get_uint64() >> 11) * (1.0 / 9007199254740992.0);
}

/* generates a random number on (0, 1)-real-interval */
double
prng_mt19937_get_double_in_o0_o1 (void)
{
  return ((prng_mt19937_get_uint64() >> 12) + 0.5) * (1.0 / 4503599627370496.0);
}


int main (void)
{
  int i;
  uint64_t init[4] = { 0x12345ULL, 0x23456ULL, 0x34567ULL, 0x45678ULL }, length = 4;
  prng_mt19937_init_by_array(init, length);
  printf("1000 outputs of genrand64_int64()\n");
  for (i = 0; i < 1000; i++) {
    printf("%20zu ", prng_mt19937_get_uint64());
    if (i % 5 == 4) printf("\n");
  }
  printf("\n1000 outputs of genrand64_real2()\n");
  for (i = 0; i < 1000; i++) {
    printf("%10.8f ", prng_mt19937_get_double_in_c0_o1());
    if (i % 5 == 4) printf("\n");
  }
  return 0;
}
