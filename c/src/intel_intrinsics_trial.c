/**
 * @file
 *
 * @brief Experiments with intel intrinsics.
 * @details To be detailed.
 *
 * @par intel_intrinsics_trial.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2016 Roberto Corradini. All rights reserved.
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/*
  https://en.wikipedia.org/wiki/CPUID
  https://github.com/gcc-mirror/gcc/blob/master/gcc/config/i386/cpuid.h
*/

/*
  Launch this command to get the list of compile time symbol.

  $ gcc -msse3 -dM -E - < /dev/null | egrep "SSE|AVX" | sort

  On my Haswell i7 I get this.

  #define __AVX__ 1
  #define __AVX2__ 1
  #define __SSE__ 1
  #define __SSE2__ 1
  #define __SSE2_MATH__ 1
  #define __SSE3__ 1
  #define __SSE4_1__ 1
  #define __SSE4_2__ 1
  #define __SSE_MATH__ 1
  #define __SSSE3__ 1
*/

/*
  <mmintrin.h>  MMX
  <xmmintrin.h> SSE
  <emmintrin.h> SSE2
  <pmmintrin.h> SSE3
  <tmmintrin.h> SSSE3
  <smmintrin.h> SSE4.1
  <nmmintrin.h> SSE4.2
  <ammintrin.h> SSE4A
  <wmmintrin.h> AES
  <immintrin.h> AVX
  <zmmintrin.h> AVX512
*/

#include <immintrin.h>

#include <cpuid.h>

/**
 * @brief Main entry for the Intel intrinsics trial program.
 */
int
main (int argc, char *argv[])
{
  printf("Intel intrinsics trial program.\n");

  __m128i x;
  uint64_t x0, x1;

#ifdef __AVX2__
  printf("AVX2 is available\n");
#endif

#ifdef __SSE4_2__
  printf("SSE4_2 is available\n");
#endif

#ifdef __SSE4_1__
  printf("SSE4_1 is available\n");
#endif

  int ret;
  unsigned int level;
  unsigned int eax, ebx, ecx, edx;

  // level = 0x80000006;
  level = 0;
  ret = __get_cpuid(level, &eax, &ebx, &ecx, &edx);
  printf("__get_cpuid for level=%08X, ret=%d, eax=%08X, ebx=%08X, ecx=%08X, edx=%08X\n", level, ret, eax, ebx, ecx, edx);

  x = _mm_setzero_si128();

  x0 = _mm_extract_epi64(x, 0);
  x1 = _mm_extract_epi64(x, 1);

  printf("x0 = %zu\n", x0);
  printf("x1 = %zu\n", x1);

  x = _mm_setr_epi64(_mm_set_pi64x(0xFFFFFFFFFFFFFFFF), _mm_set_pi64x(0xFFFFFFFFFFFFFFFF));

  x0 = _mm_extract_epi64(x, 0);
  x1 = _mm_extract_epi64(x, 1);

  printf("x0 = %zu\n", x0);
  printf("x1 = %zu\n", x1);

  /* Expected are both equal to:  18446744073709551615 */
  assert(x0 == 18446744073709551615UL);
  assert(x1 == 18446744073709551615UL);


  /*
    Second example ...
    http://www.codeproject.com/Articles/874396/Crunching-Numbers-with-AVX-and-AVX
  */

  /* Initialize the two argument vectors */
  __m256 evens = _mm256_set_ps(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0);
  __m256 odds = _mm256_set_ps(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0);

  /* Compute the difference between the two vectors */
  __m256 result = _mm256_sub_ps(evens, odds);

  /* Display the elements of the result vector */
  float* f = (float*)&result;
  printf("%f %f %f %f %f %f %f %f\n",
         f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7]);


  return 0;
}
